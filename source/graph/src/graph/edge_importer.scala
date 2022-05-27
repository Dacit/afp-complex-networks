package isabelle.graph


import isabelle.Export_Theory.{Prop, Theory}
import isabelle.Pure_Thy.{DUMMY, FUN, ITSELF, PROP}
import isabelle._
import isabelle.graph.Graph.{Edge, Edge_Label, Entity_Kind, UId}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object Edge_Importer
{

  /* Dependency builder */

  class Builder(
    entity_thys: mutable.Map[(Entity_Kind.Value, String), Set[String]] = mutable.Map.empty,
    thy_imports: mutable.Map[String, List[String]] = mutable.Map.empty)
  {
    def add_thy_deps(name: String, parents: List[String]): Unit =
    {
      thy_imports.update(name, parents)
    }

    def add_entities(theory_name: String, entities: Iterator[(Entity_Kind.Value, String)]): Unit =
      entities.foreach { e =>
        entity_thys.updateWith(e) {
          case None => Some(Set(theory_name))
          case Some(theories) => Some(theories + theory_name)
        }
      }

    def build(progress: Progress = new Console_Progress()): Edge_Importer =
    {
      val import_set = mutable.Map.empty[String, Set[String]]

      def rec(theory_name: String): Set[String] =
      {
        import_set.get(theory_name) match {
          case Some(imports) => imports
          case None =>
            val all_imports = thy_imports.getOrElse(theory_name, List.empty).toSet
              .flatMap(rec) + theory_name
            import_set.update(theory_name, all_imports)
            all_imports
        }
      }

      thy_imports.keys.foreach(rec)
      new Edge_Importer(entity_thys.toMap, import_set.toMap)
    }
  }
}

class Edge_Importer(
  entity_thys: Map[(Entity_Kind.Value, String), Set[String]],
  thy_deps: Map[String, Set[String]])
{
  private def is_pure(name: String): Boolean =
    name.startsWith("Pure.") || name == DUMMY || name == FUN || name == PROP || name == ITSELF

  private def get_session(entity: (Entity_Kind.Value, String), theory_name: String): Option[String] =
  {
    if (is_pure(entity._2)) {
      None
    } else {
      entity_thys.get(entity) match {
        case None => None
        case Some(et_thys) =>
          val name =
            if (et_thys.size == 1) Some(et_thys.head)
            else {
              val imp_thys = thy_deps.getOrElse(theory_name,
                error("Imports missing for " + theory_name))
              val res = et_thys.intersect(imp_thys)
              if (res.isEmpty) None
              else if (res.size == 1) Some(res.head)
              else error("Expected single thy for entity " + entity + ". Found: " + commas_quote(res))
            }
          name.map(_.split('.').head)
      }
    }
  }

  private def get_uid(
    kind: Entity_Kind.Value,
    name: String,
    theory_name: String,
    cache: mutable.Map[(Entity_Kind.Value, String), Option[String]]): Option[UId] =
  {
    val session = cache.getOrElseUpdate((kind, name), get_session((kind, name), theory_name))
    session.map(UId.to_entity_uid(_, kind, name))
  }

  def to_edges(session_name: String, theory: Theory, skip_checks: Boolean = false): Seq[Edge] =
  {
    val cache = mutable.Map.empty[(Entity_Kind.Value, String), Option[String]]

    def local_uid(kind: Entity_Kind.Value, name: String): UId =
    {
      if (skip_checks) UId.to_entity_uid(session_name, kind, name)
      else get_uid(kind, name, theory.name, cache).getOrElse(
        error("Entity " + name + " (" + kind + ") missing for " + theory.name))
    }

    // build graph
    val thy_name = theory.name
    /* Extract edges from theory content. */
    case class Edge_Builder(from: UId)
    {
      def edge_to(other: UId, label: Edge_Label.Value): Option[Edge] =
        if (other != from) Some(Edge(from, other, label)) else None

      def sort(sort: Term.Sort): Seq[Edge] =
        sort.flatMap(get_uid(Entity_Kind.CLASS, _, thy_name, cache)).flatMap(edge_to(_, Edge_Label.DEF))

      def typ(typ: Term.Typ): Seq[Edge] = typ match {
        case Term.Type(name, args) =>
          get_uid(Entity_Kind.TYPE, name, thy_name, cache).flatMap(edge_to(_, Edge_Label.DEF)) ++:
            args.flatMap(this.typ)
        case Term.TFree(_, sort) => this.sort(sort)
        case Term.TVar(_, sort) => this.sort(sort)
      }

      def term(term: Term.Term): Seq[Edge] = term match {
        case Term.Const(name, typargs) =>
          get_uid(Entity_Kind.CONST, name, thy_name, cache).flatMap(edge_to(_, Edge_Label.DEF)) ++:
            typargs.flatMap(this.typ)
        case Term.Free(_, typ) => this.typ(typ)
        case Term.Var(_, typ) => this.typ(typ)
        case Term.Bound(_) => Seq.empty
        case Term.Abs(_, typ, body) => this.typ(typ) ++ this.term(body)
        case Term.App(fun, arg) => this.term(fun) ++ this.term(arg)
      }

      def prop(prop: Prop): Seq[Edge] =
      {
        prop.typargs.flatMap {
          case (_, sort) => this.sort(sort)
        } ++ prop.args.flatMap {
          case (_, typ) => this.typ(typ)
        } ++ this.term(prop.term)
      }
    }

    val edges = ArrayBuffer.empty[Edge]

    theory.types.foreach { typ =>
      typ.content.get.abbrev.foreach { abbrev =>
        val builder = Edge_Builder(local_uid(Entity_Kind.TYPE, typ.name))
        edges ++= builder.typ(abbrev)
      }
    }

    theory.consts.foreach { const =>
      val builder = Edge_Builder(local_uid(Entity_Kind.CONST, const.name))
      edges ++= builder.typ(const.content.get.typ)
      const.content.get.abbrev.foreach { abbrev =>
        edges ++= builder.term(abbrev)
      }
    }

    theory.axioms.foreach { axiom =>
      val builder = Edge_Builder(local_uid(Entity_Kind.AXIOM, axiom.name))
      edges ++= builder.prop(axiom.content.get.prop)
    }

    theory.thms.foreach { thm =>
      val builder = Edge_Builder(local_uid(Entity_Kind.THM, thm.name))
      edges ++= builder.prop(thm.content.get.prop)
      val deps = thm.content.get.deps.flatMap(get_uid(Entity_Kind.THM, _, thy_name, cache))
      edges ++= deps.map(Edge(local_uid(Entity_Kind.THM, thm.name), _, Edge_Label.DEP))
    }

    theory.classes.foreach { cls =>
      val builder = Edge_Builder(local_uid(Entity_Kind.CLASS, cls.name))
      cls.content.get.params.foreach {
        case (_, typ) => edges ++= builder.typ(typ)
      }
      edges ++= cls.content.get.axioms.flatMap(builder.prop)
    }

    theory.locales.foreach { locale =>
      val builder = Edge_Builder(local_uid(Entity_Kind.LOCALE, locale.name))
      locale.content.get.typargs.foreach {
        case (_, sort) => edges ++= builder.sort(sort)
      }
      locale.content.get.args.foreach {
        case ((_, typ), _) => edges ++= builder.typ(typ)
      }
      edges ++= locale.content.get.axioms.flatMap(builder.prop)
    }

    theory.locale_dependencies.foreach { dep0 =>
      val entity = dep0
      val dep = dep0.content.get
      val uid = local_uid(Entity_Kind.LOCALE_DEPENDENCY, entity.name)

      get_uid(Entity_Kind.LOCALE, dep.source, thy_name, cache).foreach { dep_source =>
        edges += Edge(uid, dep_source, Edge_Label.DEP)
      }

      get_uid(Entity_Kind.LOCALE, dep.target, thy_name, cache).foreach { dep_target =>
        edges += Edge(uid, dep_target, Edge_Label.DEP)
      }

      val builder = Edge_Builder(uid)
      dep.subst_types.foreach {
        case ((_, sort), typ) =>
          edges ++= builder.sort(sort)
          edges ++= builder.typ(typ)
      }
      dep.subst_terms.foreach {
        case ((_, typ), term) =>
          edges ++= builder.typ(typ)
          edges ++= builder.term(term)
      }
    }

    theory.classrel.foreach { rel =>
      get_uid(Entity_Kind.CLASS, rel.class1, thy_name, cache).foreach { uid1 =>
        get_uid(Entity_Kind.CLASS, rel.class2, thy_name, cache).foreach { uid2 =>
          edges += Edge(uid1, uid2, Edge_Label.REL)
        }
      }
    }

    theory.arities.foreach { arity =>
      get_uid(Entity_Kind.TYPE, arity.type_name, thy_name, cache).foreach { uid =>
        val builder = Edge_Builder(uid)
        edges ++= arity.domain.flatMap(builder.sort)
        edges ++= builder.prop(arity.prop)
      }
    }

    theory.constdefs.foreach { constdef =>
      val ax_uid = local_uid(Entity_Kind.AXIOM, constdef.axiom_name)
      get_uid(Entity_Kind.CONST, constdef.name, thy_name, cache).foreach { uid =>
        edges += Edge(ax_uid, uid, Edge_Label.DEF)
      }
    }

    theory.typedefs.foreach { typedef =>
      val ax_uid = local_uid(Entity_Kind.AXIOM, typedef.axiom_name)
      val rep_uid = local_uid(Entity_Kind.CONST, typedef.rep_name)
      val abs_uid = local_uid(Entity_Kind.CONST, typedef.abs_name)
      get_uid(Entity_Kind.TYPE, typedef.name, thy_name, cache).foreach { uid =>
        edges += Edge(ax_uid, uid, Edge_Label.DEF)
        edges += Edge(rep_uid, uid, Edge_Label.DEF)
        edges += Edge(abs_uid, uid, Edge_Label.DEF)
      }
    }

    theory.datatypes.foreach { datatype =>
      val builder = Edge_Builder(local_uid(Entity_Kind.TYPE, datatype.name))
      edges ++= builder.typ(datatype.typ)
      edges ++= datatype.typargs.map(_._2).flatMap(builder.sort)
      edges ++= datatype.constructors.flatMap {
        case (term, typ) => builder.term(term) ++ builder.typ(typ)
      }
    }

    // TODO theory.spec_rules

    edges.distinct.toVector
  }
}
