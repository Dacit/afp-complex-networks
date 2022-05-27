package isabelle.afpextra


import isabelle._

import afp.Metadata.Email
import afp._

import java.util.Properties
import javax.mail.internet.{InternetAddress, MimeMessage}
import javax.mail.{Authenticator, Message, PasswordAuthentication, Transport, Session => JSession}


object AFP_Send_Mail
{
  def sendmail(to: String, from: String, password: String, subject: String, content: String,
    smtpHost: String = "mail.in.tum.de"): Unit =
  {
    val props = new Properties()
    props.put("mail.smtp.host", smtpHost)
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")

    val session = JSession.getDefaultInstance(props, new Authenticator(){
      override protected def getPasswordAuthentication = new PasswordAuthentication(from, password)
    })

    val message = new MimeMessage(session)

    message.setFrom(new InternetAddress(from))
    message.addRecipient(Message.RecipientType.TO, new InternetAddress(to))
    message.setSubject(subject)
    message.setContent(content, "text/html")

    Transport.send(message)
  }

  def make_subject(name: String): String =
    "AFP [" + name + "]: Important Concepts and Central Theorems - Feedback needed!"

  def make_content(name: String, title: String, content: String): String =
    "Dear " + name + ",<br><br>" +
    "for your APF Entry " + quote(title) + ", I would like to know which concepts (constants, locales, classes, types) you consider to be most important in the formalization, and which theorems you found to be most central when developing the formalization.<br>" +
    "Your feedback would help in making your entry easier to grasp and better to understand. For that, I would appreciate it if you send me links to the top five important concepts and central theorems each.<br>" +
    "The following are some suggestions, but feel free to include others, too:<br>" +
    content + "<br><br>" +
    "Thanks for your participation!<br><br>" +
    "Fabian"

  def afp_send_mail(dir: Path, from: String, password: String, confirm: Boolean = false,
    progress: Progress = new Progress): Unit =
  {
    val structure = AFP_Structure()
    val authors = structure.load_authors.map(author => author.id -> author).toMap
    val topics = structure.load_topics

    def sub_topics(topic: Metadata.Topic): List[Metadata.Topic] = topic :: topic.sub_topics
      .flatMap(sub_topics)

    val topics_by_id = Utils
      .grouped_sorted(topics.flatMap(sub_topics), (t: Metadata.Topic) => t.id)
    val releases = structure.load_releases.groupBy(_.entry)
    val licenses = structure.load_licenses.map(license => license.id -> license).toMap

    val tos = for {
      elem <- File.read_dir(dir)
      file_content = File.read(dir + Path.basic(elem))
      if file_content.length > 70
      (name, ext) = Path.explode(elem).split_ext
      if ext == "html"
      entry = structure.load_entry(name.implode, authors, topics_by_id, licenses, releases)
      email <- entry.authors.collect { case e: Email => e }
    } yield {
      val author = authors(email.author)

      val to = email.address
      val subject = make_subject(name = entry.name)
      val content = make_content(name = author.name, title = entry.title, content = file_content)

      if (confirm) {
        progress.echo("Sending to " + to + ": " + subject)
        sendmail(to = to, from = from, password = password, content = content, subject = subject)
      }
      else {
        progress.echo("To: " + to)
        progress.echo("From: " + from)
        progress.echo("Subject: " + subject)
        progress.echo("Content: " + content)
      }
      to
    }
    progress.echo("Total: " + tos.distinct.size)
  }


  /* isabelle tool wrapper */

  val isabelle_tool: Isabelle_Tool =
    Isabelle_Tool("afp_send_mail", "Send AFP mails", Scala_Project.here, args =>
  {
    var confirm = false
    var test: Option[String] = None

    val getopts = Getopts("""
Usage: isabelle afp_send_mail [OPTIONS] DIR USER PASSWORD

Options are:
  -c       confirm
  -t TO    test email settings

Send email to all afp authors.
""",
    "c" -> (_ => confirm = true),
    "t:" -> (arg => test = Some(arg)))

    val (dir, from, password) = getopts(args) match {
      case dir :: from :: password :: Nil => (Path.explode(dir), from, password)
      case _ => getopts.usage()
    }

    val progress = new Console_Progress()

    test match {
      case Some(test_to) =>
        sendmail(to = test_to, from = from, password = password,
          subject = make_subject("Test_Subject"),
          content = make_content("Test Name", "Test Title", "<strong>Test Content</strong>"))
      case None =>
        afp_send_mail(dir = dir, from = from, password = password, confirm = confirm,
          progress = progress)
    }
  })
}