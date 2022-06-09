# Source code
## Requirements
Isabelle 2021-1 + AFP2021-1, on branch website-redesign.
# Installation
1. Build graph dependencies: in [graph](graph/), run `./mvnw package`
2. Run a neo4j instance with `graph-data-science` and `apoc` installed (there is a sample docker-compose file in [graph](graph/) that you can run with `docker-compose up` from that directory
3. add afp, graph, utils components to Isabelle with `isabelle components -u <DIR>`
## Usage
Run with `isabelle afp_graph_analysis -?` and `isabelle graph_analysis -?`.
Individual experiments can be run as aspects.
