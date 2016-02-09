---
output: html_document
---
# MixedGraphs

Graphs are implemented patchily in R, in various packages which each have 
their strengths and weaknesses.  `MixedGraphs` allows flexible specification
of graphs with multiple edge types, and can be expanded with custom edges.
Ultimately we intend to be able to use the functionality of most existing 
graph packages with the objects in `mixedgraphs`.

Current features:

* Graphs can be quickly created 'by hand'.

* Edges can be specified as adjacency matrices, lists, or a matrix of vertex 
  numbers.

* Built in edge-types include undirected, directed, bidirected and partially 
  directed.
  
To install the package, run the commands
```
install.packages("devtools")
devtools::install_github("rje42/MixedGraphs")
library(MixedGraphs)
```

### Future Plans

Ultimately there will be functions for converting between the
representations in the various other graph packages (e.g.\ `graph`,
`igraph`, `ggm`).  This is currently partially implemented.
