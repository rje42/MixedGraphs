---
output: html_document
---
# MixedGraphs

Graphs are implemented patchily in R, in various packages which each have 
their strengths and weaknesses.  `MixedGraphs` allows flexible specification
of graphs with multiple edge types, that can be expanded with custom edges,
as well as interfaces for existing packages. 
Ultimately we intend to be able to use the functionality of most existing 
graph packages with the objects in `MixedGraphs`.

Current features:

* Graphs can be quickly created 'by hand'.

* Edges can be specified as adjacency matrices, lists of adjacencies or 
  edges, or a matrix of vertex numbers.

* Built in edge-types include undirected, directed, bidirected and partially 
  directed.
  
To install the package, run the commands
```
install.packages("devtools")
devtools::install_github("rje42/MixedGraphs")
library(MixedGraphs)
```

### Conversion

The package provides functions for converting between 
representations in the various other graph packages (e.g.\ `graph`,
`igraph`, `ggm`).  This is currently partially implemented.
We also provide a pipe function `%G%` that allows the use of
code in another package, automatically converting the 
graph to the appropriate format.
