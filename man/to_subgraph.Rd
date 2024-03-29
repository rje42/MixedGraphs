% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Construct.R
\name{to_subgraph}
\alias{to_subgraph}
\title{Map a graph onto an induced subgraph}
\usage{
to_subgraph(graph, ord, vnames)
}
\arguments{
\item{graph}{an object of class \code{mixedgraph}}

\item{ord}{an ordering to use for the existing vertices}

\item{vnames}{a character vector of variable names}
}
\description{
Map a graph onto an induced subgraph
}
\details{
Either \code{ord} or \code{vnames} must be specified, and if they are both given then
they must have the same length.  \code{vnames} must contain all the variable names
used in \code{graph}.

\code{ord} places the vertices in the order provided.  For example, if the
supplied vector is \code{c(2, 4, 1, 3)} for a graph with vertices \code{"x1"} and
\code{"x2"}, then the output graph will have the vertex order \verb{"x2", "x_4", "x_1", "x_3"}.
}
