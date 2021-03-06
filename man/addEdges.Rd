% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutilate.R
\name{addEdges}
\alias{addEdges}
\alias{removeEdges}
\title{Add or remove edges}
\usage{
addEdges(graph, edges, ..., remDup = TRUE)

removeEdges(graph, edges, ..., force = FALSE)
}
\arguments{
\item{graph}{a \code{mixedgraph} object}

\item{edges}{list of edges to be added/removed}

\item{...}{edges to be added with arguments matching names of edge types}

\item{remDup}{logical: should we check for duplicate edges?}

\item{force}{should we just ignore edges not actually present?}
}
\description{
Add or remove edges
}
\details{
The \code{remDup} argument is set by default to
remove duplicate edges. Currently \code{removeEdges()} forces 
all edges to be represented by adjacency matrices.
}
\section{Functions}{
\itemize{
\item \code{removeEdges}: remove edges
}}

