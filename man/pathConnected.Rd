% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Adjacencies.R
\name{pathConnected}
\alias{pathConnected}
\title{Find vertices in subset connected to vertex}
\usage{
pathConnected(graph, v, D, etype, dir, verbose = FALSE)
}
\arguments{
\item{graph}{an object of class \code{mixedgraph}}

\item{v}{vertex to check}

\item{D}{set to look for paths to}

\item{etype}{edge types to use}

\item{dir}{integer vector of directions}

\item{verbose}{logical: should additional information be provided?}
}
\description{
Function for finding nodes in a set connected to a vertex via paths in the
original graph
}
\details{
This function will look for paths in \code{graph} from \code{v}
that only use the edge types in \code{etype} and the directions specified,
and stop whenever a path hits something in \code{D}.  It then outputs the
subset of elements of \code{D} that it hits.

Note that for directed edges, \code{dir} defaults to 1, and so only
follows the canonical direction of the edge.  Set to 0 if all directions are
valid.
}
