% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_sep.R
\name{complete_mg}
\alias{complete_mg}
\title{Construct complete graph}
\usage{
complete_mg(nU, nB, ord)
}
\arguments{
\item{nU, nB}{number of nodes in each component}

\item{ord}{optional reordering of the vertices}
}
\description{
Make a complete graph with nodes having only tails in subset of size \code{nU},
and only arrow heads in remaining subset of size \code{nB}
}
\details{
The default ordering is topological, placing the vertices with only
tails first.
}
