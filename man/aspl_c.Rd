% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures.R
\name{aspl_c}
\alias{aspl_c}
\title{Corrected average shortest path length}
\usage{
aspl_c(g, directed = FALSE)
}
\arguments{
\item{g}{a graph object of class \code{igraph}.}

\item{directed}{a logical specifying whether the algorithm should
take edge weights into account \code{igraph}.}
}
\value{
Local clustering coefficient.
}
\description{
Computes the corrected average shortest path length using
\code{igraph}'s \link[igraph]{average.path.length} function.
The correction is achieved by dividing average shortest path
length by the quotient of the average degree and the number of
vertices, i.e., the expected averafe transitivity for a
Erdös-Renyi random graph.
}
