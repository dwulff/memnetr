% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rwalk}
\alias{rwalk}
\title{Random walk}
\usage{
rwalk(adjlist, start, observe, nmax = 1000L, pjump = 0)
}
\arguments{
\item{adjlist}{a list containing row indices for adjacent vertices as created
by \link{get_adjlist}.}

\item{start}{index of the start vertix.}

\item{observe}{integer vector specifying the vertices whose first visits should be recorded.}

\item{nmax}{integer specifying the maximum number of steps.}

\item{pjump}{numeric specifying the probability of a jump.}
}
\value{
Numeric, 3 column matrix containing in each row the start vertex, the end vertex, and
the (minimum) number of steps it took to reach the end vertext from the start vertex.
}
\description{
Traverses a network using a switcher-random walk process and records the earliest
visit to vertices of interest.
}
\details{
Beginning at a given start vertext, function traverses a network using switcher
random walk and records for each of a list of vertices of interest the index at which
the respective vertices have been visited first.

If a vertex specified in \code{observe} has never been visited then the function
returns \code{nmax} for that vertex.
}
