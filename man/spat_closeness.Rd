% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spat_closeness.R
\name{spat_closeness}
\alias{spat_closeness}
\title{Calculate Spatial closeness}
\usage{
spat_closeness(graph, alpha, rank = TRUE)
}
\arguments{
\item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.}

\item{alpha}{specifies alpha value at which closeness is to be calculted}

\item{rank}{decides whether to return rank of nodes of raw values. Defaults to TRUE meaning ranks are to be calculated}
}
\description{
This function allows you calculate spatial closeness of nodes at a particular alpha
}
\examples{

}
\keyword{:}
\keyword{clustering}
\keyword{spatial}

