\name{NearestNeigNetworkDensity}
\alias{NearestNeigNetworkDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Nearest Neighbour Network Density
%%  ~~function to do ... ~~
}
\description{ This function calculates the proportion of the k-nearest neighbors each node is connected to
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
k_fulfillment(g, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node}

  \item{k}{Nearest Neighbour}
%%     ~~Describe \code{x} here~~
}
}
\details{ This function uses the distance matrix between every node pair in the network which was created previously and stored in dist_mat2. The dist matrix was created out of the network in struct_viz.R function
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
k_fulfillment()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{density}% use one of  RShowDoc("KEYWORDS")
\keyword{network}% __ONLY ONE__ keyword per line
\keyword{neighbour}
\keyword{social}
\keyword{spatial}
