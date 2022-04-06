\name{SpatialCloseness}
\alias{SpatialCloseness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Spatial Closeness
%%  ~~function to do ... ~~
}
\description{This function allows you calculate spatial closeness of nodes at an alpha of 0.5
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spat_closeness(graph, alpha = 0.5, alphaWarning, rank=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.}

\item{alpha}{specifies alpha value at which closeness is to be calculted}

\item{alphaWarning}{Indication of default alpha}

\item{rank}{decides whether to return rank of nodes of raw values. Defaults to TRUE meaning ranks are to be calculated}
}
%%     ~~Describe \code{x} here~~
}
}
\details{
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
spat_closeness()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{clustering}
\keyword{spatial}
