\name{SpatialSociogram}
\alias{SpatialSociogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Spatial Sociogram
%%  ~~function to do ... ~~
}
\description{This function creates a sociogram where the relative position of the nodes with respect to each other nodes are preserved
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spatial_sociogram(graph, v_color='gray', v_lab_color="black", e_width=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node}

\item{v_color}{Color of the vertices. Default=gray}

\item{v_lab_color}{Color of the vertex labels. Default=black}

\item{e_width}{Edge widths. Default=2}
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
spatial_sociogram()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sociogram}
\keyword{spatial}
\keyword{plot}
