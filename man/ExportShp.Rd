\name{Export Shapefile}
\alias{Export Shapefile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Export Spatial Social Network into a shapefile
%%  ~~function to do ... ~~
}
\description{Export spatial social network as a point and a line shapefiles in a folder called shp inside the working directory. The shapefiles are not projected. This function exports the nodes and edges making up the spatial social network as a point and a line shapefile respectively
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
export_shp_SpatNet(graph, projstr = CRS("+init=epsg:3395"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node}
  \item{projstr}{projection string for the input co-ordinates. Defaults to CRS("+init=epsg:3395").
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sptial}% use one of  RShowDoc("KEYWORDS")
\keyword{sociogram}% __ONLY ONE__ keyword per line
\keyword{shapefile}
