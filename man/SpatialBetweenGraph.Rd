\name{SpatialBetweennessGraph}
\alias{SpatialBetweennessGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spatial Betweenness Graph
%%  ~~function to do ... ~~
}
\description{This function creates the graph of spatial betweenness rank change between alpha values. Seems to not work if modying only from_alpha or to_alpha
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spat_betweenness_change_graph(graph, transparency = TRUE, from_alpha = 0, to_alpha = 1, step = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node}

\item{tranparency}{Specifies whether lines in the graph are highlighted according to the maximum rank changes. Defaults to TRUE}

\item{from_alpha}{Specifies start alpha value of spatial betweenness calculation. Defaults to 0}

\item{to_alpha}{Specifies end alpha value of spatial betweenness calculation. Defaults to 1}

\item{step}{Species the interval at which the alpha values are to be calculated. Defaults to 0.05}

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
spat_betweenness_change_graph()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{betweenness}
\keyword{graph}
\keyword{spatial}
