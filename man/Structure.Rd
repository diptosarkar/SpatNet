\name{Structure}
\alias{Structure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Structure Diagram
%%  ~~function to do ... ~~
}
\description{This function plots the structure diagram for spatial social networks
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
structure_viz(graph, resolution = FALSE, num = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{graph}{igraph object with V(g)$X and V(g)$Y having the X and Y co-ordinates of each node.}

\item{resolution}{Specifies the grouping of points for numbering. Defaults to NA and numbering cannot be performed}

\item{num}{Specifies whether points should be aggregated and numbered. Requires resolution to be specified}
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
structure_viz()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{clustering}
\keyword{spatial}
\keyword{diagram}
\keyword{plot}
