\name{rankplot}
\alias{rankplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualizing rankings of 3 or 4 items by plot.
}
\description{
Display rankings of 3 items by hexagon and 4 items by truncated octahedron. Each line linking two points represents a Kendall distance of one. The size of the point is proportional to the frequency of that particular ranking.
}
\usage{
rankplot(dset,trans=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
\item{trans}{
(For 4-item ranking dataset) Another representation of the rankings. 
}
}
\details{
As a ranking dataset of k items can be represented in k-1 dimensions, ranking data with 3 or 4 items can be represented in graphs without losing any information. For visualization of ranking data with more items, multidimensional preference analysis can be used.
}
%%\value{

%%}
\references{
Thompson, G. L. (1993). Graphical techniques for ranked data. In Fligner, M. A., and Verducci, J. S. (eds.) Probability Models and Statistical Analyses for Ranking Data, pp. 294-298.
}
\author{
Paul H. Lee and Philip L. H. Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)

## multidimensional preference analysis of the artificial dataset
## rankplot(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rankplot}

