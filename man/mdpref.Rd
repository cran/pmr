\name{mdpref}
\alias{mdpref}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multidimensional preference analysis.
}
\description{
Display a 2D plot of the position of both judges and items.
}
\usage{
mdpref(dset,rank.vector=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
\item{rank.vector}{
The vectors of the rankings at default will be displayed if the value is set to TRUE. 
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{item}{Coordinates of the items.}
\item{ranking}{Coordinates of the rankings.}
\item{explain}{Proportion of variance explained by the first two dimensions.}
}
\references{
Carroll, J. D. (1972) Individual differences and multidimensional scaling. In Shepard, R. N., Ronney, A. K., and Nerlove, S. B. (eds.) 
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
mdpref(test,rank.vector=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mdpref}
