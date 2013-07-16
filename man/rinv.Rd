\name{rinv}
\alias{rinv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse of a ranking dataset.
}
\description{
Computing the inverse of a summarized (or aggregated) ranking dataset. The function rankagg can be used to created a summarized ranking dataset from individual rankings.
}
\usage{
rinv(dset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Return the dataset which is the orderings of the original ranking data.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
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

## compute the inverse of the artificial dataset
## rinv(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rinv}

