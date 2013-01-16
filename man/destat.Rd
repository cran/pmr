\name{destat}
\alias{destat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive statistics of a ranking dataset
}
\description{
Computing the descriptive statistics (mean rank, pairs matrix, and marginals matrix) of a ranking dataset.
}
\usage{
destat(dset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset (aggregated)
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{mean.rank}{Mean rank of the items.}
\item{pair}{The number of observations which the first item (row) is more preferred than the second item (column).}
\item{mar}{The number of observations which the item i (row) is ranked j (column). }
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

## compute the descriptive statistics of the artificial dataset
## destat(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{destat}

