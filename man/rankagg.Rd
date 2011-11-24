\name{rankagg}
\alias{rankagg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse of a ranking dataset.
}
\description{
Turn individual rankings into a summary matrix.
}
\usage{
rankagg(dset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
each row represent a single ranking
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Return the dataset which summarize the original ranking data.
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
test <- data.frame(X1,X2,X3)

## aggregate the ranking of all the observations and create a summary matrix
test2 <- rankagg(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rankagg}

