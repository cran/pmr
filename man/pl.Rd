\name{pl}
\alias{pl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Luce Models
}
\description{
The Luce Models for ranking data
}
\usage{
pl(dset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
}
\details{
Fit the Luce models for the dataset and return a mle object. Standard methods on mle (e.g., AIC(), coef(), vcov()) apply. The Chi-square residual are given in the output.
}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%\item{loglik}{Loglikelihood.}
%%\item{par}{Estimate of the dispersion parameter.}
%%\item{se}{Standard error of the estimate.}
%%\item{fit.value}{Fitted values of the model.}
%%\item{residual}{Pearson sum of square residual of the model.}
%%}
\references{
Luce, R. D. (1959) Individual Choice Behavior. New York: Wiley.
}
\author{
Paul H. Lee and Philip L. H. Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rinv}}
}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)

## fit the Luce model
pl(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Luce models}
