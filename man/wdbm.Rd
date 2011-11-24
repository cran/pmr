\name{wdbm}
\alias{wdbm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Distance-based Models
}
\description{
Weighted Distance-based Models for ranking data
}
\usage{
wdbm(dset, dtype="tau")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
  \item{dtype}{
type of weighted distance. tau : Kendall's tau, rho : Spearman's rho, rho2 : Spearman's rho square, foot : footrule
}
}
\details{
Fit the weighted distance-based models for the dataset and return a mle object. Standard methods on mle (e.g., AIC(), coef(), vcov()) apply. The modal ranking and the Chi-square residual are given in the output.
}
%%\value{
%%\item{modal.ranking}{The modal ranking of the dataset.}
%%\item{loglik}{Loglikelihood.}
%%\item{par}{Estimate of the weights. Note that weights are sorted according to items instead of modal ranking.}
%%\item{se}{Standard error of the estimates.}
%%\item{fit.value}{Fitted values of the model.}
%%\item{residual}{Pearson sum of square residual of the model.}
%%}
\references{
Lee, P. H., and Yu, P. L. H. (2010) Distance-based tree models for ranking data. Computational Statistics and Data Analysis, 54(6), 1672-1682.
}
\author{
Paul H. Lee and Philip L. H. Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbm}}
}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)

## fit the weighted distance-based model with Spearman's foot distance
wdbm(test,dtype="foot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{weighted distance-base models}
