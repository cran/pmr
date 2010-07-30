\name{dbm}
\alias{dbm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance-based Models
}
\description{
Distance-based Models for ranking data
}
\usage{
dbm(dset, dtype="tau")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
  \item{dtype}{
type of distance. tau : Kendall's tau, rho : Spearman's rho, rho2 : Spearman's rho square, foot : footrule
}
}
\details{
Fit the distance-based models for the dataset.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{modal.ranking}{The modal ranking of the dataset.}
\item{loglik}{Loglikelihood.}
\item{par}{Estimate of the dispersion parameter.}
\item{se}{Standard error of the estimate.}
\item{fit.value}{Fitted values of the model.}
\item{residual}{Pearson sum of square residual of the model.}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Paul H. Lee and Philip L. H. Yu
}
%% \note{
%%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wdbm}}
}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)

## fit the distance-based model with Spearman's rho distance
dbm(test,dtype="rho")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distance-based model}
