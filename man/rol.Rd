\name{rol}
\alias{rol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Rank-ordered Logit Models
}
\description{
The Rank-ordered Logit (ROL) Models for ranking data. ROL models are extensions of the Luce models by incorporating covariates.
}
\usage{
rol(dset, covariate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset
}
  \item{covariate}{
the covariates of the ranking dataset
}
}
\details{
Fit the rank-ordered logit models for the dataset and return a mle object. Standard methods on mle (e.g., @coef, @vcov) apply. By default, the intercept term is included.
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
Beggs, S., Cardell, S., and Hausman, J. (1981) Assessing the potential demand for electric cars. Journal of Econometrics, 16: 1-19.

Chapman, R. G., and Staelin, R. (1982) Exploiting rank ordered choice set data within the stochastic utility model. Journal of Market Research, 19:288-301. 

Hausman, J., and Ruud, P. A. (1987) Specifying and testing econometric models for rank-ordered data. Journal of Econometrics, 34:83-104.
}
\author{
Paul H. Lee and Philip L. H. Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pl}}
}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
X4 <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3)

## fit the Luce model
## rol(test,X4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Rank-order logit models}
