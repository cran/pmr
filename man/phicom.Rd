\name{phicom}
\alias{phicom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
phi-component Models
}
\description{
phi-component Models for ranking data. The distance-based models assume that rankings closer to the modal ranking are more likely to be observed. Phi-component models are extensions of distance-based models with Kendall's distance by allowing weights at different stages.
}
\usage{
phicom(dset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset (aggregated)
}

}
\details{
Fit the phi-component models for the dataset and return a mle object. Standard methods on mle (e.g., @coef, @vcov) apply. The modal ranking and the Chi-square residual are given in the output.
}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%\item{modal.ranking}{The modal ranking of the dataset.}
%%\item{loglik}{Loglikelihood.}
%%\item{par}{Estimate of the dispersion parameter.}
%%\item{se}{Standard error of the estimate.}
%%\item{fit.value}{Fitted values of the model.}
%%\item{residual}{Pearson sum of square residual of the model.}
%%}
\references{
Fligner, M. A., and Verducci, J. S. (1986) Distance based ranking models. Journal of the Royal Statistical Society Series B, 48(3), 359-369.
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

## fit the phi-component model
## phicom(test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{phi-component model}

