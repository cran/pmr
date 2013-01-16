\name{local.knn.cv}
\alias{local.knn.cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Local k-nearest neighbor method for label ranking.
}
\description{
Local k-nearest neighbor method with the parameter k determined using cross-validation error (defined as the sum of Kendall's distance).
}
\usage{
local.knn.cv(dset,covariate.test,covariate,cv=10,k.max=20,method.cv="mean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
a ranking dataset for training the k-nearest neighbor.
}
\item{covariate.test}{
the covariates of the judges to be predicted. 
}
\item{covariate}{
the covariates of the rankings. 
}
\item{cv}{
the number of cross-validated samples. The default value is 10.
}
\item{k.max}{
the maximum number of nearest neighbors to be tested. The default value is 20.
}
\item{method.cv}{
the prediction method. mean : mean rank, pl : Luce model
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{

%%}
\references{
Cheng, W., Dembczynski, K., Hullermeier, E. (2010). Label ranking methods based on the Plackett-Luce model. Proceedings of ICML 2010.
}
\author{
Paul H. Lee and Philip L. H. Yu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{local.knn}}
}
\examples{
## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
co <- c(6,5,4,3,2,1)
co.test <- 1.2
train <- data.frame(X1,X2,X3)

## local k-nearest neighbor method of the artificial dataset
## local.knn.cv(train,co.test,co)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{local.knn.cv}

