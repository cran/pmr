\name{ahp}
\alias{ahp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Analytic hierarchy process (AHP)
}
\description{
Computing the weights, Saaty's (1977) and Koczkodaj's (1997) inconsistencies for analytic hierarchy process (AHP).
}
\usage{
ahp(dset, sim_size=500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
an ``A'' matrix. It should be a square matrix with diagonal values equal 1 and $a_{ij} = 1/a_{ij}$.
}
  \item{sim_size}{
	simulation size for computation of Saaty's inconsistency. Default is 500.
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{weighting}{Eigenvalues of the criteria.}
\item{Saaty}{Saaty's inconsistency.}
\item{Koczkodaj}{Koczkodaj's inconsistency.}
}
\references{
Koczkodaj, W. W., Herman, M. W., and Orlowski, M. (1997) Using consistency-driven pairwise comaprisons in knowledge-based systems. Proceedings of the sixth international conference on information and knowledge management, ACM Press, 91-96.

Saaty, T. L. (1980) The Analytic Hierarchy Process. McGraw-Hill, New York.

Saaty, T. L. (1977) A scaling methods for priorities in hierarchical structure. Journal of Mathematical Psychology, 15, 234-281.
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
## create an artificial A matrix abc, example taken from Koczkodaj et al. (1997)
abc <- matrix(data = 1:15, nrow = 4, ncol = 4, byrow = TRUE)abc[1,1] <- 1abc[1,2] <- 2abc[1,3] <- 5abc[1,4] <- 4abc[2,1] <- 1/2abc[2,2] <- 1abc[2,3] <- 3abc[2,4] <- 1.9abc[3,1] <- 1/5abc[3,2] <- 1/3abc[3,3] <- 1abc[3,4] <- 0.7abc[4,1] <- 1/4abc[4,2] <- 1/1.9abc[4,3] <- 1/0.7abc[4,4] <- 1

## compute the weights, Saaty's and Koczkodaj's inconsistencies
## ahp(abc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Analytic hierarchy process}

