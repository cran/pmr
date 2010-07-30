\name{idea}
\alias{idea}
\docType{data}
\title{
idea
}
\description{
Ranking of 5 items according to the similarity with the word "idea".
}
\usage{data(idea)}
\format{
  A data frame with 98 observations on the following 6 variables.
  \describe{
    \item{\code{X1}}{the rank of word "thought"}
	\item{\code{X2}}{the rank of word "play"}
	\item{\code{X3}}{the rank of word "theory"}
	\item{\code{X4}}{the rank of word "dream"}
	\item{\code{X5}}{the rank of word "attention"}
    \item{\code{n}}{number of observations}
  }
}
\details{
98 college students were asked to rank five words according to the similarity with the word "idea". The five words were (1) thought, (2) play, (3) theory, (4) dream, and (5) attention.
}
\source{
Fligner, M. A., and Verducci, J. S. (1986) Distance based ranking models. Journal of the Royal Statistical Society Series B, 48(3), 359-369.
}
\references{
Fligner, M. A., and Verducci, J. S. (1986) Distance based ranking models. Journal of the Royal Statistical Society Series B, 48(3), 359-369.
}
\examples{
data(idea)
## maybe str(idea) ; plot(idea) ...
}
\keyword{datasets}
