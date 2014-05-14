\name{song}
\alias{song}
\docType{data}
\title{
song
}
\description{
Ranking of 5 items according to the similarity with the word "song".
}
\usage{data(song)}
\format{
  A data frame with 83 observations on the following 6 variables.
  \describe{
    \item{\code{X1}}{the rank of word "score"}
	\item{\code{X2}}{the rank of word "instrument"}
	\item{\code{X3}}{the rank of word "solo"}
	\item{\code{X4}}{the rank of word "benediction"}
	\item{\code{X5}}{the rank of word "suit"}
    \item{\code{n}}{number of observations}
  }
}
\details{
83 college students were asked to rank five words according to the similarity with the word "song". The five words were (1) score, (2) instrument, (3) solo, (4) benediction, and (5) suit.
}
\source{
Critchlow, D. E., Fligner, M. A., and Verducci, J. S. (1991) Probability models on rakning. Journal of Mathematical Psychology, 35, 294-318.
}
\references{
Critchlow, D. E., Fligner, M. A., and Verducci, J. S. (1991) Probability models on rakning. Journal of Mathematical Psychology, 35, 294-318.
}
\examples{
data(song)
## maybe str(song) ; plot(song) ...
}
\keyword{datasets}
