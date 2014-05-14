\name{leisure.black}
\alias{leisure.black}
\docType{data}
\title{
leisure.black
}
\description{
Ranking of 3 items according to the preference of the group in an activity.
}
\usage{data(leisure.black)}
\format{
  A data frame with 13 observations on the following 4 variables.
  \describe{
    \item{\code{X1}}{Male}
	\item{\code{X2}}{Female}
	\item{\code{X3}}{Both}
    \item{\code{n}}{number of observations}
  }
}
\details{
13 black women, aged 70-79, were asked whom they would prefer to spend their leisure time.
}
\source{
Hollander, M. and Sethuraman, J. (1978) Testing for agreement between groups of judges. Biometrika, 65, 403-411. 
}
\references{
Marden, J. I. (1995) Analyzing and Modeling Rank Data. Chapman and Hall.
}
\examples{
data(leisure.black)
## maybe str(leisure.black) ; plot(leisure.black) ...
}
\keyword{datasets}
