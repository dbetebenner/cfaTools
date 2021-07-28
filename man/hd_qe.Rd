\name{hd_qe}
\alias{hd_qe}
\title{Harrell-Davis quantile estimator with trimming proposed by Akinshin.}
\description{Function to compute the Harrell-Davis estimate for any specified quantile(s).
Trimming of the values is also integrated to incrase the robustness of the estimator to
outlier values.
}
\usage{
hd_qe(
	x,
	q = 0.5,
  na.rm = TRUE,
  trim = 0.01)
}

\arguments{
  \item{x}{A vector of continuous observations.
  }
  \item{q}{Quantile(s) between 0 and 1 - default is 0.5, the median..
  }
  \item{na.rm}{Set to TRUE to remove NA, FALSE otherwise - default is TRUE.
  }
	\item{trim}{Proportion of trimming to do. Default is 0.01 - 1 percent of both tails of the distribution.
  }
}


\details{\code{hd_qe} is a weighted average of all the order statistics. See
\url{https://garstats.wordpress.com/2016/06/09/the-harrell-davis-quantile-estimator/}
for more details. This function is taken in part from the from the \code{rogme} package
by Guillaume Rousselet (\url{https://github.com/GRousselet/rogme/}, which in turn is
based on Rand Wilcox's \code{WRS} package - \url{https://github.com/nicebread/WRS/}).
The function incorporates suggestions to improve the robustness of the Harrell-Davis
estimatorby adding in the ability to trim observed values, based (loosely) on the
suggestions of Andrey Akinshin in a series of blogs - see \url{https://aakinshin.net/posts/winsorized-hdqe/}
and linked posts.
}

\value{Function returns a quantile estimate (numeric) vector of length \code{q}.
}

\author{Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\examples{
	\dontrun{
		###  require(SGPdata)
		hd_qe(
  			x = sgpData_LONG_COVID[
					      YEAR=="2019" &
								CONTENT_AREA=="ELA" &
								GRADE=="3", SCALE_SCORE],
  			q = 1:9/10)
	}
}

\keyword{ misc }
