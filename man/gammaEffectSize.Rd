\name{gammaEffectSize}
\alias{gammaEffectSize}
\title{Calculate Gamma effect size proposed by Akinshin.}
\description{Function for calculating non-parametric effect size for any specified quantile(s).
}
\usage{
gammaEffectSize(
	x,
	y,
	prob)
}

\arguments{
  \item{x}{Numeric vector of values from the comparison group (e.g., scale scores from grade 3 math for year 1).
  }
  \item{y}{Numeric vector of values from the group of interest (e.g., scale scores from grade 3 math for year 2).
  }
  \item{prob}{Argument to specify quantile(s) to evaluate (between 0 and 1).
  }
}


\details{Typical use of the function is to submit scale score or SGP data from
 which to calculate effect sizes (standardized differences in the two distributions
 at the given quantiles). This function is taken directly from the Andrey Akinshin
 blog from June 25, 2020 - \url{https://aakinshin.net/posts/nonparametric-effect-size/}.
}

\value{Function returns an effect size (numeric) vector of length \code{prob}.
}

\author{Andrey Akinshin, Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\examples{
	\dontrun{
		###  require(SGPdata)
		###  require(data.table)
		gammaEffectSize(
  			x = na.omit(sgpData_LONG_COVID[YEAR=="2019" & CONTENT_AREA=="ELA" & GRADE=="3", SCALE_SCORE]),
  			y = na.omit(sgpData_LONG_COVID[YEAR=="2021" & CONTENT_AREA=="ELA" & GRADE=="3", SCALE_SCORE]),
				prob = 1:9/10)
	}
}

\keyword{ misc }
\keyword{ models }
