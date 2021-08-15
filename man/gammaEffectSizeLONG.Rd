\name{gammaEffectSizeLong}
\alias{gammaEffectSizeLong}
\title{Calculate Gamma effect sizes from a longitudinal data set. }
\description{Function for calculating non-parametric effect size for any specified
quantile(s) as proposed by Akinshin.
}
\usage{
gammaEffectSizeLong(
	data_table,
  variable,
  year_1,
  year_2,
  quantiles=1:9/10,
  digits=3)
}

\arguments{
  \item{data_table}{Longitudinal dataset (e.g., from the SGP package) from which
	to calculate effect sizes over specified years.
  }
  \item{variable}{Character (length 1) identifying the variable of interest (e.g., \sQuote{SCALE_SCORE}).
  }
	\item{year_1}{Character identifying the comparison/control year(s). Can be multiple years.
	Variable assumed to be named \sQuote{YEAR}.
	}
	\item{year_2}{Character identifying the year(s) of interest (treatment). Can be multiple years.
	Variable assumed to be named \sQuote{YEAR}.
	}
  \item{quantiles}{Argument to specify quantile(s) to evaluate (between 0 and 1). Default is Deciles.
  }
	\item{digits}{Number of digits to round results to.
	}
}


\details{Typical use of the function is to submit scale score or SGP data from
 which to calculate effect sizes (standardized differences in the two distributions
 at the given quantiles). This function applies the lower level function Andrey Akinshin
 proposed in a June 25, 2020 blog post - \url{https://aakinshin.net/posts/nonparametric-effect-size/}
 to longitudinal data typically produced from SGP analyses. Usually done within
 a \code{data.table}
}

\value{Function returns an effect size (numeric) vector of length \code{quantiles}
that is easily combined into a table using \href{https://github.com/Rdatatable/data.table/}{data.table}.
See example below.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@nciea.org}}

\examples{
	\dontrun{
		###  require(SGPdata)
		###  require(data.table)
		sgpData_LONG_COVID[,
  			as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE", "2019", "2021")),
				keyby=c("CONTENT_AREA", "GRADE")]
	}
}

\keyword{ misc }
\keyword{ models }
