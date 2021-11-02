\name{getShiftedValues}
\alias{getShiftedValues}
\title{Function for creating lagged/leading value variables from LONG data}
\description{Function for creating lagged/leading value variables from LONG data.
}
\usage{
getShiftedValues(
	data_table,
	shift_group=c("ID", "CONTENT_AREA"),
	shift_period="YEAR",
	shift_variable="SCALE_SCORE",
	shift_amount=1L,
	shift_type="lag")
}

\arguments{
  \item{data_table}{Data table to add lagged/leading variable(s) to.
  }
  \item{shift_group}{Variable(s) indicating groups for which to create lagged/leading variables for.
  }
  \item{shift_period}{Variable indicating what period the lagged/leading shift occurs relative to.
  }
  \item{shift_variable}{Variable that lagged/leading values are derived from.
  }
  \item{shift_amount}{Number of period(s) that the shift_variable is shifted relative to the shift_period.
  }
  \item{shift_type}{Either "lag" or "lead".
  }
}


\details{Typical use of the function is to create prior scale score for students from a LONG formatted longitudinal file.
}

\value{Function returns the submitted \code{data_table} and appends additional variables with suffixes added to the \code{shift_variable} name. For example \code{SCALE_SCORE_LAG_1}.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
	\dontrun{
		getShiftedValues(SGPdata::sgpData_LONG)
	}
}

\keyword{ misc }
\keyword{ models }
