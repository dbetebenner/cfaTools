\name{cfaGaps}
\alias{cfaGaps}
\title{Diagnostic achievement gap information}
\description{Function for creating diganostic plots depicting achievement and other types of gaps derived from education assessment data.
}
\usage{
cfaGaps(
	gap_data,
	gap_groups=NULL,
	output_format="PDF")
}

\arguments{
  \item{gap_data}{Assessment data (usually scaled scores) from which to calculate and depict (achievement) gaps.
  }
  \item{gap_groups}{Vector of group names with which to calculate/create gap diagnostic information/plots.
  }
  \item{output_format}{Character string indicating the output format for the figures produced by cfaGaps. Currently supports PDF, PNG, SVG, and PLOTLY.
  }
}


\details{Typical use of the function is to submit scale score data together with group identifiers from which to calculate and depict (achievement) gaps.
}

\value{Function returns plots depicting/representing (achievement) gap data).
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
	\dontrun{
		cfaGaps(gap_data=sgpData_LONG[YEAR=="2018_2019", c("SCALE_SCORE", "ETHNICITY"), with=FALSE],
						gap_groups=c("African American", "White"))  
	}
}

\keyword{ misc }
\keyword{ models }
