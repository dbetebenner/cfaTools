\name{academicImpactSummary}
\alias{academicImpactSummary}
\title{Academic Impact Summary Data from individual level SGP Data}
\description{Function for creating summary (e.g., School) impact data.
}
\usage{
academicImpactSummary(
	sgp_data,
	state,
	current_year=NULL,
	prior_year=NULL,
	content_areas=NULL,
	grades=NULL)
}

\arguments{
  \item{sgp_data}{SGP object or LONG data output from SGP analyses.
  }
  \item{state}{Two letter state acronymn for use with SGPstateData embedded in SGP package for percent proficient calculations.
  }
  \item{current_year}{Year to be considered as current for purposes of academic impact calculations.
  }
  \item{prior_year}{Year to be considered as prior for purposes of academic impact calculations.
  }
  \item{content_areas}{Content areas for which to calculate academic impact.
  }
  \item{grades}{Grades within the designated content areas to calculate academic impact.
  }
}


\details{Typical use of function creates summary file for schools or districts indicating academic impact.
}

\value{Function returns a summary table providing non-adjusted and adjusted median SGPs as well as non-adjusted and adjusted gamma effect sizes.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
	\dontrun{
		academicImpactSummary(
			sgp_data=sgpData_LONG_COVID,
			state="DEMO_COVID",
			current_year="2021",
			prior_year="2019",
			content_areas=c("ELA", "MATHEMATICS"),
			grades=as.character(3:8))
	}
}

\keyword{ misc }
\keyword{ models }
