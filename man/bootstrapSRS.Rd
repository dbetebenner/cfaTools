\name{bootstrapSRS}
\alias{bootstrapSRS}
\title{Bootstrap sampling function for use with pre-designated strata proportions}
\description{Function for bootstrap summary statistics using data and
pre-designated strata proportions.
}
\usage{
bootstrapSRS(
        summary_variable,
        strata_variable,
        population_strata_proportions,
        sample_size=NULL,
        bootstrap_iterations=100,
        summary_statistic="mean",
        return_bootstrap_distribution=FALSE)
}

\arguments{
  \item{summary_variable}{
  }
  \item{strata_variable}{
  }
  \item{population_strata_proportions}{
  }
  \item{sample_size}{
  }
  \item{bootstrap_iterations}{
  }
  \item{summary_statistic}{
  }
  \item{return_bootstrap_distribution}{
  }
}


\details{Function calculates a summary statistic of existing data (e.g., mean)
by re-sampling based upon designated strata proportions
}

\value{Function returns either a list (default) containing the summary statistic
(e.g., mean) and standard deviation (i.e., standard error) for the bootstrap
sample of that statistic or funtion returns a vector containing the bootstrap
replication values.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
	\dontrun{
		bootstrapSRS <-
		        data_table=sgpData_LONG[
                          CONTENT_AREA=="MATHEMATICS" &
                          GRADE=="5" &
                          YEAR=="2020_2021"][,
                              c("SCALE_SCORE", "ETHNICITY"),
                          with=FALSE],
		        summary_variable="SCALE_SCORE",
		        strata_variable="ETHNICITY",
		        population_strata_proportions=
                c('African American'=.157,
                  Asian=.054,
                  Hispanic=.261,
                  'Native American'=.033,
                  White=.495),
		        sample_size=10000,
		        bootstrap_iterations=100,
		        summary_statistic="mean",
		        return_bootstrap_distribution=FALSE)
		}
}

\keyword{ misc }
\keyword{ models }
