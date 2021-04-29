\name{bootstrapSRS_SGP}
\alias{bootstrapSRS_SGP}
\title{Higher level function for using bootstrap_SRS with SGP objects}
\description{Function for calculating bootstrap summary statistics using data and pre-designated strata proportions with SGP objects.
}
\usage{
bootstrapSRS_SGP(
	sgp_object,
        strata_variables,
        strata_proportions_years,
        summary_years,
        create_scale_score_deciles=c("PRIOR", "CURRENT"),
        sample_size=NULL,
        bootstrap_iterations=100,
        summary_statistic="mean",
        aggregation_group=c("CONTENT_AREA", "YEAR")
}

\arguments{
  \item{sgp_object}{
  }
  \item{strata_variables}{
  }
  \item{strata_proportions_years}{
  }
  \item{summary_years}{
  }
  \item{create_scale_score_deciles}{
  }
  \item{sample_size}{
  }
  \item{bootstrap_iterations}{
  }
  \item{summary_statistic}{
  }
  \item{aggregation_group}{
  }
}


\details{Function calculates a summary statistic of existing data (e.g., mean) by re-sampling based upon designated strata proportions
}

\value{Function returns either a list (default) containing the summary statistic (e.g., mean) and standard deviation (i.e., standard error) for the bootstrap sample of that statistic or funtion returns a vector containing the bootstrap replication values.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
### bootstrapSRS_SGP needs an object of class SGP run.
### See the Step 6 comparisons in the Demonstration Learning Loss Analytics for examples
### https://github.com/CenterForAssessment/SGP_Research/tree/master/Demonstration/Learning_Loss_Analysis/Step_6_Summary_Comparisons 
}

\keyword{ misc }
\keyword{ models }
