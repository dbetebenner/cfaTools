\name{imputationSummary}
\alias{imputationSummary}
\title{A function for pooling imputed scale scores, SGPs and Baseline SGPs}
\description{Function produces summary tables and multiple imputation statistics
for imputed score data and SGP analyses.
}
\usage{
imputationSummary(
  data.to.summarize,
  institution.level = NULL,
  summary.level = NULL,
  standardize.scores = NULL)
}

\arguments{
  \item{data.to.summarize}{An dataset of imputed scale scores and SGPs (cohort and
  baseline referenced) calculated using those imputed scores.
  }
  \item{institution.level}{Institution ID for summary table aggregation. E.g.,
  "SCHOOL_NUMBER" or "DISTRICT_NUMBER". \code{NULL} (default) aggregates across
  all students (entire state).
  }
  \item{summary.level}{Additional aggregation levels. E.g., "GRADE" and/or "CONTENT_AREA".
  }
  \item{standardize.scores}{If not \code{NULL} (default), then level by which Scale
  scores are standardized (by mean and SD of the observed scores). E.g., "GRADE"
  to standardize by grade for cross grade comparisons.
  }
}


\details{Produces summary tables and multiple imputation statistics. Function assumes
data contains variables named "SCALE_SCORE_OBSERVED", "SGP_OBSERVED", and "SGP_BASELINE_OBSERVED"
for the unaltered (missing) data, and imputed/imputation derived variables with the
tag \code{_IMPUTED_} added. For example, "SCALE_SCORE_IMPUTED_1", "SGP_IMPUTED_1", and
"SGP_BASELINE_IMPUTED_1". That is, the data is in a wide format (not stacked by imputation).
}

\value{Function returns a list including a summary table disaggregated by the requested factors
and a table including the overall mean differences and correlations between the observed
and (averaged/pooled) imputed values (i.e. mean SGPs and scale scores). The summary table
includes several statistics derived for the evaluation of multiple imputation analyses.
}

\author{Adam R. Van Iwaarden \email{avaniwaarden@nciea.org} }

\keyword{ misc }
\keyword{ models }
