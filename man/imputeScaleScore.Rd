\name{imputeScaleScore}
\alias{imputeScaleScore}
\title{Multiple imputation of missing scale scores for SGP style data}
\description{Function produces multiple imputations for missing data via methods
available for the "mice" package.
}
\usage{
imputeScaleScore(
  impute.data,
  additional.data = NULL,
  include.additional.missing = TRUE,
  compact.results = TRUE,
  return.current.year.only = FALSE,
  diagnostics.dir = getwd(),
  growth.config = NULL,
  status.config = NULL,
  default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
  demographics = NULL,
  institutions = NULL,
  impute.factors = "SCALE_SCORE",
  impute.long = FALSE,
  impute.method = NULL,
  cluster.institution = FALSE, # TRUE for multilevel, cross-sectional methods
  partial.fill = TRUE,
  parallel.config = NULL, # define cores, packages, cluster.type
  seed = 4224L,
  M = 10,
  maxit = 5,
  verbose=FALSE,
  ...)
}

\arguments{
  \item{impute.data}{The incomplete dataset in which to impute student scale score values.
  }
  \item{additional.data}{The function will return only data that is required for
  a SGP analysis (based on growth.config). This allows for the addition of more
  data (e.g. 2019 grades not used as priors)
  }
  \item{include.additional.missing}{TRUE. The function will create scale scores
  for students present in the most recent prior year without rows in the current
  year. If FALSE, assumes that rows with \code{SCALE_SCORE = NA} are present in
  the current year (and with \code{VALID_CASE == "VALID_CASE"}), and scores will
  only be imputed for those cases/rows.
  }
  \item{compact.results}{By default (TRUE), the function returns a single data.table
  object with a column added for each requested imputation (M). If FALSE, function
  will return a list of longitudinal datassets with the current (imputed) and prior
  (unchanged) student records. This is helpful for diagnostics and ease of use,
  but also produces more redundant prior data than needed.
  }
  \item{return.current.year.only}{FALSE. Return all records from the long data used
  to create the imputed scale scores.  If TRUE, only records from the current year
  are returned.
  }
  \item{diagnostics.dir}{Directory path for diagnostic plots to be placed. Default is
  the working directory. Additional subdirectories are created internally as needed.
  }
  \item{growth.config}{An elongated SGP config script with an entry for each
  grade/content_area/year cohort that will be analyzed in subsequent simulations.
  }
  \item{status.config}{An elongated SGP config script with an entry for each
  grade/content_area/year cohort that will be analyzed in subsequent simulations.
  Unlike a growth.config entry, status.config entries use data from the same grade,
  but from a prior year (i.e. not individual variables). For example you might
  impute missing 2021 3rd grade ELA scores from 2019 3rd grade school mean scale
  score, FRL status, etc.
  }
  \item{default.vars}{Variables that will be used in extracting cohort records
  (subject and grade) and other variables from the data that the user may want
  returned in the imputed data.
  }
  \item{demographics}{Demographic (factor/character) variables to use and/or return
  in the final dataset.
  }
  \item{institutions}{Institution IDs that will be used/or returned in the imputed data
  }
  \item{impute.factors}{Intersection of default.vars, demographics and institutions
  that will be used in the imputation calculations. An institution included will
  be used to construct institution level means of other student level impute.factors.
  For example, with c("SCHOOL_NUMBER", "SCALE_SCORE", "FREE_REDUCED_LUNCH_STATUS"),
  student level scores and demographics will be used along with their associated
  school level mean scores and proportion FRL (4 total factors). The default
  ('SCALE_SCORE') means that only prior scale scores are considered.
  }
  \item{impute.long}{Defaults to FALSE, meaning scores are imputed in a "wide",
  or cross-sectional format. Some imputation methods (e.g., "2l.pan") can be used
  to impute the data in a longitudinal form (clustered by student over time/grade),
  in which case this argument should be set to TRUE.
  }
  \item{impute.method}{The name of the method from the mice package or other add-on
  package functions used to impute the scale scores. The default is NULL, which
  translates to the default method in the mice package - "pmm" for predictive mean
  matching.
  }
  \item{cluster.institution}{Defaults to FALSE, meaning institution IDs will not
  be used to identify clustering/levels in a multilevel fashion. Typically only
  used for multilevel methods (e.g., "2l.pan" or "2l.pmm"), but can also be used
  in other methods to include institution ID as a factor variable.
  }
  \item{partial.fill}{Should an attempt be made to fill in some of the demographic
  and institution ID information based on students previous values? Part of the
  process of the imputeScaleScore function is to take the long data (impute.data)
  and then first widen and then re-lengthen the data, which creates holes for
  students with incomplete longitudinal records. This part of the function fills
  in these holes for students with existing missing data.
  }
  \item{parallel.config}{The default is NULL meaning imputations will be calculated
  sequentially in a call to mice::mice. Alternatively, a list with named elements
  cores, packages and/or cluster.type can be provided. The "cores" element should
  be a singl numeric value for the number of CPU cores/hyperthreads to use. The
  "packages" element is a character string of package name(s) for the requested
  imputation methods. "cluster.type" specifies the parallel backend to use (typically
  FORK for Linux/Unix/Mac and "PSOCK" for Windows). An example list might look
  like: "list(packages = c('mice', 'miceadds'), cores=10)".
  }
  \item{seed}{A random seed set for the imputation process to allow for replication
  of results, or for alternative results using the same code.
  }
  \item{M}{The number of imputed datasets to return. The default is 10.
  }
  \item{maxit}{The number of iterations allowed for each imputation process. See
  the 'mice' package documentation for details.
  }
  \item{verbose}{Defaults to FALSE, meaning progress information from the mice
  package is not printed out to the console.
  }
  \item{...}{Additional arguments for the mice::mice function and any particular
  imputation method/function. See each function/package documentation for details.
  }
}


\details{The function returns either a single imputed data set (with additional
columns for each number of requested imputations) or a list of M imputed datasets
from the specified imputation method. The datasets will exclude data for students
not used in any of the specified growth.config or status.config cohorts, unless
the additional.data argument has been included.
}

\value{Function returns either a single data set containing additional columns of
imputed records of a list (default) of imputed longitudinal data sets.
}

\author{Adam R. Van Iwaarden \email{avaniwaarden@nciea.org} }

\examples{
	\dontrun{
    data_to_impute <- SGPdata::sgpData_LONG_COVID

    ###   Read in STEP 0 SGP configuration scripts
    source("SGP_CONFIG/STEP_0/Ampute_2021/Growth.R")
    source("SGP_CONFIG/STEP_0/Ampute_2021/Status.R")

    ###   NOTE: the imputeScaleScore function requires the "mice" package!
    Test_Data_LONG <- imputeScaleScore(
                            impute.data = data_to_impute,
                            growth.config = growth_config_2021,
                            status.config = status_config_2021,
                            M = 1)

		}
}

\keyword{ misc }
\keyword{ models }
