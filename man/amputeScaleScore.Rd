\name{amputeScaleScore}
\alias{amputeScaleScore}
\title{A function for removing cases from a SGP style dataset}
\description{Function removes cases to produce desired missingness patterns and proportions.
}
\usage{
amputeScaleScore(
  ampute.data,
  additional.data = NULL,
  compact.results = FALSE,
  growth.config = NULL,
  status.config = NULL,
  default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
  demographics = NULL,
  institutions = NULL,
  ampute.vars  = NULL,
  ampute.var.weights = NULL,
  reverse.weight = "SCALE_SCORE",
  ampute.args = list(prop=0.3, type="RIGHT"),
  complete.cases.only = TRUE,
  partial.fill = TRUE,
  invalidate.repeater.dups = TRUE,
  seed = 4224L,
  M = 10)
}

\arguments{
  \item{ampute.data}{The complete dataset in which to create missing values.
  }
  \item{additional.data}{the function will return only data that is required for
  a SGP analysis (based on growth.config). This allows for the addition of more
  data (e.g. 2019 grades not used as priors)
  }
  \item{compact.results}{By default (FALSE), the function will return a list of
  longitudinal datassets with the current (amputed) and prior (unchanged) student
  records. This is helpful for diagnostics and ease of use, but also produces more
  redundant prior data than needed. Setting this argument to TRUE returns a single
  data.table object with a TRUE/FALSE indicator column added for each requested
  amputation. This flag can be used to make the SCALE_SCORE (and/or other variables)
  NA in subsequent use cases.
  }
  \item{growth.config}{An elongated SGP config script with an entry for each
  grade/content_area/year cohort that will be analyzed in subsequent simulations.
  }
  \item{status.config}{An elongated SGP config script. This needs to have an entry
  for each grade/content_area/year cohort that will be analyzed in subsequent
  simulations. Unlike a growth.config entry, status.config entries use data from
  the same grade, but from a prior year (i.e. not individual variables). For example
  you might predict 2021 3rd grade ELA missingness from 2019 3rd grade school
  mean scale score, FRL status, etc.
  }
  \item{default.vars}{Variables that will be used in extracting cohort records
  (subject and grade) and other variables from the data that the user may want
  returned in the amputed data.
  }
  \item{demographics}{Demographic (factor/character) variables to use and/or return
  in the final dataset.
  }
  \item{institutions}{Institution IDs that will be used/or returned in the amputed data
  }
  \item{ampute.vars}{Intersection of default.vars, demographics and institutions
  that will be used in the construction of the weighted scores that define the
  probability of being missing. Any institution included will be used to construct
  institution level means of other student level ampute.vars. For example, with
  c("SCHOOL_NUMBER", "SCALE_SCORE", "FREE_REDUCED_LUNCH_STATUS"), student level
  scores and demographics will be used along with their associated school level mean
  scores and proportion FRL (4 total factors). The default (NULL) means that no factors
  are considered, creating a "missing completely at random" (MCAR) missingness pattern.
  }
  \item{ampute.var.weights}{Relative weights assigned to the ampute vars. Default
  is NULL meaning the weighted sum scores will be calculated with equal weight (=1).
  A named list can be provided with the desired relative weights. For example,
  'list(SCALE_SCORE=3, FREE_REDUCED_LUNCH_STATUS=2, SCHOOL_NUMBER=1)' will weight
  a student's scale scores by a factor of 3 and FRL by 2, with all (any other ampute.vars)
  remaining at the default of 1. This includes school level aggregates in this
  example. Note that differential weights for institutions should be placed at
  the end of the list. If institution IDs (e.g., SCHOOL_NUMBER) are omitted from
  the list, the aggregates will be given the same weight as the associated student
  level variable. In the given example, SCALE_SCORE and school mean scale score
  would be given a relative weight of 3. This argument is ignored when `ampute.vars = NULL`.
  }
  \item{reverse.weight}{The current default for ampute.args$type is "RIGHT", which
  means that students with high weighted scores have the highest probability for
  amputation. This makes sense for high % FRL schools, but not for high achieving
  students and/or students in high achieving schools. This function inverses the
  variable(s) individual (and institutional mean) value(s) so that higher weight
  is given to lower scores/means. This argument is ignored when `ampute.vars = NULL`.
  }
  \item{ampute.args}{Variables to be used in the mice:ampute.continuous function.
  Currenly only `prop` and `type` can be modified. See ?mice::ampute.continuous
  and ?mice::ampute for more information. The `prop` piece is inexact and has required
  some modification on my part. Its still imprecise, particularly for values away
  from 0.5 (50% missing). Also, the max missingness is 85%, and for that you need
  to set prop=0.95 or greater. Note that the prop gives a total proportion missing,
  which accounts for missingness already included in the data due to students with
  incomplete longitudinal data histories. For example, if a cohort starts with 5%
  students missing due to incomplete histories, an additional 25% will be made
  missing to get to the 30% (default) missingness. This last point has been dealt
  with in some regards with the next argument, which removes these cases first.
  }
  \item{complete.cases.only}{Should cases without the most recent prior and current
  score be removed? This removes students with partial longitudinal histories
  from the most recent prior (e.g., 2019) to the current year (e.g., 2021),
  producing a "complete" dataset that is easier to interpret.
  }
  \item{partial.fill}{Should an attempt be made to fill in some of the demographic
  and institution ID information based on students previous values? Part of the
  process of the amputeScaleScore function is to take the long data (ampute.data)
  and then first widen and then re-lengthen the data, which creates holes for
  students with incomplete longitudinal records. This part of the function fills
  in these holes for students with existing missing data.
  }
  \item{invalidate.repeater.dups}{Students who repeat a grade will get missing
  data rows inserted for the grade that they "should" be in in 2021. This leads
  to duplicated cases that can lead to problems in the SGP analyses. This argument
  returns those cases with the VALID_CASE variable set to "INVALID_CASE".
  }
  \item{seed}{A random seed set for the amputation process to allow for replication
  of results, or for alternative results using the same code.
  }
  \item{M}{The number of amputed datasets to return. The default is 10.
  }
}


\details{From the amputation process specified, the function returns either a list
(default) of M amputed datasets or a single data set with M columns of missing
record indicators. The datasets will exclude data for students not used in any of
the specified growth.config or status.config cohorts, unless the additional.data
argument has been included.
}

\value{Function returns either a list (default) of amputed longitudinal data sets
or a single data set containing additional columns of indicators for records to
be removed.
}

\author{Adam R. Van Iwaarden \email{avaniwaarden@nciea.org} }

\examples{
	\dontrun{
    data_to_ampute <- SGPdata::sgpData_LONG_COVID

    ###   Read in STEP 0 SGP configuration scripts
    source("SGP_CONFIG/STEP_0/Ampute_2021/Growth.R")
    source("SGP_CONFIG/STEP_0/Ampute_2021/Status.R")

    ###   NOTE: the amputeScaleScore function requires the "mice" package!
    Test_Data_LONG <- amputeScaleScore(
                            ampute.data = data_to_ampute,
                            growth.config = growth_config_2021,
                            status.config = status_config_2021,
                            M = 1)

		}
}

\keyword{ misc }
\keyword{ models }
