\name{computeHoMetric2}
\alias{computeHoMetric2}
\title{Compute Ho Metric 2 "Fair Trend"}
\description{This function creates individual level predictied scale scores for use in comparisions between 2019 and 2021.
}
\usage{
computeHoMetric2(
  covid.data
}

\arguments{
  \item{covid.data}{This function takes long data following the SGP data structure. Defaults to sgpData_LONG_COVID. 
  }
}


\details{This function creates individual level predictied scale scores for use in comparisions between 2019 and 2021.
}

\value{Function returns a dataframe with CONTENT_AREA, ID, YEAR, GRADE, and SCALE_SCORE_predict. This last column, SCALE_SCORE_predict is the individual student scores to be used in aggregations to produce Ho's Fair Trend Statistics (i.e., "adjusted" 2019 scores).
}

\author{Nathan Dadey \email{ndadey@nciea.org} }

\examples{
### computeHoMetric2 needs an object of class SGP run.
### See:
### https://centerforassessment.github.io/cfaTools/articles/fair_trend.html
}

\keyword{ misc }
\keyword{ models }
