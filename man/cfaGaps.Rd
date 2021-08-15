\name{cfaGaps}
\alias{cfaGaps}
\title{Diagnostic achievement gap information}
\description{Function for creating diganostic plots depicting achievement and other types of gaps derived from education assessment data.
}
\usage{
cfaGaps(
	gap_data,
	gap_groups=NULL,
	gap_plot_title="Non-Parametric Gap Plot",
	gap_plot_fig_width=13,
	gap_plot_fig_height=8.5,
	gap_plot_file_path="Visualizations/Gaps/",
	gap_plot_file_name="Gaps",
	gap_plot_output_format="PDF")
}

\arguments{
  \item{gap_data}{Assessment data (usually scaled scores) from which to calculate and depict (achievement) gaps.
  }
  \item{gap_groups}{Vector of group names with which to calculate/create gap diagnostic information/plots.
  }
  \item{gap_plot_title}{Character string indicating the title of the gap plot. Default is 'Non-paerametric gap plot'.
  }
  \item{gap_plot_fig_width}{Width (in inches) of gap plot. Default is 13 inches.
  }
  \item{gap_plot_fig_height}{Height (in inches) of gap plot. Default is 8.5 inches.
  }
  \item{gap_plot_file_path}{Path to be used where gap plot is saved. Default is 'Visualizations/Gaps/'.
  }
  \item{gap_plot_file_name}{File name to be used for gap plot. Default is 'Gaps'.
  }
  \item{gap_plot_output_format}{Character string indicating the output format for the figures produced by cfaGaps. Currently supports PDF, PNG, SVG, and PLOTLY.
  }
}


\details{Typical use of the function is to submit scale score data together with group identifiers from which to calculate and depict (achievement) gaps.
}

\value{Function returns plots depicting/representing (achievement) gap data).
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\examples{
	\dontrun{
		cfaGaps(
			gap_data=sgpData_LONG[
									YEAR=="2018_2019",
										c("SCALE_SCORE",
										  "ETHNICITY"),
									with=FALSE],
			gap_groups=c("African American", "White"))
	}
}

\keyword{ misc }
\keyword{ models }
