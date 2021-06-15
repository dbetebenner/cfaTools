\name{messageLog}
\alias{messageLog}
\title{A function for creating customized log reports and saving message info.
}
\description{Function produces a text file (custom message log).
}
\usage{
messageLog(
  log.message,
	log.directory = "Logs",
	logfile = NULL,
	add.date = TRUE,
	appendLF = TRUE)
}

\arguments{
  \item{log.message}{Text string to be exported to a text file log.
  }
  \item{log.directory}{File path where log file should be saved.
  }
  \item{logfile}{Name of file to be saved.  If NULL, then file will be named as
  \code{messageLog_*DATE*.txt}, where \code{*DATE*} is the current date.
  }
  \item{add.date}{TRUE. Adds the date to a user provided (non-NULL) \code{logfile} name.
  }
  \item{appendLF}{TRUE. Adds log message to a file (if it already exists).
  }
}

\details{Creates (or adds to) a log file with the text given in the "log.message" argument.
}

\value{A text file with name provided in "logfile", saved in the "log.directory".
}

\author{Adam R. Van Iwaarden \email{avaniwaarden@nciea.org} }

\keyword{ misc }
\keyword{ models }
