`messageLog` <- function(
	log.message,
	log.directory = "Logs",
	logfile = NULL,
	add.date = TRUE,
	appendLF = TRUE) {

	PrintLogMessage <- function() {
		# print log message to file
		if (!dir.exists(log.directory)) dir.create(log.directory, recursive = TRUE)
		if (is.null(logfile)) {
			logfile <- file.path(log.directory, paste0("messageLog_", gsub("-", "_", Sys.Date()), ".txt"))
		} else {
			if (add.date) {
				logfile <- paste0(logfile, "_", paste(strsplit(as.character(Sys.Date()), "-")[[1]][c(2,3,1)], collapse="_"), ".txt")
			} else logfile <- paste0(logfile, ".txt")
			logfile <- file.path(log.directory, logfile)
		}

		if (is.call(log.message)) {
			log.message2 <- c(paste0("\n\n\t", as.character(log.message)[1L], "(\n\t\t"), paste(names(log.message)[-1L], as.character(log.message)[-1L], sep=" = ", collapse="\n\t\t"), ")\n\n")
			cat(log.message2, file = logfile, append=appendLF)
		} else cat(log.message, "\n", file=logfile, sep="", append=appendLF)
	}

	if (!is.call(log.message)) {
		base::message(log.message)
	}
	PrintLogMessage()
	invisible()
}
