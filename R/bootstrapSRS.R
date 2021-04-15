`bootstrapSRS` <-
function(summary_variable,
        strata_variable,
        population_strata_proportions,
        sample_size=NULL,
        bootstrap_iterations=100,
        summary_statistic="mean", ## or median
        return_bootstrap_distribution=FALSE) {

	sample_data <- data.table(SUMMARY_VARIABLE=summary_variable, STRATA_VARIABLE=as.character(strata_variable))
	if (is.null(sample_size)) sample_size=nrow(sample_data)
	stopifnot(length(population_strata_proportions) == uniqueN(sample_data[['STRATA_VARIABLE']]))
	tmp.replications <- ceiling(max((population_strata_proportions*sample_size)/table(sample_data[['STRATA_VARIABLE']])))
	tmp.replicated_sample_data <- rbindlist(replicate(tmp.replications, return(sample_data), simplify=FALSE))
	tmp.sample_statistic <- replicate(bootstrap_iterations, eval(parse(text=paste0(summary_statistic, "(tmp.replicated_sample_data[, .SD[sample(.N, ceiling(sample_size*population_strata_proportions[[unlist(.BY)]]))], keyby=STRATA_VARIABLE][['SUMMARY_VARIABLE']], na.rm=TRUE)"))))
	if (return_bootstrap_distribution){
		return(tmp.sample_statistic)
	} else {
		tmp.list <- list(eval(parse(text=paste(summary_statistic, "(tmp.sample_statistic, na.rm=TRUE)"))), sd(tmp.sample_statistic, na.rm=TRUE))
 		names(tmp.list) <- c(toupper(summary_statistic), "SD")
		return(tmp.list)
    }
} ### END bootstrapSRS
