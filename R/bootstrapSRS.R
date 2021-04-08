`bootstrapSRS` <-
function(
        data_table,
        summary_variable,
        strata_variable,
        population_strata_proportions,
        sample_size=NULL,
        bootstrap_iterations=100,
        summary_statistic="mean",
        return_bootstrap_distribution=FALSE) {

	sample_data <- data_table[, c(summary_variable, strata_variable), with=FALSE]
	if (is.null(sample_size)) sample_size=nrow(sample_data)
	stopifnot(length(population_strata_proportions) == uniqueN(sample_data[[strata_variable]]))
	tmp.replications <- ceiling(max((population_strata_proportions*sample_size)/table(sample_data[[strata_variable]])))
    tmp.replicated_sample_data <- rbindlist(replicate(tmp.replications, return(sample_data), simplify=FALSE))
    tmp.sample_statistic <- replicate(bootstrap_iterations, eval(parse(text=paste0(summary_statistic, "(tmp.replicated_sample_data[, .SD[sample(.N, ceiling(sample_size*population_strata_proportions[[unlist(.BY)]]))], keyby=", strata_variable, "][['", summary_variable, "']])"))))
    if (return_bootstrap_distribution){
        return(tmp.sample_statistic)
    } else {
        tmp.list <- list(eval(parse(text=paste(summary_statistic, "(tmp.sample_statistic)"))), sd(tmp.sample_statistic))
        names(tmp.list) <- c(toupper(summary_statistic), "SD")
        return(tmp.list)
    }
} ### END bootstrapSRS
