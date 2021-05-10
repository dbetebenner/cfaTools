`bootstrapSRS_SGP` <-
function(sgp_object,
        strata_summaries=c("STATUS", "GROWTH"),
        strata_variables="SCALE_SCORE_DECILE",
        strata_proportions_years_status,
        strata_proportions_years_growth,
        summary_years,
        create_scale_score_deciles=TRUE,
        sample_size=NULL,
        bootstrap_iterations=100,
        summary_statistic="mean",
        aggregation_group=c("CONTENT_AREA", "GRADE")) {

    CONTENT_AREA <- GRADE <- GROUP_TOTAL <- PROPORTION <- SCALE_SCORE <- SCALE_SCORE_DECILE <- SCALE_SCORE_PRIOR <- SCALE_SCORE_PRIOR_DECILE <- NULL
    SGP <- STRATA_VARIABLE <- TOTAL <- VALID_CASE <- YEAR <- MEAN_SCALE_SCORE_INFERRED_SE <- MEAN_SGP_INFERRED_SE <- SGP_BASELINE <- MEAN_SGP_BASELINE_INFERRED_SE <- NULL
    status_summaries <- growth_summaries <- NULL


    ### Utility functions
    getStrataProportions <- function(tmp.dt) {
        tmp.proportions <- tmp.dt[['PROPORTION']]
        names(tmp.proportions) <- tmp.dt[['STRATA_VARIABLE']]
        return(tmp.proportions)
    }

    ### copy data from object
    if ("SGP" %in% class(sgp_object)) sgp_object_data <- copy(sgp_object@Data)
    if ("data.table" %in% class(sgp_object)) sgp_object_data <- sgp_object

    ### Create prior/current achievement deciles if requested:
    if (create_scale_score_deciles) {
        sgp_object_data[!is.na(SCALE_SCORE_PRIOR), SCALE_SCORE_PRIOR_DECILE:=cut(SCALE_SCORE_PRIOR, breaks= quantile(SCALE_SCORE_PRIOR, probs= seq(0, 1, by= 0.1)), include.lowest=TRUE, labels=paste("Decile", c(1:10))), by=aggregation_group]
        sgp_object_data[!is.na(SCALE_SCORE), SCALE_SCORE_DECILE:=cut(SCALE_SCORE, breaks= quantile(SCALE_SCORE, probs= seq(0, 1, by= 0.1)), include.lowest=TRUE, labels=paste("Decile", c(1:10))), by=aggregation_group]
    }


    ### STATUS summaries
    if ("STATUS" %in% strata_summaries) {

        sgp_object_data <- na.omit(sgp_object_data, cols=strata_variables)
        eval(parse(text=paste("sgp_object_data[,STRATA_VARIABLE:=paste(", paste(strata_variables, collapse=", "), ")]")))

        ### Split up data into parts for proprotions and parts for summarizing (may be the same set)
        data_for_proportions <- sgp_object_data[VALID_CASE=="VALID_CASE" & YEAR %in% strata_proportions_years_status]
        data_for_summaries <- sgp_object_data[VALID_CASE=="VALID_CASE" & YEAR %in% summary_years]

        ### Create proportions
        dt.proportions <- data_for_proportions[!is.na(STRATA_VARIABLE)][, TOTAL:=.N, by=aggregation_group][, GROUP_TOTAL:=.N, by=c(aggregation_group, "STRATA_VARIABLE")][,PROPORTION:=GROUP_TOTAL/TOTAL][,TOTAL:=NULL][,GROUP_TOTAL:=NULL]
        dt.proportions.unique <- unique(dt.proportions, by=c(aggregation_group, "STRATA_VARIABLE"))[,c(aggregation_group, "STRATA_VARIABLE", "PROPORTION"), with=FALSE]
        setkeyv(dt.proportions.unique, c(aggregation_group, "STRATA_VARIABLE"))


        ### Create and add in bootstrapSRS value

        summaries.1 <- data_for_summaries[,list(MEAN_SCALE_SCORE=mean(SCALE_SCORE, na.rm=TRUE)), keyby=aggregation_group]
        tmp.summaries.2 <- data_for_summaries[,bootstrapSRS(SCALE_SCORE, STRATA_VARIABLE, getStrataProportions(dt.proportions.unique[.BY]), sample_size=sample_size, bootstrap_iterations=bootstrap_iterations, summary_statistic=summary_statistic), keyby=aggregation_group]
        summaries.2 <- unique(tmp.summaries.2, by=aggregation_group)[,MEAN_SCALE_SCORE_INFERRED_SE:=tmp.summaries.2[seq_len(nrow(tmp.summaries.2)) %% 2==0]$V1]
        setnames(summaries.2, "V1", "MEAN_SCALE_SCORE_INFERRED")

       status_summaries <- summaries.1[summaries.2]
   } ### END STATUS


   ### GROWTH summaries
   if ("GROWTH" %in% strata_summaries) {

       strata_variables_growth <- strata_variables
       if ("SCALE_SCORE_DECILE" %in% strata_variables) {
           strata_variables_growth <- sub("SCALE_SCORE_DECILE", "SCALE_SCORE_PRIOR_DECILE", strata_variables)
       }

        sgp_object_data <- na.omit(sgp_object_data, cols=strata_variables)
        eval(parse(text=paste("sgp_object_data[,STRATA_VARIABLE:=paste(", paste(strata_variables, collapse=", "), ")]")))

        ### Split up data into parts for proprotions and parts for summarizing (may be the same set)
        data_for_proportions <- sgp_object_data[VALID_CASE=="VALID_CASE" & YEAR %in% strata_proportions_years_growth]
        data_for_summaries <- sgp_object_data[VALID_CASE=="VALID_CASE" & YEAR %in% summary_years]

        ### Create proportions
        dt.proportions <- data_for_proportions[!is.na(STRATA_VARIABLE)][, TOTAL:=.N, by=aggregation_group][, GROUP_TOTAL:=.N, by=c(aggregation_group, "STRATA_VARIABLE")][,PROPORTION:=GROUP_TOTAL/TOTAL][,TOTAL:=NULL][,GROUP_TOTAL:=NULL]
        dt.proportions.unique <- unique(dt.proportions, by=c(aggregation_group, "STRATA_VARIABLE"))[,c(aggregation_group, "STRATA_VARIABLE", "PROPORTION"), with=FALSE]
        setkeyv(dt.proportions.unique, c(aggregation_group, "STRATA_VARIABLE"))

        ### Create and add in bootstrapSRS value
        summaries.1 <- data_for_summaries[,list(MEAN_SGP=mean(SGP, na.rm=TRUE)), keyby=aggregation_group]
        summaries.2 <- data_for_summaries[,list(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE)), keyby=aggregation_group]
        tmp.summaries.3 <- data_for_summaries[,bootstrapSRS(SGP, STRATA_VARIABLE, getStrataProportions(dt.proportions.unique[.BY]), sample_size=sample_size, bootstrap_iterations=bootstrap_iterations, summary_statistic=summary_statistic), keyby=aggregation_group]
        summaries.3 <- unique(tmp.summaries.3, by=aggregation_group)[,MEAN_SGP_INFERRED_SE:=tmp.summaries.3[seq_len(nrow(tmp.summaries.3)) %% 2==0]$V1]
        tmp.summaries.4 <- data_for_summaries[,bootstrapSRS(SGP_BASELINE, STRATA_VARIABLE, getStrataProportions(dt.proportions.unique[.BY]), sample_size=sample_size, bootstrap_iterations=bootstrap_iterations, summary_statistic=summary_statistic), keyby=aggregation_group]
        summaries.4 <- unique(tmp.summaries.4, by=aggregation_group)[,MEAN_SGP_BASELINE_INFERRED_SE:=tmp.summaries.4[seq_len(nrow(tmp.summaries.4)) %% 2==0]$V1]
        setnames(summaries.3, "V1", "MEAN_SGP_INFERRED")
        setnames(summaries.4, "V1", "MEAN_SGP_BASELINE_INFERRED")

       growth_summaries <- summaries.1[summaries.2][summaries.3][summaries.4]
   } ### END GROWTH

   if (is.null(status_summaries) & !is.null(growth_summaries)) return(growth_summaries)
   if (!is.null(status_summaries) & is.null(growth_summaries)) return(status_summaries)
   if (!is.null(status_summaries) & !is.null(growth_summaries)) return(status_summaries[growth_summaries])

} ### END bootstrapSRS_SGP
