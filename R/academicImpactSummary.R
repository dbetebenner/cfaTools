`academicImpactSummary` <- function(
    sgp_data,
    state=NULL,
    current_year=NULL,
    prior_year=NULL,
    content_areas=NULL,
    all_grades=NULL,
    sgp_grades=NULL,
    aggregation_group="SCHOOL_NUMBER",
    years_for_aggregates=NULL,
    rtm_adjustment=TRUE) {

    ACHIEVEMENT_LEVEL <- ACHIEVEMENT_LEVEL_PRIOR_2YEAR <- CONTENT_AREA <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP <- NULL
    COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ <- NULL
    COVID_ACADEMIC_IMPACT_SGP_DIFF <- COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ <- COVID_ACADEMIC_IMPACT_SSS_DIFF <- COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ <- NULL
    GES_MEDIAN_SGP <- GES_MEDIAN_SGP_ADJUSTED <- GES_MEDIAN_SSS <- GES_MEDIAN_SSS_ADJUSTED <- GRADE <- ID <- MEAN_SCALE_SCORE_PRIOR_STANDARDIZED <- NULL
    MEAN_SCALE_SCORE_STANDARDIZED <- MEDIAN_SGP <- MEDIAN_SGP_BASELINE <- MEDIAN_SGP_BASELINE_PRIOR <- MEDIAN_SGP_PRIOR_2YEAR <- NULL
    MSGP_BASELINE_DIFFERENCE_ADJUSTED <- MSGP_BASELINE_DIFFERENCE_UNCORRECTED <- MSSS_DIFFERENCE_ADJUSTED <- NULL
    MSSS_DIFFERENCE_UNCORRECTED <- PRIOR_MSGP_CENTERED_2YEAR <- PRIOR_MSSS_CENTERED_2YEAR <- SCALE_SCORE <- NULL
    SCALE_SCORE_PRIOR_STANDARDIZED <- SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR <- SCALE_SCORE_STANDARDIZED <- SGP <- SGP_BASELINE <- VALID_CASE <- YEAR <- NULL

    ### Create state (if NULL) from sgp_object (if possible)
  	if (is.null(state)) {
  		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_data))))
  		state <- SGP::getStateAbbreviation(tmp.name)
  	}


    ### Create sgp_data data set
    if (SGP::is.SGP(sgp_data)) sgp_data <- sgp_data@Data
    if (!"data.table" %in% class(sgp_data)) stop("Please Provide either and SGP object or LONG data")
    setkey(sgp_data, VALID_CASE, CONTENT_AREA, YEAR, ID)
    sgp_data <- na.omit(sgp_data["VALID_CASE"], aggregation_group)


    ### Utility functions
    hdmedian <- function(x, ...) as.numeric(Hmisc::hdquantile(x, probs=0.5, names=FALSE, ...))

    percent_proficient <- function(achievement_level, state_abb=state) {
        achievement.levels <- SGP::SGPstateData[[state_abb]][['Achievement']][['Levels']][['Labels']]
        prof.not.proficient <- SGP::SGPstateData[[state_abb]][['Achievement']][['Levels']][['Proficient']]
        proficient.achievement.levels <- achievement.levels[which(prof.not.proficient=="Proficient")]
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[proficient.achievement.levels], na.rm=TRUE)/sum(tmp.table, na.rm=TRUE)
    }

    Z <- function(data_table, var.to.standardize, reference.year = NULL, rm.na = TRUE) {
      YEAR <- NULL
      x <- data_table[, get(var.to.standardize)]
      if (!is.null(reference.year)){
        y <- data_table[YEAR==reference.year, get(var.to.standardize)]
      } else y <- x
      (x - mean(y, na.rm = rm.na)) / sd(y, na.rm = rm.na)
    }

    scorePercentile <- function(data_table, var.to.percentile, reference.year = NULL, rm.na = TRUE) {
        if (is.null(reference.year)) {
            tmp.percentile.cuts <- quantile(data_table[[var.to.percentile]], probs=seq(0.005, 0.995, length=100), na.rm = rm.na)
        } else {
            tmp.percentile.cuts <- quantile(data_table[YEAR==reference.year][[var.to.percentile]], probs=seq(0.005, 0.995, length=100), na.rm = rm.na)
        }
        if (!all(is.na(tmp.percentile.cuts))) {
          findInterval(data_table[[var.to.percentile]], tmp.percentile.cuts, rightmost.close=TRUE)
        } else rep(as.integer(NA), length(data_table[[var.to.percentile]]))
    }

    ### Calculate parameters from data if not provided
    if (is.null(current_year)) current_year <- tail(sort(unique(sgp_data[['YEAR']])), 1)
    if (is.null(prior_year)) prior_year <- tail(sort(unique(sgp_data[['YEAR']])), 2)[-2]
    if (is.null(content_areas)) content_areas <- unique(sgp_data[['CONTENT_AREA']])
    if (is.null(all_grades)) all_grades <- sort(unique(sgp_data[['GRADE']]))
    if (is.null(sgp_grades)) sgp_grades <- sort(unique(sgp_data[!is.na(SGP_BASELINE), GRADE]))
    if (is.null(years_for_aggregates)) years_for_aggregates <- c(prior_year, current_year)
    if (length(intersect(years_for_aggregates, c(prior_year, current_year)))==0) stop("Argument years_for_aggregates must be one/both of prior_year and/or current_year")
    year_gap <- tail(as.numeric(unlist(strsplit(current_year, split="_"))) - as.numeric(unlist(strsplit(prior_year, split="_"))), 1)

    ### Trim down sgp_data
    sgp_data <- sgp_data[GRADE %in% all_grades & CONTENT_AREA %in% content_areas]

    ##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
    if (!"SCALE_SCORE_STANDARDIZED" %in% names(sgp_data)) {
        sgp_data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = prior_year), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
    }
    if (!"SCALE_SCORE_PERCENTILE" %in% names(sgp_data)) {
        sgp_data[, SCALE_SCORE_PERCENTILE := scorePercentile(.SD, "SCALE_SCORE", reference.year = prior_year), by = list(CONTENT_AREA, GRADE), .SDcols=c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
    }

    ### Create `aggregation_group` Level Summary Table(s)
    grp_status <- sgp_data[
                    VALID_CASE=="VALID_CASE" & GRADE %in% all_grades, .(
                      MEAN_SCALE_SCORE_STANDARDIZED = mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                      MEAN_SCALE_SCORE_PERCENTILE = mean(SCALE_SCORE_PERCENTILE, na.rm=TRUE),
                      MEDIAN_SCALE_SCORE_PERCENTILE = median(as.numeric(SCALE_SCORE_PERCENTILE), na.rm=TRUE),
                      PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_LEVEL),
                      COUNT_SCALE_SCORE=sum(!is.na(SCALE_SCORE_STANDARDIZED))),
                    keyby=c("YEAR", aggregation_group, "CONTENT_AREA")]

    grp_growth <- sgp_data[
                    VALID_CASE=="VALID_CASE" & GRADE %in% sgp_grades, .(
                      MEAN_SGP=mean(SGP, na.rm=TRUE),
                      MEDIAN_SGP=hdmedian(as.numeric(SGP), na.rm=TRUE),
                      MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                      MEDIAN_SGP_BASELINE=hdmedian(as.numeric(SGP_BASELINE), na.rm=TRUE),
                      COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                    keyby=c("YEAR", aggregation_group, "CONTENT_AREA")]

    group_aggregates <- grp_growth[grp_status]

    ###   Expand `group_aggregates` to get all combinations of aggregate levels/rows
    ##    https://stackoverflow.com/questions/43483497/data-table-equivalent-of-tidyrcomplete
    tmp.grp.vars <- c("YEAR", aggregation_group, "CONTENT_AREA")
    group_aggregates <- group_aggregates[do.call(CJ, c(mget(tmp.grp.vars), unique=TRUE)), on=tmp.grp.vars]

    ###   Create LAGGED growth and status variables
    shift.key <- c(aggregation_group, "CONTENT_AREA", "YEAR")
    setkeyv(group_aggregates, shift.key)

    group_aggregates[, c("MEDIAN_SGP_PRIOR_1YEAR", "MEDIAN_SGP_PRIOR_2YEAR") := shift(MEDIAN_SGP, 1:2), by = c(eval(aggregation_group), "CONTENT_AREA")]
    group_aggregates[, MEDIAN_SGP_BASELINE_PRIOR_2YEAR := shift(MEDIAN_SGP_BASELINE, 1), by = c(eval(aggregation_group), "CONTENT_AREA")] # Only getting this for 2021 (1 year shift = 2 years)
    group_aggregates[, c("MEAN_SCALE_SCORE_PRIOR_STANDARDIZED", "MEAN_SCALE_SCORE_PRIOR_2YEAR_STANDARDIZED") := shift(MEAN_SCALE_SCORE_STANDARDIZED, 1:2), by = c(eval(aggregation_group), "CONTENT_AREA")]
    group_aggregates[, c("MEAN_SCALE_SCORE_PRIOR_PERCENTILE", "MEAN_SCALE_SCORE_PRIOR_2YEAR_PERCENTILE") := shift(MEAN_SCALE_SCORE_PERCENTILE, 1:2), by = c(eval(aggregation_group), "CONTENT_AREA")]
    group_aggregates[, c("PERCENT_PROFICIENT_PRIOR", "PERCENT_PROFICIENT_PRIOR_2YEAR") := shift(PERCENT_PROFICIENT, 1:2), by = c(eval(aggregation_group), "CONTENT_AREA")]

    # table(group_aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_2YEAR)])
    # table(group_aggregates[, YEAR, is.na(MEDIAN_SGP_BASELINE_PRIOR_2YEAR)])

    if (year_gap!=1) {
      # group_aggregates[YEAR == current_year, MEDIAN_SGP_PRIOR_2YEAR := MEDIAN_SGP_BASELINE_PRIOR_2YEAR] # Using only BASELINE to BASELINE for 2021
      group_aggregates[YEAR == current_year, MEDIAN_SGP_PRIOR_2YEAR := MEDIAN_SGP_PRIOR_1YEAR]
      group_aggregates[YEAR == current_year, MEDIAN_SGP_PRIOR_1YEAR := NA]
      group_aggregates[YEAR == current_year, MEAN_SCALE_SCORE_PRIOR_2YEAR_STANDARDIZED := MEAN_SCALE_SCORE_PRIOR_STANDARDIZED]
      group_aggregates[YEAR == current_year, MEAN_SCALE_SCORE_PRIOR_2YEAR_PERCENTILE := MEAN_SCALE_SCORE_PRIOR_PERCENTILE]
      group_aggregates[YEAR == current_year, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED := NA]
      group_aggregates[YEAR == current_year, PERCENT_PROFICIENT_PRIOR_2YEAR := PERCENT_PROFICIENT_PRIOR]
      group_aggregates[YEAR == current_year, PERCENT_PROFICIENT_PRIOR := NA]
    }
    group_aggregates <- group_aggregates[YEAR %in% years_for_aggregates]

    ###   Create CENTERED prior 2 year prior variables
    group_aggregates[, PRIOR_MSSS_CENTERED_2YEAR := MEAN_SCALE_SCORE_PRIOR_2YEAR_STANDARDIZED - mean(MEAN_SCALE_SCORE_PRIOR_2YEAR_STANDARDIZED, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
    group_aggregates[, PRIOR_MSSP_CENTERED_2YEAR := MEAN_SCALE_SCORE_PRIOR_2YEAR_PERCENTILE - mean(MEAN_SCALE_SCORE_PRIOR_2YEAR_PERCENTILE, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
    group_aggregates[, PRIOR_MSGP_CENTERED_2YEAR := MEDIAN_SGP_PRIOR_2YEAR - mean(MEDIAN_SGP_PRIOR_2YEAR, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
    # group_aggregates[, as.list(summary(PRIOR_MSGP_CENTERED_2YEAR)), keyby = list(YEAR, CONTENT_AREA)]

    ####################################################################################
    #####
    ###   MEDIAN_SGP_BASELINE
    #####
    ####################################################################################

    ###   Create uncorrected Baseline difference (2021 - 2019)
    group_aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED := MEDIAN_SGP_BASELINE - MEDIAN_SGP_PRIOR_2YEAR]

    if (rtm_adjustment) {
    ###   RTM Adjusted MSGP_BASELINE_DIFFERENCE
    msgp_rtm_models <- list()
    for (CA in content_areas) {
      msgp_rtm_models[[CA]] <- MASS::rlm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }

    ##    Model diagnostics
    # par(mfrow = c(2, 2))
    # for (CA in content_areas) {
    # hist(msgp_rtm_models[[CA]]$residuals, breaks=50)
    # qqnorm(msgp_rtm_models[[CA]]$residuals);qqline(msgp_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MSGP_BASELINE_DIFFERENCE_UNCORRECTED]), msgp_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MEDIAN_SGP_BASELINE]), msgp_rtm_models[[CA]]$fitted.values)
    # }

    ##    Create Adjusted MSGP_BASELINE_DIFFERENCE by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
    group_aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED := as.numeric(NA)]
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, MSGP_BASELINE_DIFFERENCE_ADJUSTED := MSGP_BASELINE_DIFFERENCE_UNCORRECTED - (PRIOR_MSGP_CENTERED_2YEAR*msgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
    }
    } # END  rtm_adjustment

    ##    correlation checks
    # cor(group_aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "ELA", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')

    ##    Create COVID Impact Levels for MSGP Baseline Differences
    group_aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF := fcase(
                        MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                        MSGP_BASELINE_DIFFERENCE_UNCORRECTED < 5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                        MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                        MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -15 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -25, "Large",
                        MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF :=
                        factor(COVID_ACADEMIC_IMPACT_SGP_DIFF, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    if (rtm_adjustment) {
    group_aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ := fcase(
                        MSGP_BASELINE_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                        MSGP_BASELINE_DIFFERENCE_ADJUSTED < 5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                        MSGP_BASELINE_DIFFERENCE_ADJUSTED < -5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                        MSGP_BASELINE_DIFFERENCE_ADJUSTED < -15 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -25, "Large",
                        MSGP_BASELINE_DIFFERENCE_ADJUSTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
    }
    # table(group_aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==current_year, COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==current_year, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)

    #####
    ###   Gamma Effect Size (within School MSGP 2021 - MSGP 2019)
    #####

    ges_data <- copy(sgp_data)
    ges_data[YEAR < prior_year, SGP_BASELINE := SGP]
    ges_sgp_prior <- ges_data[GRADE %in% sgp_grades,
            as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", SGP:::yearIncrement(prior_year, -2), prior_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := prior_year]

    ##    Now replace 2019 BASELINEs with COHORT (only for BASELINES that were present originally)
    ges_data[YEAR == prior_year, SGP_BASELINE := SGP]
    ges_sgp_crnt <- ges_data[GRADE %in% sgp_grades,
            as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior_year, current_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := current_year]

    ges_sgp <- rbindlist(list(ges_sgp_prior, ges_sgp_crnt)); rm(ges_data)

    setnames(ges_sgp, c("Q_50", "V1"), rep("GES_MEDIAN_SGP", 2), skip_absent=TRUE) # Edge cases where first result is NA causes returned var to be named `V1`
    setkeyv(ges_sgp, c(aggregation_group, "YEAR", "CONTENT_AREA"))
    setkeyv(group_aggregates, c(aggregation_group, "YEAR", "CONTENT_AREA"))

    ##    Merge in GES with other summary statistics
    group_aggregates <- ges_sgp[group_aggregates]

    if (rtm_adjustment) {
    ###   RTM Adjusted G.E.S.
    gessgp_rtm_models <- list()
    for (CA in content_areas) {
      gessgp_rtm_models[[CA]] <- MASS::rlm(GES_MEDIAN_SGP ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }

    ##    Model diagnostics
    # par(mfrow = c(2, 2))
    # for (CA in content_areas) {
    # hist(gessgp_rtm_models[[CA]]$residuals, breaks=50)
    # qqnorm(gessgp_rtm_models[[CA]]$residuals);qqline(gessgp_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$fitted.values)
    # }

    ##    Create Adjusted GES_MEDIAN_SGP by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
    group_aggregates[, GES_MEDIAN_SGP_ADJUSTED := as.numeric(NA)]
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*gessgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
    }
    }  #  END rtm_adjustment

    ##    Visualization, summary and correlation checks
    # na.omit(group_aggregates[, as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP, PRIOR_MSGP_CENTERED_2YEAR])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, PRIOR_MSGP_CENTERED_2YEAR])
    # cor(group_aggregates[, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
    #
    # cor(group_aggregates[, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "ELA", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
    #
    # ges_2019_ela_adj <- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == "ELA"]); summary(ges_2019_ela_adj)
    # ges_2019_math_adj<- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_2019_math_adj)

    ##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP := fcase(
                        GES_MEDIAN_SGP >= 0.2, "Improvement",
                        GES_MEDIAN_SGP <  0.2 & GES_MEDIAN_SGP >= -0.2, "Modest to None",
                        GES_MEDIAN_SGP < -0.2 & GES_MEDIAN_SGP >= -0.5, "Moderate",
                        GES_MEDIAN_SGP < -0.5 & GES_MEDIAN_SGP >= -0.8, "Large",
                        GES_MEDIAN_SGP < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    if (rtm_adjustment) {
    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ := fcase(
                        GES_MEDIAN_SGP_ADJUSTED >= 0.2, "Improvement",
                        GES_MEDIAN_SGP_ADJUSTED <  0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.2, "Modest to None",
                        GES_MEDIAN_SGP_ADJUSTED < -0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.5, "Moderate",
                        GES_MEDIAN_SGP_ADJUSTED < -0.5 & GES_MEDIAN_SGP_ADJUSTED >= -0.8, "Large",
                        GES_MEDIAN_SGP_ADJUSTED < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
    }
    # table(group_aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==current_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)
    # table(group_aggregates[YEAR==current_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)

    ####################################################################################
    #####
    ###   MEAN_SCALE_SCORE_STANDARDIZED
    #####
    ####################################################################################

    ###   Create uncorrected mean scale score difference (2021 - 2019)
    group_aggregates[, MSSS_DIFFERENCE_UNCORRECTED := MEAN_SCALE_SCORE_STANDARDIZED - MEAN_SCALE_SCORE_PRIOR_2YEAR_STANDARDIZED]
    group_aggregates[, MSSP_DIFFERENCE_UNCORRECTED := MEAN_SCALE_SCORE_PERCENTILE - MEAN_SCALE_SCORE_PRIOR_2YEAR_PERCENTILE]

    if (rtm_adjustment) {
    ###   RTM Adjusted MSSS_DIFFERENCE
    msss_rtm_models <- mssp_rtm_models <- list()
    for (CA in content_areas) {
      msss_rtm_models[[CA]] <- MASS::rlm(MSSS_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }
    for (CA in content_areas) {
      mssp_rtm_models[[CA]] <- MASS::rlm(MSSP_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSSP_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }

    ##    Model diagnostics
    # par(mfrow = c(2, 2))
    # for (CA in content_areas) {
    # hist(msss_rtm_models[[CA]]$residuals, breaks=50)
    # qqnorm(msss_rtm_models[[CA]]$residuals);qqline(msss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MSSS_DIFFERENCE_UNCORRECTED]), msss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MEAN_SCALE_SCORE_STANDARDIZED]), msss_rtm_models[[CA]]$fitted.values)
    # }

    ##    Create Adjusted MSSS_DIFFERENCE_ by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
    group_aggregates[, MSSS_DIFFERENCE_ADJUSTED := as.numeric(NA)][, MSSP_DIFFERENCE_ADJUSTED := as.numeric(NA)]
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, MSSS_DIFFERENCE_ADJUSTED := MSSS_DIFFERENCE_UNCORRECTED - (PRIOR_MSSS_CENTERED_2YEAR*msss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
    }
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, MSSP_DIFFERENCE_ADJUSTED := MSSP_DIFFERENCE_UNCORRECTED - (PRIOR_MSSP_CENTERED_2YEAR*mssp_rtm_models[[CA]]$coef[["PRIOR_MSSP_CENTERED_2YEAR"]])]
    }
    }  #  END rtm_adjustment
    ##    correlation checks
    # cor(group_aggregates[, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "ELA", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')


    #####
    ###   Gamma Effect Size (within aggregation_group MSSS 2021 - MSSS 2019)
    #####

    ###   Create uncorrected G.E.S.
    ges_sss <- rbindlist(list(
        sgp_data[,
            as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", SGP:::yearIncrement(prior_year, -2), prior_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := prior_year],
        sgp_data[,
            as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", prior_year, current_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := current_year]))

    setnames(ges_sss, c("Q_50", "V1"), rep("GES_MEDIAN_SSS", 2), skip_absent=TRUE)
    setkeyv(ges_sss, c(aggregation_group, "YEAR", "CONTENT_AREA"))
    setkeyv(group_aggregates, c(aggregation_group, "YEAR", "CONTENT_AREA"))

    ##    Merge in GES with other summary statistics
    group_aggregates <- ges_sss[group_aggregates]

    if (rtm_adjustment) {
    ###   RTM Adjusted G.E.S.
    gesss_rtm_models <- list()
    for (CA in content_areas) {
      gesss_rtm_models[[CA]] <- MASS::rlm(GES_MEDIAN_SSS ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }

    ##    Model diagnostics
    # par(mfrow = c(2, 2))
    # for (CA in content_areas) {
    # hist(gesss_rtm_models[[CA]]$residuals, breaks=50)
    # qqnorm(gesss_rtm_models[[CA]]$residuals);qqline(gesss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$fitted.values)
    # }

    ##    Create Adjusted GES_MEDIAN_SSS by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
    group_aggregates[, GES_MEDIAN_SSS_ADJUSTED := as.numeric(NA)]
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, GES_MEDIAN_SSS_ADJUSTED := GES_MEDIAN_SSS - (PRIOR_MSSS_CENTERED_2YEAR*gesss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
    }
    }  #  END rtm_adjustment

    #####
    ###   Gamma Effect Size (within aggregation_group MSSP 2021 - MSSP 2019)
    #####

    ###   Create uncorrected G.E.S.
    ges_ssp <- rbindlist(list(
        sgp_data[,
            as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_PERCENTILE", SGP:::yearIncrement(prior_year, -2), prior_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := prior_year],
        sgp_data[,
            as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_PERCENTILE", prior_year, current_year, quantiles=c(0.5), digits=2)),
          keyby=c("CONTENT_AREA", aggregation_group)][, YEAR := current_year]))

    setnames(ges_ssp, c("Q_50", "V1"), rep("GES_MEDIAN_SSP", 2), skip_absent=TRUE)
    setkeyv(ges_ssp, c(aggregation_group, "YEAR", "CONTENT_AREA"))
    setkeyv(group_aggregates, c(aggregation_group, "YEAR", "CONTENT_AREA"))

    ##    Merge in GES with other summary statistics
    group_aggregates <- ges_ssp[group_aggregates]

    if (rtm_adjustment) {
    ###   RTM Adjusted G.E.S.
    gessp_rtm_models <- list()
    for (CA in content_areas) {
      gessp_rtm_models[[CA]] <- MASS::rlm(GES_MEDIAN_SSP ~ 0 + PRIOR_MSSP_CENTERED_2YEAR, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == CA])
    }

    ##    Model diagnostics
    # par(mfrow = c(2, 2))
    # for (CA in content_areas) {
    # hist(gesss_rtm_models[[CA]]$residuals, breaks=50)
    # qqnorm(gesss_rtm_models[[CA]]$residuals);qqline(gesss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$residuals)
    # plot(na.omit(group_aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$fitted.values)
    # }

    ##    Create Adjusted GES_MEDIAN_SSS by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
    group_aggregates[, GES_MEDIAN_SSP_ADJUSTED := as.numeric(NA)]
    for (CA in content_areas) {
      group_aggregates[CONTENT_AREA == CA, GES_MEDIAN_SSP_ADJUSTED := GES_MEDIAN_SSP - (PRIOR_MSSP_CENTERED_2YEAR*gessp_rtm_models[[CA]]$coef[["PRIOR_MSSP_CENTERED_2YEAR"]])]
    }
    }  #  END rtm_adjustment

    ##    Visualization, summary and correlation checks
    # na.omit(group_aggregates[, as.list(round(summary(GES_MEDIAN_SSS_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS, PRIOR_MSSS_CENTERED_2YEAR])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
    # plot(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, PRIOR_MSSS_CENTERED_2YEAR])
    # cor(group_aggregates[, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')

    # cor(group_aggregates[, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "ELA", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
    # cor(group_aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')

    # ges_ss_19_ela_adj <- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == "ELA"]); summary(ges_ss_19_ela_adj)
    # ges_ss_19_math_adj<- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=group_aggregates[YEAR == prior_year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_ss_19_math_adj)


    ##    Create COVID Impact Levels for MSGP Baseline Differences
    group_aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF := fcase(
                        MSSS_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                        MSSS_DIFFERENCE_UNCORRECTED < 5 & MSSS_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                        MSSS_DIFFERENCE_UNCORRECTED < -5 & MSSS_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                        MSSS_DIFFERENCE_UNCORRECTED < -15 & MSSS_DIFFERENCE_UNCORRECTED >= -25, "Large",
                        MSSS_DIFFERENCE_UNCORRECTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF :=
                        factor(COVID_ACADEMIC_IMPACT_SSS_DIFF, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSP_DIFF := fcase(
                        MSSP_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                        MSSP_DIFFERENCE_UNCORRECTED < 5 & MSSP_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                        MSSP_DIFFERENCE_UNCORRECTED < -5 & MSSP_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                        MSSP_DIFFERENCE_UNCORRECTED < -15 & MSSP_DIFFERENCE_UNCORRECTED >= -25, "Large",
                        MSSP_DIFFERENCE_UNCORRECTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSP_DIFF :=
                        factor(COVID_ACADEMIC_IMPACT_SSP_DIFF, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    if (rtm_adjustment) {
    group_aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ := fcase(
                        MSSS_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                        MSSS_DIFFERENCE_ADJUSTED < 5 & MSSS_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                        MSSS_DIFFERENCE_ADJUSTED < -5 & MSSS_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                        MSSS_DIFFERENCE_ADJUSTED < -15 & MSSS_DIFFERENCE_ADJUSTED >= -25, "Large",
                        MSSS_DIFFERENCE_ADJUSTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSP_DIFF_ADJ := fcase(
                        MSSP_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                        MSSP_DIFFERENCE_ADJUSTED < 5 & MSSP_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                        MSSP_DIFFERENCE_ADJUSTED < -5 & MSSP_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                        MSSP_DIFFERENCE_ADJUSTED < -15 & MSSP_DIFFERENCE_ADJUSTED >= -25, "Large",
                        MSSP_DIFFERENCE_ADJUSTED < -25, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_SSP_DIFF_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_SSP_DIFF_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
    }

    ##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS := fcase(
                        GES_MEDIAN_SSS >= 0.2, "Improvement",
                        GES_MEDIAN_SSS <  0.2 & GES_MEDIAN_SSS >= -0.2, "Modest to None",
                        GES_MEDIAN_SSS < -0.2 & GES_MEDIAN_SSS >= -0.5, "Moderate",
                        GES_MEDIAN_SSS < -0.5 & GES_MEDIAN_SSS >= -0.8, "Large",
                        GES_MEDIAN_SSS < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSP := fcase(
                        GES_MEDIAN_SSP >= 0.2, "Improvement",
                        GES_MEDIAN_SSP <  0.2 & GES_MEDIAN_SSP >= -0.2, "Modest to None",
                        GES_MEDIAN_SSP < -0.2 & GES_MEDIAN_SSP >= -0.5, "Moderate",
                        GES_MEDIAN_SSP < -0.5 & GES_MEDIAN_SSP >= -0.8, "Large",
                        GES_MEDIAN_SSP < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    if (rtm_adjustment) {
    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ := fcase(
                        GES_MEDIAN_SSS_ADJUSTED >= 0.2, "Improvement",
                        GES_MEDIAN_SSS_ADJUSTED <  0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.2, "Modest to None",
                        GES_MEDIAN_SSS_ADJUSTED < -0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.5, "Moderate",
                        GES_MEDIAN_SSS_ADJUSTED < -0.5 & GES_MEDIAN_SSS_ADJUSTED >= -0.8, "Large",
                        GES_MEDIAN_SSS_ADJUSTED < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSP_ADJ := fcase(
                        GES_MEDIAN_SSP_ADJUSTED >= 0.2, "Improvement",
                        GES_MEDIAN_SSP_ADJUSTED <  0.2 & GES_MEDIAN_SSP_ADJUSTED >= -0.2, "Modest to None",
                        GES_MEDIAN_SSP_ADJUSTED < -0.2 & GES_MEDIAN_SSP_ADJUSTED >= -0.5, "Moderate",
                        GES_MEDIAN_SSP_ADJUSTED < -0.5 & GES_MEDIAN_SSP_ADJUSTED >= -0.8, "Large",
                        GES_MEDIAN_SSP_ADJUSTED < -0.8, "Severe")]

    group_aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSP_ADJ :=
                        factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSP_ADJ, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    ###   Save RTM Models as `attributes` meta-data
    setattr(group_aggregates, "msgp_rtm_models", lapply(msgp_rtm_models, summary))
    setattr(group_aggregates, "gessgp_rtm_models", lapply(gessgp_rtm_models, summary))
    setattr(group_aggregates, "msss_rtm_models", lapply(msss_rtm_models, summary))
    setattr(group_aggregates, "gesss_rtm_models", lapply(gesss_rtm_models, summary))
    setattr(group_aggregates, "mssp_rtm_models", lapply(mssp_rtm_models, summary))
    setattr(group_aggregates, "gessp_rtm_models", lapply(gessp_rtm_models, summary))
    }

    tmp.list <- list(TEMP=group_aggregates)
    if (is.null(aggregation_group)) aggregation_group <- "CONTENT_AREA"
    names(tmp.list) <- paste(aggregation_group, collapse="_by_")
    return(tmp.list)
} ### END academicImpactSummary
