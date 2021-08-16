`academicImpactSummary` <- function(
    sgp_data,
    state=NULL,
    current_year=NULL,
    prior_year=NULL,
    content_areas=NULL,
    grades=NULL) {

    ACHIEVEMENT_LEVEL <- ACHIEVEMENT_LEVEL_PRIOR_2YEAR <- CONTENT_AREA <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP <- NULL
    COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS <- COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ <- NULL
    COVID_ACADEMIC_IMPACT_SGP_DIFF <- COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ <- COVID_ACADEMIC_IMPACT_SSS_DIFF <- COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ <- NULL
    GES_MEDIAN_SGP <- GES_MEDIAN_SGP_ADJUSTED <- GES_MEDIAN_SSS <- GES_MEDIAN_SSS_ADJUSTED <- GRADE <- ID <- MEAN_SCALE_SCORE_PRIOR_STANDARDIZED <- NULL
    MEAN_SCALE_SCORE_STANDARDIZED <- MEDIAN_SGP <- MEDIAN_SGP_BASELINE <- MEDIAN_SGP_BASELINE_PRIOR <- MEDIAN_SGP_PRIOR_2YEAR <- NULL
    MEDIAN_SGP_PRIOR_3YEAR <- MSGP_BASELINE_DIFFERENCE_ADJUSTED <- MSGP_BASELINE_DIFFERENCE_UNCORRECTED <- MSSS_DIFFERENCE_ADJUSTED <- NULL
    MSSS_DIFFERENCE_UNCORRECTED <- PRIOR_MSGP_CENTERED_2YEAR <- PRIOR_MSGP_CENTERED_3YEAR <- PRIOR_MSSS_CENTERED_2YEAR <- SCALE_SCORE <- NULL
    SCALE_SCORE_PRIOR_STANDARDIZED <- SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR <- SCALE_SCORE_STANDARDIZED <- SCHOOL_NUMBER <- SGP <- SGP_BASELINE <- VALID_CASE <- YEAR <- NULL

    ### Create state (if NULL) from sgp_object (if possible)
	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- SGP::getStateAbbreviation(tmp.name)
	}

    ### Create sgp_data data set
    if (SGP::is.SGP(sgp_data)) sgp_data <- copy(sgp_data@Data)
    if (!"data.table" %in% class(sgp_data)) stop("Please Provide either and SGP object or LONG data")
    setkey(sgp_data, VALID_CASE, CONTENT_AREA, YEAR, ID)

    ### Utility functions
    hdmedian <- function(x, ...) as.numeric(Hmisc::hdquantile(x, probs=0.5, names=FALSE, ...))

    percent_proficient <- function(achievement_level, state_abb=state) {
        achievement.levels <- SGP::SGPstateData[[state_abb]][['Achievement']][['Levels']][['Labels']]
        prof.not.proficient <- SGP::SGPstateData[[state_abb]][['Achievement']][['Levels']][['Proficient']]
        proficient.achievement.levels <- achievement.levels[which(prof.not.proficient=="Proficient")]
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[proficient.achievement.levels])/sum(tmp.table)
    }


    ### Calculate parameters from data if not provided
    if (is.null(current_year)) current_year <- tail(sort(unique(sgp_data[['YEAR']])), 1)
    if (is.null(prior_year)) prior_year <- tail(sort(unique(sgp_data[['YEAR']])), 2)[-2]
    if (is.null(content_areas)) content_areas <- unique(sgp_data[['CONTENT_AREA']])
    if (is.null(grades)) grades <- unique(sgp_data[['GRADE']])

    ### Create additional variables necessary for aggregates
    if (!"SCALE_SCORE_STANDARDIZED" %in% names(sgp_data)) sgp_data[,SCALE_SCORE_STANDARDIZED:=as.numeric(scale(SCALE_SCORE)), keyby=c("CONTENT_AREA", "YEAR", "GRADE")]
    shift.key <- c("ID", "CONTENT_AREA", "YEAR")
    setkeyv(sgp_data, shift.key)
    sgp_data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR", "ACHIEVEMENT_LEVEL_PRIOR_2YEAR") := shift(list(SCALE_SCORE_STANDARDIZED, ACHIEVEMENT_LEVEL), 2), by = list(ID, CONTENT_AREA)]

    ### Create School Level Summary Table
    school.aggregates <- sgp_data[
                        VALID_CASE=="VALID_CASE" & GRADE %in% grades,
                          list(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                            MEDIAN_SGP_BASELINE=hdmedian(as.numeric(SGP_BASELINE), na.rm=TRUE),
                            MEAN_SGP=mean(SGP, na.rm=TRUE),
                            MEDIAN_SGP=hdmedian(as.numeric(SGP), na.rm=TRUE),
                            MEAN_SCALE_SCORE_STANDARDIZED = mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                            MEAN_SCALE_SCORE_PRIOR_STANDARDIZED=mean(SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR, na.rm=TRUE),
                            PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_LEVEL),
                            PERCENT_PROFICIENT_PRIOR=percent_proficient(ACHIEVEMENT_LEVEL_PRIOR_2YEAR),
                            COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                          keyby=c("YEAR", "SCHOOL_NUMBER", "CONTENT_AREA")]

    shift.key <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
    setkeyv(school.aggregates, shift.key)

    school.aggregates[, c("MEDIAN_SGP_PRIOR_2YEAR", "MEDIAN_SGP_PRIOR_3YEAR") := shift(MEDIAN_SGP, 2:3), by = list(SCHOOL_NUMBER, CONTENT_AREA)]
    school.aggregates[, MEDIAN_SGP_BASELINE_PRIOR := shift(MEDIAN_SGP_BASELINE, 1), by = list(SCHOOL_NUMBER, CONTENT_AREA)] # Only getting this for 2021 (1 year shift = 2 years)

# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_2YEAR)])
# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_3YEAR)])
# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_BASELINE_PRIOR)])

    school.aggregates[YEAR == current_year, MEDIAN_SGP_PRIOR_3YEAR := MEDIAN_SGP_PRIOR_2YEAR]
    school.aggregates[YEAR == current_year, MEDIAN_SGP_PRIOR_2YEAR := MEDIAN_SGP_BASELINE_PRIOR]

    school.aggregates[, PRIOR_MSGP_CENTERED_2YEAR := MEDIAN_SGP_PRIOR_2YEAR - mean(MEDIAN_SGP_PRIOR_2YEAR, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
    school.aggregates[, PRIOR_MSGP_CENTERED_3YEAR := MEDIAN_SGP_PRIOR_3YEAR - mean(MEDIAN_SGP_PRIOR_3YEAR, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
    school.aggregates <- school.aggregates[YEAR %in% c(prior_year, current_year)]
    school.aggregates[, as.list(summary(PRIOR_MSGP_CENTERED_2YEAR)), keyby = list(YEAR, CONTENT_AREA)]
#   table(school.aggregates[, is.na(PRIOR_MSGP_CENTERED_2YEAR), is.na(PRIOR_MSGP_CENTERED_3YEAR)], exclude=NULL)

###   Create uncorrected Baseline difference (2021 - 2019)
school.aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED := MEDIAN_SGP_BASELINE - MEDIAN_SGP_PRIOR_2YEAR]

###   RTM Adjusted MSGP_BASELINE_DIFFERENCE
msgp_rtm_models <- list()
for (CA in content_areas) {
  msgp_rtm_models[[CA]] <- MASS::rlm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == CA])
}

##    Model diagnostics
# boot.msgp.rtm <- function(data, indices, maxit=20){
#   data <- data[indices, ] # select obs. in bootstrap sample
#   # mod <- lm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=data)
#   mod <- MASS::rlm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=data, maxit=maxit)
#   coef(mod) # return coefficient vector
# }
#
# set.seed(12345) # for reproducibility
# boot_robust_rtm <- boot(data=school.aggregates[YEAR == prior_year & CONTENT_AREA == CA], statistic=boot.msgp.rtm,
#                         R=2000, maxit=200, parallel = "multicore", ncpus = 10)
#
# par(mfrow = c(2, 2))
# for (CA in content_areas) {
# hist(msgp_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(msgp_rtm_models[[CA]]$residuals);qqline(msgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MSGP_BASELINE_DIFFERENCE_UNCORRECTED]), msgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MEDIAN_SGP_BASELINE]), msgp_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted MSGP_BASELINE_DIFFERENCE by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED := as.numeric(NA)]
for (CA in content_areas) {
  school.aggregates[CONTENT_AREA == CA, MSGP_BASELINE_DIFFERENCE_ADJUSTED := MSGP_BASELINE_DIFFERENCE_UNCORRECTED - (PRIOR_MSGP_CENTERED_2YEAR*msgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
}

##    correlation checks
# cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "ELA", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')

##    Create COVID Impact Levels for MSGP Baseline Differences
school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF := fcase(
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < 5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -15 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -25, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ := fcase(
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < 5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -15 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -25, "Severe")]

# table(school.aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)

#####
###   Gamma Effect Size (within School MSGP 2021 - MSGP 2019)
#####

###   Create uncorrected G.E.S.
ges_sgp <- rbindlist(list(
    sgp_data[,
        as.list(gammaEffectSizeLong(.SD, "SGP", SGP:::yearIncrement(prior_year, -2), prior_year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := prior_year],
    sgp_data[,
        as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior_year, current_year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := current_year]))

setnames(ges_sgp, "Q_50", "GES_MEDIAN_SGP")
setkey(ges_sgp, SCHOOL_NUMBER, YEAR, CONTENT_AREA)
setkey(school.aggregates, SCHOOL_NUMBER, YEAR, CONTENT_AREA)

##    Merge in GES with other summary statistics
school.aggregates <- ges_sgp[school.aggregates]

###   RTM Adjusted G.E.S.
gessgp_rtm_models <- list()
for (CA in content_areas) {
  gessgp_rtm_models[[CA]] <- MASS::rlm(GES_MEDIAN_SGP ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content_areas) {
# hist(gessgp_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(gessgp_rtm_models[[CA]]$residuals);qqline(gessgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted GES_MEDIAN_SGP by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, GES_MEDIAN_SGP_ADJUSTED := as.numeric(NA)]
for (CA in content_areas) {
  school.aggregates[CONTENT_AREA == CA, GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*gessgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
}

##    Visualization, summary and correlation checks
# na.omit(school.aggregates[, as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP, PRIOR_MSGP_CENTERED_2YEAR])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, PRIOR_MSGP_CENTERED_2YEAR])
# cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
#
# cor(school.aggregates[, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "ELA", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
#
# ges_2019_ela_adj <- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == "ELA"]); summary(ges_2019_ela_adj)
# ges_2019_math_adj<- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_2019_math_adj)

##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP := fcase(
                    GES_MEDIAN_SGP >= 0.2, "Improvement",
                    GES_MEDIAN_SGP <  0.2 & GES_MEDIAN_SGP >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP < -0.2 & GES_MEDIAN_SGP >= -0.5, "Moderate",
                    GES_MEDIAN_SGP < -0.5 & GES_MEDIAN_SGP >= -0.8, "Large",
                    GES_MEDIAN_SGP < -0.8, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ := fcase(
                    GES_MEDIAN_SGP_ADJUSTED >= 0.2, "Improvement",
                    GES_MEDIAN_SGP_ADJUSTED <  0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP_ADJUSTED < -0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.5, "Moderate",
                    GES_MEDIAN_SGP_ADJUSTED < -0.5 & GES_MEDIAN_SGP_ADJUSTED >= -0.8, "Large",
                    GES_MEDIAN_SGP_ADJUSTED < -0.8, "Severe")]

# table(school.aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR==prior_year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)


#####
###   MEAN_SCALE_SCORE_STANDARDIZED
#####

school.aggregates[, PRIOR_MSSS_CENTERED_2YEAR := MEAN_SCALE_SCORE_PRIOR_STANDARDIZED - mean(MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
# school.aggregates[, as.list(summary(PRIOR_MSSS_CENTERED_2YEAR)), keyby = list(YEAR, CONTENT_AREA)]

###   Create uncorrected mean scale score difference (2021 - 2019)
school.aggregates[, MSSS_DIFFERENCE_UNCORRECTED := MEAN_SCALE_SCORE_STANDARDIZED - MEAN_SCALE_SCORE_PRIOR_STANDARDIZED]

###   RTM Adjusted MSSS_DIFFERENCE
msss_rtm_models <- list()
for (CA in content_areas) {
  msss_rtm_models[[CA]] <- MASS::rlm(MSSS_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content_areas) {
# hist(msss_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(msss_rtm_models[[CA]]$residuals);qqline(msss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MSSS_DIFFERENCE_UNCORRECTED]), msss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MEAN_SCALE_SCORE_STANDARDIZED]), msss_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted MSSS_DIFFERENCE_ by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
school.aggregates[, MSSS_DIFFERENCE_ADJUSTED := as.numeric(NA)]
for (CA in content_areas) {
  school.aggregates[CONTENT_AREA == CA, MSSS_DIFFERENCE_ADJUSTED := MSSS_DIFFERENCE_UNCORRECTED - (PRIOR_MSSS_CENTERED_2YEAR*msss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
}

##    correlation checks
# cor(school.aggregates[, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "ELA", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')


#####
###   Gamma Effect Size (within School MSSS 2021 - MSSS 2019)
#####

###   Create uncorrected G.E.S.
ges_sss <- rbindlist(list(
    sgp_data[,
        as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", SGP:::yearIncrement(prior_year, -2), prior_year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := prior_year],
    sgp_data[,
        as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", prior_year, current_year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := current_year]))

setnames(ges_sss, "Q_50", "GES_MEDIAN_SSS")
setkey(ges_sss, SCHOOL_NUMBER, YEAR, CONTENT_AREA)
setkey(school.aggregates, SCHOOL_NUMBER, YEAR, CONTENT_AREA)

##    Merge in GES with other summary statistics
school.aggregates <- ges_sss[school.aggregates]

###   RTM Adjusted G.E.S.
gesss_rtm_models <- list()
for (CA in content_areas) {
  gesss_rtm_models[[CA]] <- MASS::rlm(GES_MEDIAN_SSS ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content_areas) {
# hist(gesss_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(gesss_rtm_models[[CA]]$residuals);qqline(gesss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior_year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted GES_MEDIAN_SSS by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
school.aggregates[, GES_MEDIAN_SSS_ADJUSTED := as.numeric(NA)]
for (CA in content_areas) {
  school.aggregates[CONTENT_AREA == CA, GES_MEDIAN_SSS_ADJUSTED := GES_MEDIAN_SSS - (PRIOR_MSSS_CENTERED_2YEAR*gesss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
}

##    Visualization, summary and correlation checks
# na.omit(school.aggregates[, as.list(round(summary(GES_MEDIAN_SSS_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS, PRIOR_MSSS_CENTERED_2YEAR])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
# plot(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, PRIOR_MSSS_CENTERED_2YEAR])
# cor(school.aggregates[, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')

# cor(school.aggregates[, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "ELA", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current_year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')

# ges_ss_19_ela_adj <- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == "ELA"]); summary(ges_ss_19_ela_adj)
# ges_ss_19_math_adj<- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=school.aggregates[YEAR == prior_year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_ss_19_math_adj)


##    Create COVID Impact Levels for MSGP Baseline Differences
school.aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF := fcase(
                    MSSS_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                    MSSS_DIFFERENCE_UNCORRECTED < 5 & MSSS_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                    MSSS_DIFFERENCE_UNCORRECTED < -5 & MSSS_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                    MSSS_DIFFERENCE_UNCORRECTED < -15 & MSSS_DIFFERENCE_UNCORRECTED >= -25, "Large",
                    MSSS_DIFFERENCE_UNCORRECTED < -25, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ := fcase(
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < 5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -15 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -25, "Severe")]

##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS := fcase(
                    GES_MEDIAN_SSS >= 0.2, "Improvement",
                    GES_MEDIAN_SSS <  0.2 & GES_MEDIAN_SSS >= -0.2, "Modest to None",
                    GES_MEDIAN_SSS < -0.2 & GES_MEDIAN_SSS >= -0.5, "Moderate",
                    GES_MEDIAN_SSS < -0.5 & GES_MEDIAN_SSS >= -0.8, "Large",
                    GES_MEDIAN_SSS < -0.8, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ := fcase(
                    GES_MEDIAN_SSS_ADJUSTED >= 0.2, "Improvement",
                    GES_MEDIAN_SSS_ADJUSTED <  0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.2, "Modest to None",
                    GES_MEDIAN_SSS_ADJUSTED < -0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.5, "Moderate",
                    GES_MEDIAN_SSS_ADJUSTED < -0.5 & GES_MEDIAN_SSS_ADJUSTED >= -0.8, "Large",
                    GES_MEDIAN_SSS_ADJUSTED < -0.8, "Severe")]

    return(list(SCHOOL_SUMMARIES=school.aggregates))
} ### END academicImpactSummary
