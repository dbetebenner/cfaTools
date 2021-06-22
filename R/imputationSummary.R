`imputationSummary` <- function(
  data.to.summarize,
  institution.level = NULL,
  summary.level = NULL,
  standardize.scores = NULL
) {

  ###   Utility functions
  se <- function(x, na.rm=FALSE) {
    if (na.rm) x <- na.omit(x)
    sqrt(var(x)/length(x))
  }

  Z <- function(x, y = NULL, rm.na = TRUE) {
    if (is.null(y)) y <- x
    (x - mean(y, na.rm = rm.na)) / sd(y, na.rm = rm.na)
  }

  ###   Avoid "Undefined global functions or variables:" from R CMD check
  SCALE_SCORE_OBSERVED <- SGP_BASELINE_OBSERVED <- SGP_OBSERVED <-
  Mean_SS_Observed <- Mean_SGPB_Observed <- Mean_SGP_Observed <-
  Mean_SS_Imputed <- Mean_SGP_Imputed <- Mean_SGPB_Imputed <- N <- IMP_N <- SGPB_ADF <-
  SGPB_B <- SGPB_CI_high <- SGPB_CI_high_simp <- SGPB_CI_low <- SGPB_CI_low_simp <-
  SGPB_F_Simp <- SGPB_F_Stat <- SGPB_F_p <- SGPB_F_p_simp <- SGPB_Gamma <-
  SGPB_Lamda <- SGPB_RIV <- SGPB_T <- SGPB_U_Imp <- SGPB_U_bar <- SGPB_tSimp <-
  SGPB_tStat <- SGP_ADF <- SGP_B <- SGP_CI_high <- SGP_CI_high_simp <- SGP_CI_low <-
  SGP_CI_low_simp <- SGP_F_Simp <- SGP_F_Stat <- SGP_F_p <- SGP_F_p_simp <-
  SGP_Gamma <- SGP_Lamda <- SGP_RIV <- SGP_T <- SGP_U_Imp <- SGP_U_bar <- SGP_tSimp <-
  SGP_tStat <- SS_ADF <- SS_B <- SS_CI_high <- SS_CI_high_simp <- SS_CI_low <-
  SS_CI_low_simp <- SS_F_Simp <- SS_F_Stat <- SS_F_p <- SS_F_p_simp <- SS_Gamma <-
  SS_Lamda <- SS_RIV <- SS_T <- SS_U_Imp <- SS_U_bar <- SS_tSimp <- SS_tStat <- NULL

  M.IMP <- length(grep("SCALE_SCORE_IMPUTED_", names(data.to.summarize)))

  #####
  ###   summarize at institution.level and summary.level first -- 1 for each imputation
  ###   This is eq Qhat_l 2.16 -- https://stefvanbuuren.name/fimd/sec-whyandwhen.html
  #####

  if (!is.null(standardize.scores)) {
    data.to.summarize[, SS_OBS_ORIG_SCALE := SCALE_SCORE_OBSERVED]
    data.to.summarize[, SCALE_SCORE_OBSERVED := Z(SCALE_SCORE_OBSERVED), by = list(get(standardize.scores))]
  }

  smry_obs <- data.to.summarize[, list(
        Mean_SS_Observed = mean(SCALE_SCORE_OBSERVED, na.rm = TRUE),
        SE_SS_Observed = se(SCALE_SCORE_OBSERVED, na.rm = TRUE),
        Mean_SGP_Observed = mean(SGP_OBSERVED, na.rm = TRUE),
        SE_SGP_Observed = se(SGP_OBSERVED, na.rm = TRUE),
        Mean_SGPB_Observed = mean(SGP_BASELINE_OBSERVED, na.rm = TRUE),
        SE_SGPB_Observed = se(SGP_BASELINE_OBSERVED, na.rm = TRUE),
        Percent_Missing = (sum(is.na(SCALE_SCORE_OBSERVED))/.N)*100, N=.N),
    keyby = c(institution.level, summary.level)]

  smry_imp <- data.table()
  for (IMP in seq(M.IMP)) {
    ss.imp <- paste0("SCALE_SCORE_IMPUTED_", IMP)
    sgp.imp <- paste0("SGP_IMPUTED_", IMP)
    sgpb.imp <- paste0("SGP_BASELINE_IMPUTED_", IMP)

    if (!is.null(standardize.scores)) {
      data.to.summarize[, eval(ss.imp) := Z(x = get(ss.imp), y = SS_OBS_ORIG_SCALE), by = list(get(standardize.scores))]
    }

    tmp_imp_smry <- data.to.summarize[, list(
        Mean_SS_Imputed = mean(get(ss.imp), na.rm = TRUE),  #  Q_l_SS
        SS_U_Imp = var(get(ss.imp), na.rm = TRUE),  #  Ubar_l_SS

        Mean_SGP_Imputed = mean(get(sgp.imp), na.rm = TRUE),  #  Q_l_SGP
        SGP_U_Imp = var(get(sgp.imp), na.rm = TRUE),  #  Ubar_l_SGP

        Mean_SGPB_Imputed = mean(get(sgpb.imp), na.rm = TRUE),  #  Q_l_SGP_Baseline
        SGPB_U_Imp = var(get(sgpb.imp), na.rm = TRUE)),  #  Ubar_l_SGP_Baseline
      keyby = c(institution.level, summary.level)][, IMP_N := IMP]

      smry_imp <- rbindlist(list(smry_imp, tmp_imp_smry))
  }

  #####
  ###   Pool results at institution.level and summary.level over imputations
  #####

  M.IMP <- max(smry_imp$IMP_N)

  smry_imp_pool <- smry_imp[, list(
      Mean_SS_Imputed = mean(Mean_SS_Imputed, na.rm = TRUE),
      SS_U_bar = mean(SS_U_Imp, na.rm = TRUE),
      SS_B = var(Mean_SS_Imputed),

      Mean_SGP_Imputed = mean(Mean_SGP_Imputed, na.rm = TRUE),
      SGP_U_bar = mean(SGP_U_Imp, na.rm = TRUE),
      SGP_B = var(Mean_SGP_Imputed),

      Mean_SGPB_Imputed = mean(Mean_SGPB_Imputed, na.rm = TRUE),
      SGPB_U_bar = mean(SGPB_U_Imp, na.rm = TRUE),
      SGPB_B = var(Mean_SGPB_Imputed)),
    keyby=c(institution.level, summary.level)]

  smry_all <- smry_obs[smry_imp_pool]
  base.names <- names(smry_all)

  ##    T - Total Variance
  smry_all[, SS_T := SS_U_bar + (1 + 1/M.IMP)*SS_B]
  smry_all[, SGP_T := SGP_U_bar + (1 + 1/M.IMP)*SGP_B]
  smry_all[, SGPB_T := SGPB_U_bar + (1 + 1/M.IMP)*SGPB_B]

  ###   Variance Ratios :: 2.3.5

  ##    Lambda -- proportion of variation attributed to the missing data. eq 2.24
  smry_all[, SS_Lamda := round(((SS_B + SS_B/M.IMP)/SS_T), 5)]
  smry_all[, SGP_Lamda := round(((SGP_B + SGP_B/M.IMP)/SGP_T), 5)]
  smry_all[, SGPB_Lamda := round(((SGPB_B + SGPB_B/M.IMP)/SGPB_T), 5)]

  ##    RIV (r) -- relative increase in variance due to nonresponse. eq 2.25
  smry_all[, SS_RIV := round((SS_Lamda/(1-SS_Lamda)), 5)]
  smry_all[, SGP_RIV := round((SGP_Lamda/(1-SGP_Lamda)), 5)]
  smry_all[, SGPB_RIV := round((SGPB_Lamda/(1-SGPB_Lamda)), 5)]

  ##    Adjusted Degrees of Freedom -- eq 2.31
  smry_all[, SS_ADF := (((N-1)+1)/((N-1)+3))*((N-1)*(1-SS_Lamda))]
  smry_all[, SGP_ADF := (((N-1)+1)/((N-1)+3))*((N-1)*(1-SGP_Lamda))]
  smry_all[, SGPB_ADF := (((N-1)+1)/((N-1)+3))*((N-1)*(1-SGPB_Lamda))]

  ##    Gamma -- fraction of information about Q missing due to nonresponse
  smry_all[, SS_Gamma := round((SS_RIV+(2/(SS_ADF+3)))/(1+SS_RIV), 5)]
  smry_all[, SGP_Gamma := round((SGP_RIV+(2/(SGP_ADF+3)))/(1+SGP_RIV), 5)]
  smry_all[, SGPB_Gamma := round((SGP_RIV+(2/(SGPB_ADF+3)))/(1+SGPB_RIV), 5)]

  ###   Statistical Inference -- Section 2.4.2
  ##    Confidence intervals
  smry_all[!is.na(SS_ADF) & SS_ADF != 0, SS_tStat := qt(0.975, df=SS_ADF)*(sqrt(SS_T))]
  smry_all[!is.na(SGP_ADF) & SGP_ADF != 0, SGP_tStat := qt(0.975, df=SGP_ADF)*(sqrt(SGP_T))]
  smry_all[!is.na(SGPB_ADF) & SGPB_ADF != 0, SGPB_tStat := qt(0.975, df=SGPB_ADF)*(sqrt(SGPB_T))]

  smry_all[!is.na(SS_tStat), SS_CI_low := Mean_SS_Imputed-SS_tStat]
  smry_all[!is.na(SS_tStat), SS_CI_high := Mean_SS_Imputed+SS_tStat]

  smry_all[!is.na(SGP_tStat), SGP_CI_low := Mean_SGP_Imputed - SGP_tStat]
  smry_all[!is.na(SGP_tStat), SGP_CI_high := Mean_SGP_Imputed + SGP_tStat]

  smry_all[!is.na(SGPB_tStat), SGPB_CI_low := Mean_SGPB_Imputed - SGPB_tStat]
  smry_all[!is.na(SGPB_tStat), SGPB_CI_high := Mean_SGPB_Imputed + SGPB_tStat]

  ###   Simplified Confidence Invervals - when you have entire population, see Vink and Van Buuren, 2014
  nu <- M.IMP-1
  qtSimp <- qt(0.975, df=nu)

  ##    (1 + 1/M.IMP)*SS_B is the "simplified" T value (U_bar = 0)
  smry_all[!is.na(SS_B) & SS_B != 0, SS_tSimp := qtSimp*(sqrt((1 + 1/M.IMP)*SS_B))]
  smry_all[!is.na(SGP_B) & SGP_B != 0, SGP_tSimp := qtSimp*(sqrt((1 + 1/M.IMP)*SGP_B))]
  smry_all[!is.na(SGPB_B) & SGPB_B != 0, SGPB_tSimp := qtSimp*(sqrt((1 + 1/M.IMP)*SGPB_B))]

  smry_all[!is.na(SS_tSimp), SS_CI_low_simp := Mean_SS_Imputed-SS_tSimp]
  smry_all[!is.na(SS_tSimp), SS_CI_high_simp := Mean_SS_Imputed+SS_tSimp]

  smry_all[!is.na(SGP_tSimp), SGP_CI_low_simp := Mean_SGP_Imputed - SGP_tSimp]
  smry_all[!is.na(SGP_tSimp), SGP_CI_high_simp := Mean_SGP_Imputed + SGP_tSimp]

  smry_all[!is.na(SGPB_tSimp), SGPB_CI_low_simp := Mean_SGPB_Imputed - SGPB_tSimp]
  smry_all[!is.na(SGPB_tSimp), SGPB_CI_high_simp := Mean_SGPB_Imputed + SGPB_tSimp]

  ##    F-test of NULL hypotheses (Observed vs Imputed)
  smry_all[, SS_F_Stat := ((Mean_SS_Observed - Mean_SS_Imputed)^2)/SS_T]
  smry_all[!is.na(SS_ADF) & SS_ADF != 0, SS_F_p := round(pf(SS_F_Stat, df1=1, df2=SS_ADF, lower.tail=FALSE), 5)]
  smry_all[, SS_F_Simp := ((Mean_SS_Observed - Mean_SS_Imputed)^2)/((1 + 1/M.IMP)*SS_B)]
  smry_all[, SS_F_p_simp := round(pf(SS_F_Simp, df1=1, df2=nu, lower.tail=FALSE), 5)]

  smry_all[, SGP_F_Stat := ((Mean_SGP_Observed - Mean_SGP_Imputed)^2)/SGP_T]
  smry_all[!is.na(SGP_ADF) & SGP_ADF != 0, SGP_F_p := round(pf(SGP_F_Stat, df1=1, df2=SGP_ADF, lower.tail=FALSE), 5)]
  smry_all[, SGP_F_Simp := ((Mean_SGP_Observed - Mean_SGP_Imputed)^2)/((1 + 1/M.IMP)*SGP_B)]
  smry_all[, SGP_F_p_simp := round(pf(SGP_F_Simp, df1=1, df2=nu, lower.tail=FALSE), 5)]

  smry_all[, SGPB_F_Stat := ((Mean_SGPB_Observed - Mean_SGPB_Imputed)^2)/SGPB_T]
  smry_all[!is.na(SGPB_ADF) & SGPB_ADF != 0, SGPB_F_p := round(pf(SGPB_F_Stat, df1=1, df2=SGPB_ADF, lower.tail=FALSE), 5)]
  smry_all[, SGPB_F_Simp := ((Mean_SGPB_Observed - Mean_SGPB_Imputed)^2)/((1 + 1/M.IMP)*SGPB_B)]
  smry_all[, SGPB_F_p_simp := round(pf(SGP_F_Simp, df1=1, df2=nu, lower.tail=FALSE), 5)]

  #####
  ###   Overall Summary/Comparison of Imputed vs Observed
  #####

  overall_smry <- smry_all[, list(
      Mean_SS_Diff = round(mean(Mean_SS_Observed-Mean_SS_Imputed, na.rm=TRUE), 3),
      Cor_SS = round(cor(Mean_SS_Observed, Mean_SS_Imputed, use="na.or.complete"), 3),

      Mean_SGP_Diff = round(mean(Mean_SGP_Observed-Mean_SGP_Imputed, na.rm=TRUE), 3),
      Cor_SGP = round(cor(Mean_SGP_Observed, Mean_SGP_Imputed, use="na.or.complete"), 3),

      Mean_SGPB_Diff = round(mean(Mean_SGPB_Observed-Mean_SGPB_Imputed, na.rm=TRUE), 3),
      Cor_SGPB = round(cor(Mean_SGPB_Observed, Mean_SGPB_Imputed, use="na.or.complete"), 3)
  ), keyby=summary.level]

  cols.to.keep <- c(institution.level, summary.level, "Percent_Missing", "N", grep("Mean_|SE_|Lambda|Gamma|RIV|_CI_|_F_p", names(smry_all), value=TRUE))
  list(Summary = smry_all[, cols.to.keep, with=FALSE], Comparison = overall_smry)
}
