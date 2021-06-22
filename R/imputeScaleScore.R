`imputeScaleScore` <- function(
    impute.data,
    additional.data = NULL,
    include.additional.missing = TRUE,
    compact.results = TRUE,
    return.current.year.only = FALSE,
    diagnostics.dir = getwd(),
    growth.config = NULL,
    status.config = NULL,
    default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
    demographics = NULL,
    institutions = NULL,
    impute.factors = "SCALE_SCORE",
    impute.long = FALSE,
    impute.method = NULL,
    cluster.institution = FALSE, # set to TRUE for multilevel (cross-sectional & STATUS) methods
    partial.fill = TRUE,
    parallel.config = NULL, # define cores, packages, cluster.type
    seed = 4224L,
    M = 10,
    maxit = 5,
    verbose=FALSE,
    ...){

  ###   Utility functions from SGP package
  `%w/o%` <- function(x,y) x[!x %in% y]

  `getKey` <- function(data) {
		if ("YEAR_WITHIN" %in% names(data)) return(c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID", "YEAR_WITHIN")) else return(c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID"))
  } ### END getKey

  ###   Avoid "Undefined global functions or variables:" from R CMD check
  SCHOOL_NUMBER <- DISTRICT_NUMBER <- GRADE <- ID <- VALID_CASE <- YEAR <-
  SCALE_SCORE <- IMPUTED <- IMPUTED_SS <- .imp <- V3 <- setNames <- NULL


  ###   Initial checks
  if(length(inst.sum.var <- intersect(institutions, impute.factors)) > 1) stop("Only one 'institution' variable can be used as a impute (summary) variable.")

  if (is.null(impute.method)) {
    impute.method <- "pmm"
  }

  if (!cluster.institution & grepl("2l[.]", impute.method)) {
    cluster.institution <- TRUE
    message("\n\tArgument 'cluster.institution' set to TRUE with 2l.* method\n")
  }

  if (is.null(parallel.config)) {
    parexecute <- "SEQ"
  } else {
    parexecute <- "PAR"
    # require(parallel)
    # require(abind)
    if (is.logical(parallel.config)) parallel.config <- list()
    if (is.null(parallel.config$packages)) {
      parallel.config$packages <- "mice"
    }

    if (is.null(parallel.config$cores)) {
      parallel.config$cores <- parallel::detectCores()-1L
    }

    if (is.null(parallel.config$cluster.type)) {
      parallel.config$cluster.type <- ifelse(.Platform$OS.type == "unix", "FORK", "PSOCK")
    }
  }

  ###   Combine and augment config lists
  imp.config <- growth.config

  if (!is.null(growth.config)) {
    for (f in seq(imp.config)) imp.config[[f]][["analysis.type"]] <- "GROWTH"
  }
  if (!is.null(status.config)) {
    for (f in seq(status.config)) status.config[[f]][["analysis.type"]] <- "STATUS"
    imp.config <- c(imp.config, status.config)
  }

  if (is.null(imp.config)) stop("Either a 'growth.config' or 'status.config' must be supplied")

  long.to.wide.vars <- c(default.vars, institutions, demographics)

  ###   Cycle through imp.config to amputate by cohort
  if (compact.results) {
    imp.list <- vector(mode = "list", length = length(imp.config))
  } else imp.list <- vector(mode = "list", length = M)

  for (K in seq(imp.config)) {
    imp.iter <- imp.config[[K]]

    tmp.lookup <- SJ("VALID_CASE", tail(imp.iter[["sgp.content.areas"]], length(imp.iter[["sgp.grade.sequences"]])),
      tail(imp.iter[["sgp.panel.years"]], length(imp.iter[["sgp.grade.sequences"]])), imp.iter[["sgp.grade.sequences"]])
    # ensure lookup table is ordered by years.  NULL out key after sorted so that it doesn't corrupt the join in dcast.
    setkey(tmp.lookup, V3)
    setkey(tmp.lookup, NULL)
    setkeyv(impute.data, getKey(impute.data))

    tmp.long <- impute.data[tmp.lookup][, unique(c("VALID_CASE", "ID", "YEAR", long.to.wide.vars)), with=FALSE]

    prior.years <- head(imp.iter$sgp.panel.years, -1)
    current.year <- tail(imp.iter$sgp.panel.years, 1)
    current.grade <- tail(imp.iter$sgp.grade.sequences, 1)
    current.subject <- tail(imp.iter$sgp.content.areas, 1)
    prior.scores <- paste("SCALE_SCORE", prior.years, sep=".")
    current.score <- paste("SCALE_SCORE", current.year, sep=".")

    if (imp.iter$analysis.type == "GROWTH") {
      ###   convert long to wide
      tmp.wide <- dcast(tmp.long, ID ~ YEAR, sep=".", drop=FALSE, value.var=c("VALID_CASE", long.to.wide.vars))

      ###   Exclude kids missing current and most recent year's scale score
      tmp.wide <- tmp.wide[!(is.na(get(tail(prior.scores, 1))) & is.na(get(current.score))),]

      if (!include.additional.missing) {
        tmp.wide <- tmp.wide[!is.na(get(paste0("VALID_CASE.", current.year))),]
      }

      meas.list <- vector(mode = "list", length = length(long.to.wide.vars))
      meas.list <- lapply(long.to.wide.vars, function(f) meas.list[[f]] <- grep(paste0(f, "[.]"), names(tmp.wide)))
      names(meas.list) <- long.to.wide.vars

      if (partial.fill) {
        ###   Fill in missing content area and grades first
        for (ca in seq(imp.iter$sgp.panel.years)) {
          tmp.wide[, paste("CONTENT_AREA", imp.iter$sgp.panel.years[ca], sep=".") := imp.iter$sgp.content.areas[ca]]
        }

        for (g in seq(imp.iter$sgp.panel.years)) {
          tmp.wide[, paste("GRADE", imp.iter$sgp.panel.years[g], sep=".") := imp.iter$sgp.grade.sequences[g]]
        }

        ###   First stretch out to get missings in log data
        long.final <- melt(tmp.wide, id = "ID", variable.name = "YEAR", measure=meas.list)

        ###   Fill in demographics
        setkey(long.final, ID, YEAR)
        long.final <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long.final, ID),
                                tidyselect::all_of(demographics), .direction="downup")))

        ###   Fill in school numbers
        if ("SCHOOL_NUMBER" %in% institutions) {
          # if (!is.character(long.final[, SCHOOL_NUMBER])) long.final[, SCHOOL_NUMBER := as.character(SCHOOL_NUMBER)]
          # invisible(long.final[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER := -99999])

          ##  (CLUDGE) - try to figure out how to do with random forrest or something better...
          tmp.long.elem <- long.final[GRADE %in% c(0:5)]
          if (length(unique(tmp.long.elem[, GRADE])) == 1L) {
            invisible(tmp.long.elem[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER :=
              sample(unique(na.omit(tmp.long.elem$SCHOOL_NUMBER)), size=sum(is.na(tmp.long.elem$SCHOOL_NUMBER)), replace=TRUE)])
          } else {
            tmp.long.elem <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.elem, ID),
                                          SCHOOL_NUMBER, .direction="updown")))
          }

          tmp.long.mid <- long.final[GRADE %in% c(6:8)]
          if (length(unique(tmp.long.mid[, GRADE])) == 1L) {
            invisible(tmp.long.mid[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER :=
              sample(unique(na.omit(tmp.long.mid$SCHOOL_NUMBER)), size=sum(is.na(tmp.long.mid$SCHOOL_NUMBER)), replace=TRUE)])
          } else {
            tmp.long.mid <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.mid, ID),
                                        SCHOOL_NUMBER, .direction="updown")))
          }

          tmp.long.high <- long.final[GRADE %in% c(9:12)]
          if (length(unique(tmp.long.high[, GRADE])) == 1L) {
            invisible(tmp.long.high[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER :=
              sample(unique(na.omit(tmp.long.high$SCHOOL_NUMBER)), size=sum(is.na(tmp.long.high$SCHOOL_NUMBER)), replace=TRUE)])
          } else {
            tmp.long.high <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.high, ID),
                                        SCHOOL_NUMBER, .direction="updown")))
          }

          long.final <- rbindlist(list(tmp.long.elem, tmp.long.mid, tmp.long.high))
        }

        if ("DISTRICT_NUMBER" %in% institutions) {
          # invisible(long.final[is.na(DISTRICT_NUMBER), DISTRICT_NUMBER := -99999])
          long.final <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long.final, ID),
                                   DISTRICT_NUMBER, .direction="updown")))
        }

        invisible(long.final[, YEAR := as.character(factor(YEAR, labels = imp.iter[["sgp.panel.years"]]))])

        if (!impute.long) {
          ###   re-widen
          ##    This could probably be made more parsimonious!  Really only need
          ##    current & most recent prior year w/ impute.factors
          tmp.wide <- dcast(long.final, ID ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)
        }
      } else { #  END partial.fill
        long.final <- melt(tmp.wide, id = "ID", variable.name = "YEAR", measure=meas.list)
        long.final[, YEAR := as.character(factor(YEAR, labels = imp.iter[["sgp.panel.years"]]))]
      }

      if (impute.long) {
        tmp.impute.factors <- impute.factors
        ##    Convert character/factor to numeric (0/1) data
        demog.imp.vars <- grep(paste(demographics, collapse="|"), impute.factors, value=TRUE)
        if (length(demog.imp.vars) > 0) {
          for (demog in demog.imp.vars) {
            long.final[, eval(demog) := as.integer(factor(get(demog)))-1L]
            if (length(inst.sum.var) > 0) {
              tmp.imv <- paste0("IMV___", demog, "___", inst.sum.var)
              long.final[, eval(tmp.imv) := mean(get(demog), na.rm=TRUE), by=c("GRADE", inst.sum.var)]
              long.final[, eval(tmp.imv) := scale(get(tmp.imv)), keyby = "GRADE"]
              long.final[is.na(get(tmp.imv)), eval(tmp.imv) := 0]
              tmp.impute.factors <- c(tmp.impute.factors, tmp.imv)
            }
          }
        }

        if (length(inst.sum.var) > 0) {
          tmp.imv <- paste0("IMV___SCALE_SCORE___", inst.sum.var)
          long.final[, eval(tmp.imv) := mean(SCALE_SCORE, na.rm=TRUE), by=c("GRADE", inst.sum.var)]
          long.final[, eval(tmp.imv) := scale(get(tmp.imv)), keyby = "GRADE"]
          long.final[is.na(get(tmp.imv)), eval(tmp.imv) := 0]
          tmp.impute.factors <- c(tmp.impute.factors, tmp.imv)
        }

        if (impute.method %in% c("2l.pan", "2l.lmer", "2l.norm")) {
          long.final[, ID := as.integer(ID)]
        } else long.final[, ID := as.character(ID)]
        long.final[, GRADE := as.numeric(GRADE)]
      }
      long.final[, VALID_CASE := "VALID_CASE"]

    } else {  #  END "GROWTH"  --  Begin "STATUS"
      ###   Create impute.subset and tmp.long.priors
      ###   More thorough to do it with config than just:
        #   impute.subset <- tmp.long[, c("ID", impute.factors), with=FALSE] # %w/o% "SCALE_SCORE" assuming not using any other current year data.
      current.lookup <- SJ("VALID_CASE", tail(imp.iter[["sgp.content.areas"]], 1),
        tail(imp.iter[["sgp.panel.years"]], 1), tail(imp.iter[["sgp.grade.sequences"]], 1))
      setkey(current.lookup, V3)
      setkey(current.lookup, NULL)
      setkeyv(tmp.long, getKey(tmp.long))
      impute.subset <- tmp.long[current.lookup][, c("ID", impute.factors), with=FALSE]

      #  remove columns that are all NA (e.g., SGP for 3rd grade priors)
      impute.subset <- impute.subset[,
        names(impute.subset)[!unlist(lapply(names(impute.subset), function(f) all(is.na(impute.subset[,get(f)]))))], with=FALSE]

      priors.lookup <- SJ("VALID_CASE", head(imp.iter[["sgp.content.areas"]], -1),
        head(imp.iter[["sgp.panel.years"]], -1), head(imp.iter[["sgp.grade.sequences"]], -1))
      # ensure lookup table is ordered by years.  NULL out key after sorted so that it doesn't corrupt the join in dcast.
      setkey(priors.lookup, V3)
      setkey(priors.lookup, NULL)

      tmp.long.priors <- tmp.long[priors.lookup[V3 != current.year]][, c("YEAR", "GRADE", impute.factors), with=FALSE]

      ###   Create institution level summaries

      if (length(demog.imp.vars <- intersect(demographics, impute.factors)) > 0) {
        for (demog in demog.imp.vars) {
          tmp.long.priors[, eval(demog) := as.integer(factor(get(demog)))-1L]
          impute.subset[, eval(demog) := as.integer(factor(get(demog)))-1L]
        }
      }

      if (length(inst.sum.var) > 0) {
        tmp.inst.var <- paste0(inst.sum.var, ".", current.year)

        impute.subset[, paste0("IMV___SCALE_SCORE.", current.year, "___", tmp.inst.var) := as.numeric(NA)]

        if (length(demog.imp.vars) > 0) {
          for (demog in demog.imp.vars) {
            impute.subset[, paste0("IMV___", demog, "___", tmp.inst.var) := mean(get(demog), na.rm=TRUE), by=list(get(inst.sum.var))]
            impute.subset[, paste0("IMV___", demog, "___", tmp.inst.var) := scale(get(paste0("IMV___", demog, "___", tmp.inst.var)))]
          }
        }

        ##    Prior year(s) summaries to use
        smry_eval_expression <-
          paste0("PRIOR_IMV__", impute.factors %w/o% c(demographics, institutions), "_", tmp.inst.var, " = ", "mean(", impute.factors %w/o% c(demographics, institutions), ", na.rm=TRUE)") # intersect(impute.factors, demographics)
        smry_eval_expression <- setNames(smry_eval_expression, sub('^(.*) = .*', '\\1', smry_eval_expression))

        tmp_inst_smry <- tmp.long.priors[!is.na(get(inst.sum.var)),
              					   lapply(smry_eval_expression, function(f) eval(parse(text=f))), keyby = inst.sum.var]

        impute.subset <- merge(impute.subset, tmp_inst_smry, by=inst.sum.var, all.x = TRUE)
        setnames(impute.subset, inst.sum.var, tmp.inst.var)

        ##    Put in cross school mean for schools with no students in prior years
        for (prior.smry in grep("PRIOR_IMV__", names(impute.subset), value=TRUE)) {
          tmp.inst.mean <- mean(impute.subset[, get(prior.smry)], na.rm=TRUE)
          impute.subset[is.na(get(prior.smry)), eval(prior.smry) := tmp.inst.mean]
        }
      }
      setnames(impute.subset, "SCALE_SCORE", current.score)

      long.final <- impute.data[current.lookup][, unique(c("VALID_CASE", "ID", "YEAR", long.to.wide.vars)), with=FALSE]
    } ###  END "STATUS"

    #####
    ###   IMPUTE
    #####

    ##    Subset out scale scores and demographics
    if (imp.iter$analysis.type == "GROWTH" & !impute.long) {  #  done above for "STATUS"
      wide.imp.vars <- grep(paste0(impute.factors %w/o% institutions, "[.]", collapse="|"), names(tmp.wide), value=TRUE)
      wide.imp.vars <- c(wide.imp.vars, paste(intersect(impute.factors, institutions), current.year, sep="."))

      #  create subset of wide data with only variables to be used in imputation
      impute.subset <- tmp.wide[, grep(paste0("ID|", paste(wide.imp.vars, collapse="|")), names(tmp.wide)), with=FALSE]
      #  remove columns that are all NA (e.g., SGP for 3rd grade priors)
      impute.subset <- impute.subset[,
        names(impute.subset)[!unlist(lapply(names(impute.subset), function(f) all(is.na(impute.subset[,get(f)]))))], with=FALSE]

      ##    Convert character/factor to numeric (0/1) data
      demog.imp.vars <- grep(paste(demographics, collapse="|"), wide.imp.vars, value=TRUE)
      if (length(demog.imp.vars) > 0) {
        for (demog in demog.imp.vars) {
          impute.subset[, eval(demog) := as.integer(factor(get(demog)))-1L]
        }

        ##    Create a single demographic average value for demographics in impute.factors
        for (demog in intersect(impute.factors, demographics)) {
          tmp.dmg <- grep(demog, demog.imp.vars, value=TRUE)
          impute.subset[, paste0("SUMSCORE__", demog) := rowSums(.SD, na.rm = TRUE)/length(tmp.dmg), .SDcols = tmp.dmg]
          impute.subset[, eval(tmp.dmg) := NULL]
          wide.imp.vars <- c(wide.imp.vars %w/o% tmp.dmg, paste0("SUMSCORE__", demog))
        }
      }
      ##    Create institutional level averages of achievement and demographics
      ##    IMV__  -  institutional mean variable
      if (length(inst.sum.var) > 0) {
        tmp.inst.var <- paste0(inst.sum.var, ".", current.year)
        invisible(impute.subset[, paste0("IMV___SCALE_SCORE.", current.year, "___", tmp.inst.var) := as.numeric(NA)])

        tmp.demg.vars <- grep(paste(demographics, collapse="|"), wide.imp.vars, value=TRUE)
        for (wiv in tmp.demg.vars) {
          impute.subset[, paste0("IMV___", wiv, "___", inst.sum.var) := mean(get(wiv), na.rm=TRUE), by=list(get(tmp.inst.var))] # tail(tmp.inst.vars, 1)
          impute.subset[, paste0("IMV___", wiv, "___", inst.sum.var) := scale(get(paste0("IMV___", wiv, "___", inst.sum.var)))]
        }
      }
    }  ###  END "GROWTH"

    if (!impute.long | imp.iter$analysis.type == "STATUS") {
      tmp.meth <- mice::make.method(data=impute.subset)
      tmp.pred <- mice::make.predictorMatrix(data=impute.subset)
      tmp.pred[, "ID"] <- 0
      if (length(inst.sum.var) > 0) {
        if (cluster.institution) {
          tmp.pred[, tmp.inst.var] <- -2
        } else tmp.pred[, tmp.inst.var] <- 0
      }

      tmp.grp.means <- names(tmp.meth)[grepl("IMV___", names(tmp.meth)) & tmp.meth != ""]
      if (length(tmp.grp.means) > 0) {
        if (parexecute=="SEQ") {
          # require(miceadds)
        } else {
          if (!"miceadds" %in% parallel.config$packages)
            parallel.config$packages <- c(parallel.config$packages, "miceadds")
        }

        tmp.meth[tmp.grp.means] <- "2l.groupmean" # "2l.latentgroupmean.ml" #
        for (f in seq(tmp.grp.means)) {
          tmp.f <- strsplit(tmp.grp.means[f], "___")[[1]]
          tmp.pred[tmp.grp.means[f], tmp.f[2]] <- 2
          tmp.pred[tmp.grp.means[f], tmp.f[3]] <- -2
          tmp.pred[tmp.grp.means[f], intersect(colnames(tmp.pred), c(demographics, paste0("SUMSCORE__", demographics)))] <- 0 # avoid "rank deficient" in latentgroupmean
          impute.subset[, eval(tmp.f[3]) := as.integer(get(tmp.f[3]))]
        }
      }

      tmp.meth[current.score] <- impute.method
    } else {
      impute.subset <- long.final[, c("ID", "GRADE", tmp.impute.factors %w/o% institutions), with=FALSE]
      tmp.meth <- mice::make.method(data=impute.subset)
      tmp.meth[1:length(tmp.meth)] <- ""

      if (is.null(impute.method)) {
        tmp.meth["SCALE_SCORE"] <- "2l.pan" # "2l.pmm" -- miceadds
      } else tmp.meth["SCALE_SCORE"] <- impute.method

      tmp.pred <- mice::make.predictorMatrix(data=impute.subset)

      tmp.pred["SCALE_SCORE", "ID"] <- -2
      tmp.pred["SCALE_SCORE", "GRADE"] <- 2 # random effect for GRADE (time)
    }

    switch(parexecute,
      SEQ = imp <- suppressWarnings(mice::mice(impute.subset, method = tmp.meth, predictorMatrix = tmp.pred,
                                               m = M, maxit = maxit, seed = seed, print=verbose, ...)),
      PAR = imp <- suppressWarnings(parMICE(impute.subset, method = tmp.meth, predictorMatrix = tmp.pred,
                                            m = M, maxit = maxit, seed = seed, nnodes = parallel.config$cores,
                                            packages = parallel.config$packages, cluster.type = parallel.config$cluster.type, ...)))

    ##    Save some diagnostic plots
    if (!dir.exists(file.path(diagnostics.dir, "diagnostics"))) dir.create(file.path(diagnostics.dir, "diagnostics"))
    pdf(file.path(diagnostics.dir, "diagnostics", paste0("Grade_", current.grade, "_", current.subject, "_",
                  gsub("[.]", "", impute.method), ifelse(impute.long, "_LONG", ""), "_M_", M, "__maxit_", maxit, "__converge.pdf")))
    print(plot(imp)) #, names(tmp.meth[tmp.meth != ""]))) # [1:2]
    invisible(dev.off())
    pdf(file.path(diagnostics.dir, "diagnostics", paste0("Grade_", current.grade, "_", current.subject, "_",
                  gsub("[.]", "", impute.method), ifelse(impute.long, "_LONG", ""), "_M_", M, "__maxit_", maxit, "__density.pdf")))
    if (!impute.long | imp.iter$analysis.type == "STATUS") {
      print(densityplot(imp, eval(parse(text=paste0("~", current.score)))))
      print(densityplot(imp))
    } else print(densityplot(imp))
    invisible(dev.off())

    ###   Format and store results

    if (compact.results) {
      if (!impute.long | imp.iter$analysis.type == "STATUS") {
        long.imputed <- as.data.table(complete(imp, action="long", include=TRUE))[, c(".imp", "ID", current.score), with=FALSE]
        wide.imputed <- dcast(long.imputed, ID ~ .imp, value.var=current.score)
      } else {
        long.imputed <- as.data.table(complete(imp, action="long", include=TRUE))[, c(".imp", "ID", "GRADE", "SCALE_SCORE"), with=FALSE]
        wide.imputed <- dcast(long.imputed[GRADE == current.grade,], ID ~ .imp, value.var="SCALE_SCORE")
      }

      setnames(wide.imputed, names(wide.imputed)[-1], c("SCALE_SCORE", paste0("SCORE_IMP_", names(wide.imputed)[-c(1:2)])))
      invisible(wide.imputed[, YEAR := eval(current.year)])
      setkey(wide.imputed, ID, YEAR, SCALE_SCORE)
      setkey(long.final, ID, YEAR, SCALE_SCORE)
      if (return.current.year.only) {
        imp.list[[K]] <- long.final[wide.imputed]
      } else imp.list[[K]] <- wide.imputed[long.final]
    } else {
      if (!impute.long | imp.iter$analysis.type == "STATUS") {
        tmp.wide[, IMPUTED := is.na(get(current.score))]
        long.imputed <- as.data.table(complete(imp, action="long"))[, c(".imp", "ID", current.score), with=FALSE][, YEAR := eval(current.year)]
        setkey(long.imputed, ID, YEAR)
        setkey(long.final, ID, YEAR)
      }
      for (m in seq(M)) {
        if (return.current.year.only) {
          imp.list[[m]][[K]] <- long.final[long.imputed[.imp==m]]
        } else imp.list[[m]][[K]] <- long.imputed[.imp==m][long.final]
        invisible(imp.list[[m]][[K]][, IMPUTED_SS := FALSE])
        invisible(imp.list[[m]][[K]][is.na(SCALE_SCORE) & YEAR == eval(current.year), IMPUTED_SS := TRUE])
        invisible(imp.list[[m]][[K]][is.na(SCALE_SCORE) & YEAR == eval(current.year), SCALE_SCORE := get(current.score)])
        invisible(imp.list[[m]][[K]][, c(".imp", current.score) := NULL])
      }
    }
  }  ###  END K

  if (compact.results) {
    final.imp.list <- rbindlist(imp.list, use.names = TRUE, fill=TRUE)

    if (!is.null(additional.data)) {
      final.imp.list <- rbindlist(
        list(final.imp.list, additional.data[, names(final.imp.list), with=FALSE]), fill=TRUE)
    }

    ##    remove dups for repeaters created from long to wide to long reshaping
    setkeyv(final.imp.list, getKey(final.imp.list))
    setkeyv(final.imp.list, key(final.imp.list) %w/o% "GRADE")
    dup.ids <- final.imp.list[which(duplicated(final.imp.list, by=key(final.imp.list))), ID]
    final.imp.list[ID %in% dup.ids & is.na(SCALE_SCORE), VALID_CASE := "NEW_DUP"] # INVALID_CASE

    final.imp.list <- final.imp.list[VALID_CASE != "NEW_DUP"]
  } else {
    final.imp.list <- vector(mode = "list", length = M)

    for (L in seq(M)) {
      final.imp.list[[L]] <- rbindlist(imp.list[[L]], fill=TRUE)

      if (!is.null(additional.data)) {
        final.imp.list[[L]] <- rbindlist(
          list(final.imp.list[[L]], additional.data[, names(final.imp.list[[L]]), with=FALSE]), fill=TRUE)
      }

      ##    remove dups for repeaters created from long to wide to long reshaping
      setkeyv(final.imp.list[[L]], getKey(final.imp.list[[L]]))
      setkeyv(final.imp.list[[L]], key(final.imp.list[[L]]) %w/o% "GRADE")
      dup.ids <- final.imp.list[[L]][which(duplicated(final.imp.list[[L]], by=key(final.imp.list[[L]]))), ID]
      final.imp.list[[L]][ID %in% dup.ids & is.na(SCALE_SCORE), VALID_CASE := "NEW_DUP"] # INVALID_CASE

      final.imp.list[[L]] <- final.imp.list[[L]][VALID_CASE != "NEW_DUP"]
    }
  }
  return(final.imp.list)
}
