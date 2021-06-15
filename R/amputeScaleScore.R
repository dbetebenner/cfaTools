`amputeScaleScore` <- function(
    ampute.data,
    additional.data = NULL,
    compact.results = FALSE,
    growth.config = NULL,
    status.config = NULL,
    default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
    demographics = NULL,
    institutions = NULL,
    ampute.vars  = NULL,
    ampute.var.weights = NULL, # list(SCALE_SCORE=3, FREE_REDUCED_LUNCH_STATUS=2, SCHOOL_NUMBER=1), # Put institution last (if used)
    reverse.weight = "SCALE_SCORE",
    ampute.args = list(prop=0.3, type="RIGHT"),
    complete.cases.only = TRUE,
    partial.fill = TRUE,
    invalidate.repeater.dups = TRUE,
    seed = 4224L,
    M = 10){

  ###   Utility functions from SGP package
  `%w/o%` <- function(x,y) x[!x %in% y]

  `getKey` <- function(data) {
		if ("YEAR_WITHIN" %in% names(data)) return(c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID", "YEAR_WITHIN")) else return(c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID"))
  } ### END getKey

  ###   Avoid "Undefined global functions or variables:" from R CMD check
  GRADE <- ID <- VALID_CASE <- YEAR <- SCALE_SCORE <- ACHIEVEMENT_LEVEL <-
  SCHOOL_NUMBER <- DISTRICT_NUMBER <- V3 <- TMP_MCAR_PROB <- NULL

  ###   Combine and augment config lists
  amp.config <- growth.config

  if (!is.null(growth.config)) {
    for (f in seq(amp.config)) amp.config[[f]][["analysis.type"]] <- "GROWTH"
  }
  if (!is.null(status.config)) {
    for (f in seq(status.config)) status.config[[f]][["analysis.type"]] <- "STATUS"
    amp.config <- c(amp.config, status.config)
  }

  if (is.null(amp.config)) stop("Either a 'growth.config' or 'status.config' must be supplied")

  ###   If ampute.vars = NULL produce MCAR missingness
  if (is.null(ampute.vars)) {
    ampute.data[, TMP_MCAR_PROB := rnorm(.N), keyby=c("YEAR", "CONTENT_AREA", "GRADE")]
    ampute.var.weights <- NULL
    reverse.weight <- NULL
    ampute.vars <- "TMP_MCAR_PROB"
    default.vars <- c(default.vars, "TMP_MCAR_PROB")
    remove.tmp.amp.var <- TRUE
  } else remove.tmp.amp.var <- FALSE

  long.to.wide.vars <- c(default.vars, institutions, demographics)
  if (any(!ampute.vars %in% long.to.wide.vars)) {
    vars.to.add <- ampute.vars[!ampute.vars %in% long.to.wide.vars]
    long.to.wide.vars <- c(long.to.wide.vars, vars.to.add)
    message("\n\tampute.vars", vars.to.add, "not included in appropriate variable argument.  It has been temporarily added to the list of variables to return.")
  }

  ###   Cycle through amp.config to amputate by cohort
  if (compact.results) {
    amp.list <- vector(mode = "list", length = length(amp.config))
  } else amp.list <- vector(mode = "list", length = M)

  for (K in seq(amp.config)) {
    amp.iter <- amp.config[[K]]

    tmp.lookup <- SJ("VALID_CASE", tail(amp.iter[["sgp.content.areas"]], length(amp.iter[["sgp.grade.sequences"]])),
      tail(amp.iter[["sgp.panel.years"]], length(amp.iter[["sgp.grade.sequences"]])), amp.iter[["sgp.grade.sequences"]])
    # ensure lookup table is ordered by years.  NULL out key after sorted so that it doesn't corrupt the join in dcast.
    setkey(tmp.lookup, V3)
    setkey(tmp.lookup, NULL)
    setkeyv(ampute.data, getKey(ampute.data))

    tmp.long <- ampute.data[tmp.lookup][, unique(c("VALID_CASE", "ID", "YEAR", long.to.wide.vars)), with=FALSE]

    if (amp.iter$analysis.type == "GROWTH") {
      ###   convert long to wide
      tmp.wide <- dcast(tmp.long, ID ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)

      ###   Identify relevant years/scores
      prior.year <- tail(amp.iter$sgp.panel.years, 2)[1]
      current.year <- tail(amp.iter$sgp.panel.years, 1)
      prior.score <- paste("SCALE_SCORE", prior.year, sep=".")
      current.score <- paste("SCALE_SCORE", current.year, sep=".")

      ##    Remove existing missing (MCAR?) records (students without most recent prior.score and/or current.score)
      if (complete.cases.only) {
        tmp.wide <- tmp.wide[!(is.na(get(prior.score)) | is.na(get(current.score))),]
      } else { # Exclude kids missing 2 or more most recent years (at a minimum)
        tmp.wide <- tmp.wide[!(is.na(get(prior.score)) & is.na(get(current.score))),]
      }

      if (partial.fill) {
        ###   Fill in missing content area and grades first
        for (ca in seq(amp.iter$sgp.panel.years)) {
          tmp.wide[, paste("CONTENT_AREA", amp.iter$sgp.panel.years[ca], sep=".") := amp.iter$sgp.content.areas[ca]]
        }

        for (g in seq(amp.iter$sgp.panel.years)) {
          tmp.wide[, paste("GRADE", amp.iter$sgp.panel.years[g], sep=".") := amp.iter$sgp.grade.sequences[g]]
        }

        ###   If using growth fill in missings with SGP = 50 to keep in complete data and give average weight
        if (any(grepl("^SGP", ampute.vars))) {
          tmp.growth.wide <- grep(prior.year, grep("^SGP", names(tmp.wide), value=TRUE), value=TRUE)
          for (tgw in tmp.growth.wide) {
            if (!all(is.na(tmp.wide[,get(tgw)]))) tmp.wide[is.na(get(tgw)), eval(tgw) := 50]
          }
        }

        meas.list <- vector(mode = "list", length = length(long.to.wide.vars))
        meas.list <- lapply(long.to.wide.vars, function(f) meas.list[[f]] <- grep(paste0(f, "[.]"), names(tmp.wide)))
        names(meas.list) <- long.to.wide.vars

        ###   First stretch out to get missings in log data
        long.final <- melt(tmp.wide, id = "ID", variable.name = "YEAR", measure=meas.list)

        ###   Fill in demographics
        setkey(long.final, ID, YEAR)
        long.final <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long.final, ID),
                                tidyselect::all_of(demographics), .direction="downup")))

        ###   Fill in school numbers (CLUDGE) - try to figure out how to do with random forrest or something better...
        if ("SCHOOL_NUMBER" %in% institutions) {
          tmp.long.elem <- long.final[GRADE %in% c(3:5)]
          if (length(unique(tmp.long.elem[, GRADE])) == 1L) {
            tmp.long.elem[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER :=
              sample(unique(na.omit(tmp.long.elem$SCHOOL_NUMBER)), size=sum(is.na(tmp.long.elem$SCHOOL_NUMBER)), replace=TRUE)]
          } else {
            tmp.long.elem <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.elem, ID),
                                          SCHOOL_NUMBER, .direction="updown")))
          }

          tmp.long.mid <- long.final[GRADE %in% c(6:8)]
          if (length(unique(tmp.long.mid[, GRADE])) == 1L) {
            tmp.long.mid[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER :=
              sample(unique(na.omit(tmp.long.mid$SCHOOL_NUMBER)), size=sum(is.na(tmp.long.mid$SCHOOL_NUMBER)), replace=TRUE)]
          } else {
            tmp.long.mid <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.mid, ID),
                                        SCHOOL_NUMBER, .direction="updown")))
          }

          long.final <- rbindlist(list(tmp.long.elem, tmp.long.mid))
        }

        if ("DISTRICT_NUMBER" %in% institutions) {
          long.final <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long.final, ID),
                                  DISTRICT_NUMBER, .direction="updown")))
        }

        long.final[, YEAR := as.character(factor(YEAR, labels = amp.iter[["sgp.panel.years"]]))]

        ###   re-widen
        ##    This could probably be made more parsimonious!  Really only need
        ##    current & most recent prior year w/ ampute.vars
        tmp.wide <- dcast(long.final, ID ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)
      }  else { #  END partial.fill
        long.final <- melt(tmp.wide, id = "ID", variable.name = "YEAR", measure=meas.list)
        long.final[, YEAR := as.character(factor(YEAR, labels = amp.iter[["sgp.panel.years"]]))]
      }

      long.final[, VALID_CASE := "VALID_CASE"]

    } else {  #  END "GROWTH"  --  Begin "STATUS"
      ###   Create institution level summaries
      current.year <- tail(amp.iter$sgp.panel.years, 1)

      subset.wide <- tmp.long[YEAR %in% current.year, c("ID", ampute.vars), with=FALSE] # assuming not using any other current year data.

      if (!is.null(reverse.weight)) {
        for (rev.var in reverse.weight) {
          # tmp.long.priors[, eval(rev.var) := -1*get(rev.var)]
          subset.wide[, eval(rev.var) := -1*get(rev.var)]
        }
      }

      if (length(demog.amp.vars <- intersect(demographics, ampute.vars)) > 0) {
        for (demog in demog.amp.vars) {
          # tmp.long.priors[, eval(demog) := as.integer(factor(get(demog)))-1L]
          subset.wide[, eval(demog) := as.integer(factor(get(demog)))-1L]
        }
      }

      if (length(inst.sum.var <- intersect(institutions, ampute.vars)) > 0) {
        for (inst in inst.sum.var) {
          for (wav in ampute.vars %w/o% institutions) {
            subset.wide[, paste0("TMP_IMV__", wav, "_", inst) := mean(get(wav), na.rm=TRUE), by=list(get(inst))]
          }

          ##    Put in cross school mean for institutions with no students in prior years
          for (tmp.inst.smry in grep("TMP_IMV__", names(subset.wide), value=TRUE)) {
            tmp.mean <- mean(subset.wide[, get(tmp.inst.smry)], na.rm=TRUE)
            subset.wide[is.na(get(tmp.inst.smry)), eval(tmp.inst.smry) := tmp.mean]
          }
          subset.wide[, eval(inst) := NULL]
        }
      }

      #  remove columns that are all NA (e.g., SGP for 3rd grade priors)
      subset.wide <- subset.wide[,
        names(subset.wide)[!unlist(lapply(names(subset.wide), function(f) all(is.na(subset.wide[,get(f)]))))], with=FALSE]
      subset.wide <- na.omit(subset.wide)

      ###   Create long.final with only the "current" year (last elements of the config)
      ###   More thorough to do it with config than just long.final <- tmp.long[YEAR %in% current.year]
      tmp.lookup <- SJ("VALID_CASE", tail(amp.iter[["sgp.content.areas"]], 1),
        tail(amp.iter[["sgp.panel.years"]], 1), tail(amp.iter[["sgp.grade.sequences"]], 1))
      setkeyv(ampute.data, getKey(ampute.data))

      long.final <- ampute.data[tmp.lookup][, unique(c("VALID_CASE", "ID", "YEAR", long.to.wide.vars)), with=FALSE]
    } ###  END "STATUS"

    ###   AMPUTE
    ###   Use "mask" approach to single variable proportion control
    ###   https://rianneschouten.github.io/mice_ampute/vignette/ampute.html#missingness_proportion_per_variable

    ##    Subset out scale scores and demographics
    if (amp.iter$analysis.type == "GROWTH") {  #  done above for "STATUS"
      wide.amp.vars <- paste(ampute.vars %w/o% institutions, prior.year, sep=".")

      ##    Convert character/factor to numeric (0/1) data
      demog.amp.vars <- grep(paste(demographics, collapse="|"), wide.amp.vars, value=TRUE)
      if (length(demog.amp.vars) > 0) {
        for (demog in demog.amp.vars) {
          tmp.wide[, eval(demog) := as.integer(factor(get(demog)))-1L]
        }
      }

      ##    Reverse values requested
      if (!is.null(reverse.weight)) {
        for (rev.var in paste(reverse.weight, prior.year, sep=".")) {
          tmp.wide[, eval(rev.var) := -1*get(rev.var)]
        }
      }

      ##    Create institutional level averages of achievement and demographics
      ##    TMP_IMV__  -  temp institutional mean variable
      if (length(inst.sum.var <- intersect(institutions, ampute.vars)) > 0) {
        for (inst in inst.sum.var) {
          tmp.inst.var <- paste0(inst, ".", prior.year)
          for (wav in wide.amp.vars) {
            tmp.wide[, paste0("TMP_IMV__", wav, "_", inst) := mean(get(wav), na.rm=TRUE), by=list(get(tmp.inst.var))] # strsplit(inst, "_")[[1]][1]
          }
        }
      }
      subset.wide <- tmp.wide[!is.na(get(current.score)),
        grep(paste0("ID|TMP_IMV__|", paste(wide.amp.vars, collapse="|")), names(tmp.wide)), with=FALSE]
      #  remove columns that are all NA (e.g., SGP for 3rd grade priors)
      subset.wide <- subset.wide[,
        names(subset.wide)[!unlist(lapply(names(subset.wide), function(f) all(is.na(subset.wide[,get(f)]))))], with=FALSE]
      subset.wide <- na.omit(subset.wide)

      ##    Find the proportion relative to the complete data that needs to be amputated to give a total ~ ampute.args$prop
      if (!complete.cases.only) {
        target.prop <- round((round(nrow(tmp.wide)*ampute.args$prop, 0)-sum(is.na(tmp.wide[[current.score]])))/nrow(subset.wide), 3)
        ltol <- target.prop-c(0.06, rev(seq(0.001, 0.06, 0.0025)))
        utol <- target.prop+c(0.06, rev(seq(0.001, 0.06, 0.0025)))
      } else {
        target.prop <- ampute.args$prop
        ltol <- target.prop-c(0.03, rev(seq(0.001, 0.03, 0.00125)))
        utol <- target.prop+c(0.03, rev(seq(0.001, 0.03, 0.00125)))
      }

      if (target.prop < 0) {
        target.prop <- 0.001;too.low.tf <- TRUE
        pick.miss <- round(nrow(tmp.wide)*target.prop, 0)
        message("More than ", ampute.args$prop*100, "% missing cases already exist in Grade ", tail(amp.iter[["sgp.grade.sequences"]], 1), " ", tail(amp.iter[["sgp.content.areas"]], 1),
        " 'prop' will be temporarily set to 0.001 (0.1% - ", pick.miss, " records removed)")
      } else too.low.tf <- FALSE
    } else {
      target.prop <- round((round(nrow(long.final)*ampute.args$prop, 0)-sum(is.na(long.final[, SCALE_SCORE])))/nrow(subset.wide), 3)
      ltol <- target.prop-c(0.03, rev(seq(0.001, 0.03, 0.00125)))
      utol <- target.prop+c(0.03, rev(seq(0.001, 0.03, 0.00125)))
      too.low.tf <- FALSE
    }

    ##    Reduce amputation analysis data to non-NA data and standardize
    subset.wide.std <- scale(subset.wide[,-1,])

    tmp.weights <- matrix(rep(1, ncol(subset.wide.std)), nrow = 1)
    if (!is.null(ampute.var.weights)) {
      for (n in names(ampute.var.weights)) {
        tmp.weights[grep(n, names(subset.wide)[-1])] <- ampute.var.weights[[n]]
      }
    }
    #  moderate the STATUS weight for (now current) achievement (0.65 is approx low correlation between current & prior)
    if (amp.iter$analysis.type == "STATUS") {
      if (length(ss.indx <- grep("^SCALE_SCORE$", names(subset.wide)[-1])) > 0) {
        tmp.weights[ss.indx] <- tmp.weights[ss.indx]*0.65
      }
    }

    tmp.scores <- apply(subset.wide.std, 1, function (x) tmp.weights %*% x)
    if (is.null(ampute.args$type)) ampute.args$type <- "RIGHT"

    ##    mice::ampute.continuous doesn't do a good job at getting the right `prop`
    ##    value down  :(  Do a "burn in" to get it closer

    if (!too.low.tf) {
      adj.prop <- target.prop
      fin.props <- c()
      res.props <- c()
      shrink.tol <- 1L
      for (j in 1:25) {
        adj_mask <- mice::ampute.continuous(
                              P = rep(2, nrow(subset.wide)), prop = adj.prop,
                              scores = list(tmp.scores), type = ampute.args$type)
        res.prop <- (sum(adj_mask[[1]]==0)/nrow(subset.wide))
        if (!inrange(res.prop, ltol[shrink.tol], utol[shrink.tol])) {
          # constrain adjustment to keep from getting too big/small too fast
          tmp.ratio <- target.prop/res.prop
          if (tmp.ratio > 1.15) tmp.ratio <- 1.15
          if (tmp.ratio < 0.85) tmp.ratio <- 0.85
          adj.prop <- adj.prop * tmp.ratio
          if (adj.prop > 0.999) adj.prop <- 0.999
          if (adj.prop < 0.001) adj.prop <- 0.001
        } else {
          fin.props <- c(fin.props, adj.prop)
          res.props <- c(res.props, res.prop)
          shrink.tol <- shrink.tol + 1
        }
      }
      if (!is.null(fin.props)) {
        fin.prop <- weighted.mean(x=fin.props, w=(1/abs(1-target.prop/res.props)))
        if (is.na(fin.prop)) fin.prop <- mean(fin.props)
      } else fin.prop <- target.prop

      if (compact.results) {
        amp.tf <- data.table(VALID_CASE = "VALID_CASE", ID = subset.wide$ID, CONTENT_AREA = tail(amp.iter[["sgp.content.areas"]], 1),
                  GRADE = tail(amp.iter[["sgp.grade.sequences"]], 1), YEAR = tail(amp.iter[["sgp.panel.years"]], 1))
        setkey(amp.tf)
      }

      ##    Get M sets of amputation candidates
      for (amp.m in seq(M)) {
        set.seed(seed*amp.m)
        mask_var <- mice::ampute.continuous(
                                P = rep(2, nrow(subset.wide)), prop = fin.prop,
                                scores = list(tmp.scores), type = ampute.args$type)

        if (compact.results) {
          amp.tf[, eval(paste0("AMP_", amp.m)) := mask_var[[1]]==0L]
        } else {
          amp.ids <- subset.wide[which(mask_var[[1]]==0L), ID]

          amp.list[[amp.m]][[K]] <- copy(long.final)
          amp.list[[amp.m]][[K]][YEAR == current.year & ID %in% amp.ids, SCALE_SCORE := NA]
          if ("ACHIEVEMENT_LEVEL" %in% names(amp.list[[amp.m]][[K]])) {
            amp.list[[amp.m]][[K]][YEAR == current.year & ID %in% amp.ids, ACHIEVEMENT_LEVEL := NA]
          }
        }
      }
      if (compact.results) {
        setkeyv(long.final, key(amp.tf))
        amp.list[[K]] <- amp.tf[long.final]
      }
    } else {
      max.scores <- head(rev(sort(tmp.scores)), pick.miss*M)
      id.list <- subset.wide[which(tmp.scores %in% max.scores), ID]
      score.prob <- tmp.scores[which(tmp.scores %in% max.scores)]
      for (amp.m in seq(M)) {
        set.seed(seed*amp.m)
        amp.ids <- sample(x=id.list, size=pick.miss, prob=score.prob)
        amp.list[[amp.m]][[K]] <- copy(long.final)
        amp.list[[amp.m]][[K]][YEAR == current.year & ID %in% amp.ids, SCALE_SCORE := NA]
        if ("ACHIEVEMENT_LEVEL" %in% names(amp.list[[amp.m]][[K]])) {
          amp.list[[amp.m]][[K]][YEAR == current.year & ID %in% amp.ids, ACHIEVEMENT_LEVEL := NA]
        }
      }
    }
  }

  if (compact.results) {
    final.amp.list <- rbindlist(amp.list, use.names = TRUE)
    if (remove.tmp.amp.var) invisible(final.amp.list[, TMP_MCAR_PROB := NULL])
    if (!is.null(additional.data)) {
      final.amp.list <- rbindlist(
        list(final.amp.list, additional.data[, names(final.amp.list) %w/o% paste0("AMP_", seq(M)), with=FALSE]), fill=TRUE)
    }
    if (invalidate.repeater.dups) {
      setkeyv(final.amp.list, getKey(final.amp.list))
      setkeyv(final.amp.list, key(final.amp.list) %w/o% "GRADE")
      dup.ids <- final.amp.list[which(duplicated(final.amp.list, by=key(final.amp.list))), ID]
      final.amp.list[ID %in% dup.ids & is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
    }
  } else {
    final.amp.list <- vector(mode = "list", length = M)

    for (L in seq(M)) {
      final.amp.list[[L]] <- rbindlist(amp.list[[L]], fill=TRUE)

      if (remove.tmp.amp.var) invisible(final.amp.list[[L]][, TMP_MCAR_PROB := NULL])

      if (!is.null(additional.data)) {
        final.amp.list[[L]] <- rbindlist(
          list(final.amp.list[[L]], additional.data[, names(final.amp.list[[L]]), with=FALSE]), fill=TRUE)
      }
      if (invalidate.repeater.dups) {
        setkeyv(final.amp.list[[L]], getKey(final.amp.list[[L]]))
        setkeyv(final.amp.list[[L]], key(final.amp.list[[L]]) %w/o% "GRADE")
        dup.ids <- final.amp.list[[L]][which(duplicated(final.amp.list[[L]], by=key(final.amp.list[[L]]))), ID]
        final.amp.list[[L]][ID %in% dup.ids & is.na(SCALE_SCORE), VALID_CASE := "INVALID_CASE"]
      }
    }
  }
  return(final.amp.list)
}
