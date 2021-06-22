parMICE <- function(
    don.na, m = 5, method = NULL, predictorMatrix = (1 - diag(1, ncol(don.na))), where = NULL,
    visitSequence = NULL, blots = NULL, post = NULL, blocks,formulas,
    defaultMethod = c("pmm", "logreg", "polyreg", "polr"), maxit = 5,
    seed = NA, data.init = NULL, nnodes = 5, path.outfile = NULL,
    packages = NULL, cluster.type = NULL, ...){

  if (is.null(cluster.type)) cluster.type <- "PSOCK"

  cl <- parallel::makeCluster(nnodes, type = cluster.type)
  if (!is.na(seed)){parallel::clusterSetRNGStream(cl, seed)}

  tmp <- list(...)
  if ("kpmm" %in% names(tmp)){kpmm <- tmp[["kpmm"]]} else {kpmm <- 5}
  if ("method_est" %in% names(tmp)){method_est <- tmp[["method_est"]]} else {method_est <- "mm"}
  if ("incluster" %in% names(tmp)){incluster <- tmp[["incluster"]]} else {incluster <- FALSE}
  if ("nburn" %in% names(tmp)){nburn <- tmp[["nburn"]]} else {nburn <- 200}

  if (maxit==0){stop("The argument maxit=0 is not relevant for parallel calculation, use the mice function from the mice package")}

  parallel::clusterExport(cl,
                          list("mice",
                               "don.na",
                               "method",
                               "predictorMatrix",
                               "visitSequence",
                               "post",
                               "defaultMethod",
                               "maxit",
                               "where",
                               "blots",
                               "data.init",
                               # "find.defaultMethod", # from `micemd` package -- doesn't seem like we need the dependency
                               "nnodes",
                               "path.outfile",
                               "packages",
                               "kpmm", "method_est", "incluster", "nburn"
                       ), envir = environment())

  parallel::clusterEvalQ(cl, eval(parse(text=paste0("require(", packages, ")", collapse=";"))))

  if (!missing(blocks)){parallel::clusterExport(cl, list("blocks"), envir = environment())}
  if (!missing(formulas)){parallel::clusterExport(cl, list("formulas"), envir = environment())}

  if (!is.null(path.outfile)){
    parallel::clusterEvalQ(cl, sink(paste0(path.outfile, "/output", Sys.getpid(), ".txt")))
  }

  if (!missing(blocks) & !missing(formulas)){
    res <- parallel::parSapply(cl, as.list(1:m), FUN=function(mtmp, don.na, method, predictorMatrix, visitSequence,
                                                 where, blocks, formulas, blots, post, defaultMethod, data.init,
                                                 maxit, nnodes, kpmm, method_est, incluster, nburn, ...) {

      res.mice <- mice::mice(data=don.na, m=1, method = method, predictorMatrix=predictorMatrix,
                     visitSequence=visitSequence, where=where, blocks=blocks,
                     formulas=formulas, blots=blots, post=post, defaultMethod=defaultMethod,
                     data.init=data.init, maxit = maxit, printFlag=TRUE, seed=NA,
                     method_est=method_est, incluster=incluster, nburn=nburn, kpmm=kpmm, ...)
    }, don.na=don.na,
    method=method,
    predictorMatrix=predictorMatrix,
    visitSequence=visitSequence,
    blocks=blocks,
    formulas=formulas,
    where=where,
    blots=blots,
    post=post,
    defaultMethod=defaultMethod,
    data.init=data.init,
    maxit=maxit,
    nnodes=nnodes,
    method_est=method_est,
    kpmm=kpmm,
    incluster=incluster,
    nburn=nburn, packages=packages,
    simplify = FALSE, ...)
  } else if (missing(blocks) & missing(formulas)){
    res <- parallel::parSapply(cl, as.list(1:m), FUN=function(mtmp, don.na, method, predictorMatrix,
                                                    visitSequence, where, blots, post,
                                                    defaultMethod, data.init, maxit, nnodes,
                                                    kpmm, method_est, incluster, nburn, ...) {

      res.mice <- mice::mice(data=don.na, m=1, method = method, predictorMatrix=predictorMatrix,
                             visitSequence=visitSequence, where=where, blots=blots, post=post,
                             defaultMethod=defaultMethod, data.init=data.init, maxit = maxit,
                             printFlag=TRUE, seed=NA, method_est=method_est,
                             incluster=incluster, nburn=nburn, kpmm=kpmm, ...)
    }, don.na=don.na,
    method=method,
    predictorMatrix=predictorMatrix,
    visitSequence=visitSequence,
    where=where,
    blots=blots,
    post=post,
    defaultMethod=defaultMethod,
    data.init=data.init,
    maxit=maxit,
    nnodes=nnodes,
    method_est=method_est,
    kpmm=kpmm,
    incluster=incluster,
    nburn=nburn, packages = packages,
    simplify = FALSE, ...)

  } else {stop("blocks or formulas arguments are not defined. Currently, this case is not handled by the mice.par function")}

  parallel::stopCluster(cl)

  res.out <- res[[1]]
  res.out$call <- match.call()
  res.out$m <- m
  res.out$imp <- mapply(as.list(colnames(don.na)), FUN=function(xx, res){do.call(cbind, lapply(lapply(res, "[[", "imp"), "[[", xx))}, MoreArgs=list(res=res), SIMPLIFY = FALSE)
  names(res.out$imp) <- colnames(don.na)
  res.out$imp <- lapply(res.out$imp, function(xx){if (!is.null(xx)){yy <- xx;colnames(yy) <- as.character(seq(ncol(xx)));return(yy)} else {return(xx)}})
  res.out$seed <- seed
  res.out$lastSeedValue <- lapply(res, "[[", "lastSeedValue")
  res.out$chainMean <- do.call(abind::abind, lapply(res, "[[", "chainMean"), 3)
  dimnames(res.out$chainMean)[[3]] <- paste("Chain", seq(m))
  res.out$chainVar <- do.call(abind::abind, lapply(res, "[[", "chainVar"), 3)
  dimnames(res.out$chainVar)[[3]] <- paste("Chain", seq(m))
  res.out$loggedEvents <- lapply(res, "[[", "loggedEvents")
  class(res.out) <- "mids"
  return(res.out)
}
