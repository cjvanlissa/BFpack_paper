asyCov <- function(x, n, dropNA=FALSE, as.matrix=TRUE,
                   acov=c("individual", "unweighted", "weighted"),
                   suppressWarnings=TRUE, silent=TRUE, run=TRUE, ...) {
  #x <- cor(iris[, 1:4])
  #n <- nrow(iris)
    # Assumption: check the diagonals for missing data only
    p <- nrow(x)
    cNames <- oldNames <- dimnames(x)[[1]]

    # create matrix of labels for ps
    psOldNames <- outer(oldNames, oldNames, paste, sep = "_")
    psMatnames <- outer(cNames, cNames, paste, sep = "_")


    psOldNames <- vechs(psOldNames)
    acovName <- vechs(psMatnames)
    S <- mxMatrix("Stand", nrow = p, ncol = p, free = TRUE, values = jitter(vechs(cov2cor(x))),
                  name = "S", labels = acovName)
    D <- mxMatrix("Diag", nrow = p, ncol = p, free = TRUE, values = sqrt(Diag(x)),
                  name = "D")
    modelName <- "Asymptotic covariance matrix of correlation matrix"

    expCov <- mxAlgebra(D %&% S, name = "expCov", dimnames = list(cNames, cNames))

    cModel <- mxModel(model = modelName, mxData(x, "cov", numObs = n), S,
                      D, expCov, mxFitFunctionML(),
                      mxExpectationNormal("expCov", means=NA, dimnames = cNames))

    ## Return mx model without running the analysis
    if (run==FALSE) return(cModel)

    # try to run it with error message as output
    mxFit <- tryCatch(mxRun(cModel, silent=silent, suppressWarnings=suppressWarnings, ...),
                      error = function(e) e)
    if (inherits(mxFit, "error")) {
      stop(print(mxFit))
    }
    # Need to multiply 2 to the inverse of Hessian matrix
    # http://openmx.psyc.virginia.edu/thread/360
    # Fixed a bug that all elements have to be inverted before selecting some of them
    acovS <- tryCatch(2 * solve(mxFit@output$calculatedHessian)[acovName, acovName, drop=FALSE],
                      error = function(e) e)
    if (inherits(acovS, "error")) {
      stop(print(acovS))
    }

    ## No need to do it as [, drop=FALSE] has been added
    ## # When the dimensions are 1x1, dimnames are removed. Added them explicitly
    ## dimnames(acovS) <- list(acovName, acovName)

    if (dropNA) {
      out <- acovS
    } else {
      # oldNames include data for NA
      p <- length(psOldNames)
      out <- matrix(NA, nrow=p, ncol=p, dimnames=list(psOldNames, psOldNames))
      out[acovName, acovName] <- acovS
    }
    return(out)
  }

}
