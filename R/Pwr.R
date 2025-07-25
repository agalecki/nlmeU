## -> Pwr function
#' Calculates power based on a model fit
#'
#' This function is generic; method functions can be written to handle specific classes of objects.
#'
#' @export
#' @param object an object containing the results returned by a model fitting function (e.g., \code{\link[nlme:lme]{lme}}).
#' @param \dots some methods for this generic function may require additional arguments.
#' @return Numeric scalar value.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @references
#'   Galecki, A., & Burzykowski, T. (2013). *Linear Mixed-Effects Models: A Step-by-Step Approach*. Springer.
#' @seealso \code{\link{Pwr.lme}}
#' @examples
#' \dontrun{
#'   library(nlme)
#'   fm1 <- lme(distance ~ age, data = Orthodont)
#'   Pwr(fm1)
#' }
Pwr <- function(object, ...) UseMethod("Pwr")

## -> Pwr.lme function
#' Performs power calculations for an \code{lme} object
#'
#' This is a method for the \code{\link{Pwr}} generic function. It works for the
#' example given in Galecki and Burzykowski (2013) but may require additional testing,
#' especially for post-hoc power analysis.
#'
#' @method Pwr lme
#' @export
#' @importFrom stats pf qf
#' @param object an \code{\link[nlme:lme]{lme}} object containing a model fit, which provides information needed for power calculations.
#' @param \dots some additional arguments may be required.
#' @param type an optional character string specifying the type of sum of squares to be used in F-tests 
#'   needed for power calculations. Syntax is the same as for \code{\link[nlme:anova.lme]{anova.lme}} in the \code{nlme} package.
#' @param Terms an optional integer or character vector specifying which terms
#'   in the model should be jointly tested to be zero using a Wald F-test. See
#'   \code{\link[nlme:anova.lme]{anova.lme}} in the \code{nlme} package for details.
#' @param L an optional numeric vector or array specifying linear combinations
#'   of the coefficients in the model that should be tested to be zero. See
#'   \code{\link[nlme:anova.lme]{anova.lme}} in the \code{nlme} package for details.
#' @param verbose logical. If \code{TRUE}, additional information is provided. See \code{\link[nlme:anova.lme]{anova.lme}} in the \code{nlme} package for details.
#' @param sigma numeric scalar value.
#' @param ddf numeric scalar value. Redefines the default number of denominator degrees of freedom.
#' @param alpha numeric scalar value. By default, \code{0.05}.
#' @param altB matrix or vector containing alternative values for beta parameters.
#' @param tol numeric scalar value for numerical tolerance.
#' @return A data frame inheriting from class \code{Pwr.lme}.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @references
#'   Galecki, A., & Burzykowski, T. (2013). *Linear Mixed-Effects Models: A Step-by-Step Approach*. Springer.
#' @seealso \code{\link[nlme:anova.lme]{anova.lme}}
Pwr.lme <- function(object, ...,
                    type = c("sequential", "marginal"), 
                    Terms, L, verbose = FALSE, sigma, ddf = numeric(0), alpha = 0.05,
                    altB = NULL, tol = 1e-10) {
  ## Arguments: 
  ##  1. object: one object only
  #   2. adjustSigma set to FALSE. (Try to explore adjustSigma argument if missing(sigma) 
  #   3. Alternative altB name

  .functionName <- "Pwr.lme"                                # Function name
  .traceR <- if (is.null(options()$traceR)) function(...){} else options()$.traceR    

  .traceR(1, lbl = "-> Pwr.lme starts")
  if (!inherits(object, "lme")) {
    stop("Object must inherit from class \"lme\" ")
  }

  Lmiss <- missing(L)
  Tmiss <- missing(Terms)    
  Lx <- !Tmiss || !Lmiss     # Contrasts matrix L can be created 
  fixefs <- object$coefficients$fixed
  fixefNms <- names(fixefs)
  if (!Lx && !missing(sigma)) stop("L or Terms arguments need to be specified with non-missing sigma argument") 
  
  .traceR(10) 
  if (!Tmiss && Lmiss) {          # IF 1  (Check this part)
    .traceR(101, lbl = "IF1", store = FALSE)
    ## Based on Terms argument L matrix is created (with colnames)
    ## Colnames assigned
    cLnms <- fixefNms
    assign <- attr(object$fixDF, "assign")
    nTerms <- length(assign)
    nX <- length(unlist(assign))
    L <- diag(nX)[unlist(assign[Terms]), , drop = FALSE]
    colnames(L) <- cLnms 
    cLnms <- NULL
  }

  if (!missing(sigma)) object <- sigmaTolme(object, value = sigma) # Internal function call

  x <- nlme::anova.lme(object, adjustSigma = FALSE,
                       test = TRUE, type = type, L = L, verbose = verbose)  # ANOVA results stored in x 
  .traceR(15)  

  rt <- attr(x, "rt")   ### Check whether rt is needed
  ndf <- x[["numDF"]]
  ddf2 <- if (length(ddf)) ddf else x[["denDF"]] # ddf2 is needed
  rankL <- ndf

  .traceR(20, lbl = "Before if Lx")
  if (Lx) { 
    .traceR(201, lbl = "if Lx", store = FALSE) 
    ## Terms or L present: cLnms created for later use
    dimL <- if (Lmiss) NULL else dim(L)             # Extract clNms from L argument
    condt <- !Lmiss && is.null(dimL)
    ## cLnms over-written
    cLnms <- if (condt) names(L) else colnames(L)  
  }

  .traceR(202, lbl = "ifLxBefore")
  if (!is.null(altB) && Lx) { # START
    .traceR(210, lbl = "IF 4", store = FALSE)
    if (!Lmiss && is.null(dimL)) {
      .traceR(211, lbl = "IF 3", store = FALSE)
      dim(L) <- c(1, length(L))  # L is matrix
      colnames(L) <- cLnms
    }
    .traceR(215, lbl = "altB")
    ### Go through altB
    altBdt <- as.data.frame(altB)
    altBnrow <- nrow(altBdt)
    altBnms <- names(altBdt)
    fixefx <- object$coefficients$fixed
    dt1 <- data.frame(matrix(rep(fixefx, altBnrow), nrow = altBnrow, byrow = TRUE))
    names(dt1) <- fixefNms
    dt1[, altBnms] <- altBdt

    ## Trimming dimensions
    vcovb <- object$varFix
    if (length(cLnms) < length(fixefNms)) {
      vcovb <- vcovb[cLnms, cLnms]
      dt1efs <- as.matrix(dt1[cLnms])
    }
    .traceR(216, lbl = "altB")
         
    Fstat <- function(bx) {
      b1 <- L %*% bx
      res0 <- t(b1) %*% solve(L %*% vcovb %*% t(L)) %*% b1
      res0 / rankL
    }

    FstatAll <- apply(dt1efs, 1, Fstat)
    .traceR(217, lbl = "FstatAll")
    dtAll2 <- within(dt1, {
      numDF <- ndf
      denDF <- ddf2
      Fvalue <- FstatAll
      Fcrit <- qf(1 - alpha, numDF, denDF)
      nc <- Fvalue * numDF
      Power <- 1 - pf(Fcrit, numDF, denDF, nc)
    })
    .traceR(218, lbl = "EXITdtAll2")
    return(dtAll2)
  }  # END
  .traceR(21, lbl = "ifLx After")
  Fcrit <- qf(1 - alpha, ndf, ddf2)

  ncx <- x[["F-value"]] * ndf  # Rescaling not needed
  Power <- 1 - pf(Fcrit, ndf, ddf2, ncx) 
  F0val <- x[["F-value"]] 
  .traceR(25, lbl = "F0val")

  ret <- data.frame(x$numDF, ddf2, F0val, ncx, Power) # Fcrit omitted
  varNames <- c("numDF", "denDF", "F-value", "nc", "Power")  # Fcrit omitted
  vcovb <- object$varFix 

  .traceR(30, lbl = "if attr(x,L) before")
  if (!is.null(axL <- attr(x, "L"))) {  # L mtx specified
    .traceR(301, lbl = "IF5", store = FALSE)      
    if (!Tmiss) lab <- paste("Power calculations for effect(s):", 
                             paste(Terms, collapse = ", "), "\n",
                             " represented by linear combination: \n")
    if (!Lmiss) lab <- "Power calculations for a linear combination: \n"
    dimL <- dim(L)
    if (is.null(dimL)) names(L) <- cLnms
    attr(ret, "L") <- L
    names(ret) <- varNames
  } else {   # L not-specified. Effects tested one by one.
    .traceR(302, lbl = "ELSE5", store = FALSE)       
    dimnames(ret) <- list(rownames(x), varNames)  ## ???
  }
  .traceR(30, lbl = "if attr(x,L) after")  
         
  ### Modify lab.
  ### if alpha != 0.05 then paste(lab, alpha=)
  ### if ddf is NULL then warning ddf2 incorrect
  if (!is.null(attr(x, "label"))) {  
    attr(ret, "label") <- lab 
  } else {
    attr(ret, "label") <- "Power calculations: \n" 
  }
  attr(ret, "coefficients") <- object$coefficients$fixed
  attr(ret, "varFixed") <- vcovb
  attr(ret, "alpha") <- alpha
  attr(ret, "rt") <- rt

  class(ret) <- c("Pwr.lme", "data.frame")
  .traceR(1, lbl = "Pwr.lme ENDS <-")
  ret
}

#' Print method for Pwr objects
#'
#' Prints the results of power calculations for objects of class \code{Pwr.lme}.
#'
#' @export
#' @param x an object of class \code{Pwr.lme}.
#' @param verbose logical. If \code{TRUE}, additional information is printed. By default, uses the \code{verbose} attribute of \code{x}.
#' @param \dots additional arguments passed to the method.
#' @return Prints the power calculation results and returns \code{NULL} invisibly.
#' @author Andrzej Galecki and Tomasz Burzykowski
print.Pwr <- function(x, verbose = attr(x, "verbose"), ...) {
  if ((rt <- attr(x, "rt")) == 1) {
    if (!is.null(lab <- attr(x, "label"))) {
      cat(lab)
      if (!is.null(L <- attr(x, "L"))) {
        print(zapsmall(L))
      }
    }
    pval <- format(round(x[, "Power"], 4))
    pval[as.double(pval) == 0] <- "<.0001"
    x[, "F-value"] <- format(zapsmall(x[, "F-value"]))
    x[, "nc"] <- format(zapsmall(x[, "nc"]))
    x[, "Power"] <- pval
    print(as.data.frame(x))
  }
  invisible(NULL)
}
