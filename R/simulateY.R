## -> simulateY generic function
#' Simulates values of the dependent variable based on a model fit
#'
#' This function is generic; method functions can be written to handle specific classes of objects.
#'
#' @param object an object with a model fit for which the dependent variable is to be simulated.
#' @param nsim number of simulations. By default, \code{nsim = 1}.
#' @param seed integer scalar used to initiate the random number generator.
#' @param \dots some methods for this generic function may require additional arguments.
#' @param verbose logical. If \code{TRUE}, basic information about arguments is provided. By default set to \code{FALSE}.
#' @param sigma numeric scalar. Allows simulations employing an alternative value of the scale parameter.
#' @return Numeric matrix with the number of columns determined by the \code{nsim} argument.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @examples
#'
#' \dontrun{
#'   library(nlme)
#'   fm1 <- lme(distance ~ age, data = Orthodont)
#'   simulateY(fm1)
#' }
#' @export
simulateY <- function(object, nsim = 1, seed = NULL, ..., verbose = FALSE, sigma) UseMethod("simulateY")

## -> simulateY.lme method
#' Simulates values for an \code{lme} object
#'
#' This is a method for the \code{\link{simulateY}} generic function.
#'
#' Simulates values of the dependent variable for a fitted \code{\link[nlme:lme]{lme}} model.
#' Data with one level of grouping only.
#'
#' @method simulateY lme
#' @export
#' @importFrom stats runif fitted rnorm
#' @param object an \code{\link[nlme:lme]{lme}} object with a model fit for which the dependent variable is to be simulated.
#' @param nsim number of simulations. By default, \code{nsim = 1}.
#' @param seed integer scalar used to initiate the random number generator.
#' @param \dots some methods for this generic function may require additional arguments.
#' @param verbose logical. If \code{TRUE}, basic information about arguments is provided. By default set to \code{FALSE}.
#' @param sigma numeric scalar. Allows simulations employing an alternative value of the scale parameter.
#' @return Numeric matrix with the number of columns determined by the \code{nsim} argument.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @examples
#'
#' \dontrun{
#'   library(nlme)
#'   fm1 <- lme(distance ~ age, data = Orthodont)
#'   simulateY(fm1)
#' }
#' 
simulateY.lme <- function(object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), ...,
                          verbose = FALSE, sigma) {
  .functionName <- "simulateY.lme"
  .traceR <- if (is.null(options()$traceR)) function(...){} else options()$.traceR      

  .traceR(1, lbl = "-> simulateY.lme STARTS")

  if (verbose) print(paste("nsim = ", nsim, ", seed = ", seed, sep = ""))

  if (!inherits(object, "lme")) stop("Object must inherit from class \"lme\" ")

  .traceR(110, lbl = "missing(sigma)")

  if (!missing(sigma)) object$sigma <- sigma
  .traceR(115, lbl = "after !missing(sigma) ")

  groups <- object$groups[[1]]
  .traceR(120, lbl = "groups")

  ugroups <- unique(groups)
  individuals <- as.matrix(ugroups)
  if (is.numeric(individuals)) individuals <- ugroups[individuals]
  .traceR(130, lbl = "individuals")
  
  Vlist <- nlme::getVarCov(object, individuals, type = "marginal")
  fitd0 <- fitted(object, level = 0)
  chVlist <- lapply(Vlist, chol)

  nx <- nsim * length(fitd0)

  set.seed(seed)
  noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

  .traceR(150, lbl = "lapply STARTS here ***")
  dimN <- sapply(chVlist, ncol)  # Number of records per subject
  cdimN1 <- cumsum(dimN)
  cdimN0 <- cumsum(dimN) - dimN + 1
  cdimN <- cbind(cdimN0, cdimN1)
  tList <- vector(length(dimN), mode = "list")
  tList[] <- 1:length(dimN)
  auxF1 <- function(el) { # 1,2, ...
    .traceR(1001, lbl = paste("Local auxF1() STARTS. el =", el), store = FALSE)
 
    chV <- chVlist[[el]]
    ix <- cdimN[el, ]
    i0 <- ix[1]
    i1 <- ix[2]
    noisex <- noise.mtx[i0:i1, ]
    tx <- t(chV) %*% noisex   # Check transpose here
    .traceR(1002, lbl = paste("el=", el))
    .traceR(1001, lbl = paste("Local auxF1() ENDS. el =", el), store = FALSE)
    tx
  }
  res <- lapply(tList, FUN = auxF1)
  .traceR(150, lbl = "lapply ENDS here ***")

  .traceR(160, lbl = "resAll STARTS***", store = FALSE)
  resAll <- NULL 
  for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
  .traceR(160, lbl = "resAll ENDS***")
  .traceR(1, lbl = "simulateY.lme ENDS->")
  return(resAll + fitd0)
}

## -> sigmaTolme function
#' Adjusts the scale parameter of an \code{lme} object
#'
#' Adjusts the scale parameter (\code{sigma}) of an \code{\link[nlme:lme]{lme}} object
#' and rescales variance-related components accordingly. This function is intended
#' for use with \code{\link{Pwr.lme}}. **Warning**: This function modifies the
#' \code{lme} object and should be used cautiously to avoid corrupting the model.
#'
#' @param object an \code{\link[nlme:lme]{lme}} object.
#' @param value numeric scalar specifying the new scale parameter value.
#' @return The modified \code{\link[nlme:lme]{lme}} object with rescaled \code{sigma} and variance components.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @keywords internal
#' @importFrom stats vcov
sigmaTolme <- function(object, value) { 
  .functionName <- "sigmaTolme"
  .traceR <- if (is.null(options()$traceR)) function(...){} else options()$.traceR 

  .traceR(1, lbl = "-> sigmaTolme STARTS")
  sigma0 <- object$sigma 
  val <- value * value
  sc <- sqrt(val) / sigma0  
  object$sigma <- sqrt(val)
  attr(object$fixDF, "varFixFact") <- sc * attr(object$fixDF, "varFixFact") # Rescaled for anova.lme
  .traceR(5, vcov(object) * sc * sc, .functionName)
  object$varFix <- object$varFix * sc * sc  # vcov rescaled  
  .traceR(1, lbl = "sigmaTolme ENDS <-")
  object
}
