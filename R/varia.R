# Commented out nlmeU::sigma as it duplicates stats::sigma (2025-07-25)
# #' Extract scale parameter sigma from a model fit
# #' @param object an object from which the scale parameter sigma is to be extracted.
# #' @param ... additional arguments passed to methods.
# #' @return Numeric scalar representing the scale parameter sigma.
# #' @examples
# #' library(nlme)
# #' fm1 <- lme(distance ~ age, data = Orthodont)
# #' sigma(fm1)
# #' @export
# sigma <- function(object, ...) UseMethod("sigma")
#
# #' Default method for extracting scale parameter sigma
# #' @param object an object from which the scale parameter sigma is to be extracted.
# #' @param ... additional arguments (ignored).
# #' @return Numeric scalar representing the scale parameter sigma, or NULL if not available.
# #' @method sigma default
# #' @export
# sigma.default <- function(object, ...) object$sigma


# ## -> sigma.default method
# #' Default method for extracting scale parameter sigma
# #'
# #' This is a method for the \code{\link{sigma}} generic function.
# #'
# #' @method sigma default
# #' @export
# #' @param object an object for which the scale parameter can be extracted.
# #' @param \dots additional arguments passed to the method.
# #' @return Numeric scalar value representing the scale parameter.
# #' @author Andrzej Galecki and Tomasz Burzykowski
# sigma.default <- function(object, ...) object$sigma

## -> missPat function
#' Extract pattern of missing data
#'
#' This function compactly presents the pattern of missing data in a given vector, matrix, or data frame.
#'
#' @export
#' @param \dots one or more vectors, matrices, or data frames, compatible for column-wise binding.
#' @param symbols vector containing two single characters used to indicate \code{NA} and non-\code{NA} values. By default, \code{c("X", "-")}.
#' @param collapse an optional character string used in the internal call to \code{paste()} to separate results. By default, \code{""}.
#' @param missData logical. If \code{TRUE}, a data frame with the pattern of missing values is saved in the \code{missData} attribute of the returned vector. By default, \code{FALSE}.
#' @return Character vector with as many elements as the length of vector(s) or number of rows in matrices/data frames in \code{\dots}. 
#'   Attribute \code{cnames} contains names of vectors/columns/variables. 
#'   Optional attribute \code{missData} contains a data frame with the missing pattern.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @examples
#' \dontrun{
#'   data(armd.wide, package = "nlmeU")
#'   dtf <- armd.wide[ , c("visual12", "visual24", "visual52")]
#'   missPat(dtf, symbols = c("?", "+"))
#' }
#' @seealso \code{\link{armd.wide}}
missPat <- function(..., symbols = c("X", "-"), collapse = "", missData = FALSE) {
  .functionName <- "missPat"                                # Function name
  .traceR <- if (is.null(options()$traceR)) function(...){} else options()$.traceR       

  .traceR(1, "-> missPat STARTS")
  args <- as.list(substitute(list(...)))[-1]
  argsL <- lapply(args, eval)
  dt <- data.frame(argsL)
  nms <- lapply(args, FUN = function(el) {
    elx <- eval(el)
    if (is.null(colnames(elx))) {
      nc <- ncol(elx)
      if (is.null(nc)) as.character(el) else paste(el, 1:nc, sep = ":")
    } else colnames(elx)
  })
  cx1 <- symbols[1]
  cx2 <- symbols[2]
  miss.frame <- as.data.frame(ifelse(is.na(dt), cx1, cx2))
  names(miss.frame) <- unlist(nms)
  res <- apply(miss.frame, 1, paste, collapse = collapse)
  attr(res, "cnames") <- unlist(nms)
  if (missData) attr(res, "missData") <- miss.frame
  .traceR(1, "missPat ENDS <-")
  res
}

## -> runScript function
#' Execute scripts from Galecki and Burzykowski (2013)
#'
#' Executes scripts from the book by Galecki and Burzykowski (2013). If called without arguments, it prints a list of available scripts.
#'
#' @export
#' @param script character string containing the name of the script to be executed. By default, \code{NA}.
#' @param package character string containing the package name. By default, \code{"nlmeU"}.
#' @param subdir subdirectory containing scripts. By default, \code{"scriptsR2.15.0"}.
#' @param echo logical. If \code{TRUE}, the script is executed with output printed. Used by \code{source()}. By default, \code{TRUE}.
#' @return The script is executed, and results are printed. If \code{script} is \code{NA}, a list of available scripts is printed.
#' @author Andrzej Galecki and Tomasz Burzykowski
#' @references
#'   Galecki, A., & Burzykowski, T. (2013). *Linear Mixed-Effects Models: A Step-by-Step Approach*. Springer.
#' @examples
#' runScript()
runScript <- function(script = NA, package = "nlmeU", subdir = "scriptsR2.15.0", echo = TRUE) {
  scriptsDir <- system.file(subdir, package = package)
  scriptsList <- list.files(scriptsDir, pattern = "[[:alnum:]][.][R]$")
  scriptFile <- file.path(scriptsDir, script)
  if (!(script %in% scriptsList)) {
    if (is.na(script)) {
      errFun <- message
      errMsg <- ""
    } else {
      errFun <- stop
      errMsg <- paste("Example", script, "does not exist. ")
    }
    errFun(errMsg, "Scripts in ", scriptsDir, " are: \n", paste("\"", scriptsList, 
                                                                collapse = "\", \n", sep = ""), "\"")
    if (subdir == "scriptsR2.15.0")
      cat('\n Scripts employing lme4.0 package are stored in: \n',
          file.path(scriptsDir, "lme4.0"), ' directory',
          ' and can be found by issuing:\n   runScript(subdir = "scriptsR2.15.0/lme4.0") command \n')         
  } else {
    sourceText <- source(scriptFile, echo = echo)
    sourceText
  }
}
