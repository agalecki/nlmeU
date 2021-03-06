\name{Pwr.lme}
\alias{Pwr.lme}
\title{Performs power calculations}
\usage{
  \method{Pwr}{lme} (object, ...,
    type = c("sequential", "marginal"), Terms, L,
    verbose = FALSE, sigma, ddf = numeric(0), alpha = 0.05,
    altB = NULL, tol = 1e-10)
}
\arguments{
  \item{object}{an object containing \code{lme} fit, which
  provides information needed for power calculations}

  \item{\dots}{some additional arguments may be required.}

  \item{type}{an optional character string specifying the
  type of sum of squares to be used in F-tests needed for
  power calculations. Syntax is the same as for
  \code{anova.lme()} in \code{nlme} package.}

  \item{Terms}{an optional integer or character vector
  specifying which terms in the model should be jointly
  tested to be zero using a Wald F-test. See
  \code{anova.lme} in \code{nlme} package for details.}

  \item{L}{an optional numeric vector or array specifying
  linear combinations of the coefficients in the model that
  should be tested to be zero. See \code{anova.lme} in
  \code{nlme} package for details.}

  \item{verbose}{an optional logical value. See
  \code{anova.lme} in nlme package for details.}

  \item{sigma}{numeric scalar value.}

  \item{ddf}{numeric scalar value. Argument can be used to
  redefine default number of denominator degrees of
  freedom}

  \item{alpha}{numeric scalar value. By default 0.05.}

  \item{altB}{matrix/vector containing alternative values
  for beta parameters}

  \item{tol}{numeric scalar value.}
}
\value{
  a data frame inheriting from class Pwr.lme
}
\description{
  This is method for \code{Pwr()} generic function. It
  works fine for an example given in the book. It may
  require additional testing, especially for post-hoc power
  analysis
}
\author{
  Andrzej Galecki and Tomasz Burzykowski
}
\seealso{
  \code{\link{anova.lme}}
}

