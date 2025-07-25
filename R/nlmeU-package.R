#' nlmeU: Datasets and Utility Functions Enhancing Functionality of 'nlme' Package
#'
#' Datasets and utility functions enhancing functionality of the \code{nlme} package.
#' Datasets, functions, and scripts are described in the book *Linear Mixed-Effects Models:
#' A Step-by-Step Approach* by Galecki and Burzykowski (2013). The package is under development.
#'
#' Note: The \code{sigma} function in this package masks \code{stats::sigma}.
#' Use \code{nlmeU::sigma} to access this package's version or \code{stats::sigma} for the stats package's version.
#'
#' @name nlmeU-package
#' @aliases nlmeU
#' @title Datasets and Utility Functions Enhancing Functionality of 'nlme' Package
#' @author Andrzej Galecki \email{agalecki@@umich.edu}, Tomasz Burzykowski \email{tomasz.burzykowski@@uhasselt.be}
#' @importFrom nlme varPower Initialize varWeights getVarCov
#' @examples
#' \dontrun{
#'   library(nlmeU)
#'   help(package = "nlmeU")
#' }
#' @references
#'   Galecki, A., & Burzykowski, T. (2013). *Linear Mixed-Effects Models: A Step-by-Step Approach*. Springer.
#' @source
#'   \url{http://www-personal.umich.edu/~agalecki/}
#' @details
#' This package provides datasets and utility functions to complement the \code{nlme} package,
#' including functions like \code{\link{logLik1}}, \code{\link{Pwr}}, \code{\link{simulateY}},
#' and \code{\link{sigma}}. It also includes datasets such as \code{\link{armd}},
#' \code{\link{armd0}}, \code{\link{armd.wide}}, \code{\link{fcat}}, \code{\link{prt.fiber}},
#' \code{\link{prt}}, \code{\link{prt.subjects}}, and \code{\link{SIIdata}}.
#' @seealso \code{\link[nlme:nlme]{nlme}}
#' @keywords package
"_PACKAGE"

#' armd Data (867 x 8)
#'
#' Data from Age-Related Macular Degeneration (ARMD) clinical trial
#'
#' The ARMD data arise from a randomized multi-center clinical trial comparing
#' an experimental treatment (interferon-alpha) versus placebo for patients
#' diagnosed with ARMD.
#'
#' @format A data frame with 867 rows and 8 columns:
#' \describe{
#'   \item{subject}{A factor with 234 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{6}, ..., \code{240}}
#'   \item{treat.f}{A factor with 2 levels: \code{Placebo}, \code{Active}}
#'   \item{visual0}{An integer vector with values ranging from 20 to 85}
#'   \item{miss.pat}{A factor with 8 levels: \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ..., \code{X-XX}}
#'   \item{time.f}{A factor with 4 levels: \code{4wks}, \code{12wks}, \code{24wks}, \code{52wks}}
#'   \item{time}{A numeric vector with values 4, 12, 24, 52}
#'   \item{visual}{An integer vector with values ranging from 3 to 85}
#'   \item{tp}{A numeric vector with values 1, 2, 3, 4 corresponding to time points 4, 12, 24, 52, respectively}
#' }
#' @source Pharmacological Therapy for Macular Degeneration Study Group (1997).
#'   Interferon alpha-IIA is ineffective for patients with choroidal
#'   neovascularization secondary to age-related macular degeneration. Results of
#'   a prospective randomized placebo-controlled clinical trial. Archives of
#'   Ophthalmology, 115, 865-872.
#' @seealso \code{\link{armd0}}, \code{\link{armd.wide}}
#' @keywords datasets
#' @examples
#' data(armd, package = "nlmeU")
#' summary(armd)
#' @export
armd <- NULL

#' armd0 Data (1107 x 8)
#'
#' Data from Age-Related Macular Degeneration (ARMD) clinical trial
#'
#' The ARMD data arise from a randomized multi-center clinical trial comparing an experimental treatment (interferon-alpha)
#' versus placebo for patients diagnosed with ARMD.
#'
#' @format A data frame with 1107 rows and 8 columns:
#' \describe{
#'   \item{subject}{A factor with 240 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ...}
#'   \item{treat.f}{A factor with 2 levels: \code{Placebo}, \code{Active}}
#'   \item{visual0}{An integer vector with values from 20 to 85}
#'   \item{miss.pat}{A factor with 9 levels: \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ...}
#'   \item{time.f}{A factor with 5 levels: \code{Baseline}, \code{4wks}, \code{12wks}, \code{24wks}, \code{52wks}}
#'   \item{time}{A numeric vector with values from 0 to 52}
#'   \item{visual}{An integer vector with values from 3 to 85}
#'   \item{tp}{A numeric vector with values from 0 to 4}
#' }
#' @source Pharmacological Therapy for Macular Degeneration Study Group (1997).
#'   Interferon alpha-IIA is ineffective for patients with choroidal neovascularization secondary to age-related macular degeneration.
#'   Results of a prospective randomized placebo-controlled clinical trial. Archives of Ophthalmology, 115, 865-872.
#' @seealso \code{\link{armd}}, \code{\link{armd.wide}}
#' @keywords datasets
#' @examples
#' data(armd0, package = "nlmeU")
#' summary(armd0)
#' @export
armd0 <- NULL

#' armd.wide Data (240 x 10)
#'
#' Data from Age-Related Macular Degeneration (ARMD) clinical trial
#'
#' The ARMD data arise from a randomized multi-center clinical trial comparing
#' an experimental treatment (interferon-alpha) versus placebo for patients
#' diagnosed with ARMD.
#'
#' @format A data frame with 240 rows and 10 columns:
#' \describe{
#'   \item{subject}{A factor with 240 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{240}}
#'   \item{lesion}{An integer vector with values 1, 2, 3, 4}
#'   \item{line0}{An integer vector with values ranging from 5 to 17}
#'   \item{visual0}{An integer vector with values of visual acuity measured at baseline ranging from 20 to 85}
#'   \item{visual4}{An integer vector with values of visual acuity measured at 4 weeks ranging from 12 to 84}
#'   \item{visual12}{An integer vector with values of visual acuity measured at 12 weeks ranging from 3 to 85}
#'   \item{visual24}{An integer vector with values of visual acuity measured at 24 weeks ranging from 5 to 85}
#'   \item{visual52}{An integer vector with values of visual acuity measured at 52 weeks from 4 to 85}
#'   \item{treat.f}{A factor with 2 levels: \code{Placebo}, \code{Active}}
#'   \item{miss.pat}{A factor with 9 levels: \code{----}, \code{---X}, \code{--X-}, \code{--XX}, \code{-XX-}, ..., \code{XXXX}}
#' }
#' @source Pharmacological Therapy for Macular Degeneration Study Group (1997).
#'   Interferon alpha-IIA is ineffective for patients with choroidal
#'   neovascularization secondary to age-related macular degeneration. Results of
#'   a prospective randomized placebo-controlled clinical trial. Archives of
#'   Ophthalmology, 115, 865-872.
#' @seealso \code{\link{armd}}, \code{\link{armd0}}
#' @keywords datasets
#' @examples
#' data(armd.wide, package = "nlmeU")
#' summary(armd.wide)
#' @export
armd.wide <- NULL

#' fcat Data (4851 x 3)
#'
#' Data from Flemish Community Attainment-Targets (FCAT) Study
#'
#' An educational study, in which elementary school graduates were evaluated
#' with respect to reading comprehension in Dutch. Pupils from randomly selected
#' schools were assessed for a set of nine attainment targets. The dataset is an
#' example of grouped data, for which the grouping factors are crossed.
#'
#' @format A data frame with 4851 rows and 3 columns:
#' \describe{
#'   \item{target}{A factor with 9 levels: \code{T1(4)}, \code{T2(6)}, \code{T3(8)}, \code{T4(5)}, \code{T5(9)}, ..., \code{T9(5)}}
#'   \item{id}{A factor with 539 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{539}}
#'   \item{scorec}{An integer vector with values from 0 to 9}
#' }
#' @source Janssen, R., Tuerlinckx, F., Meulders, M., & De Boeck, P. (2000).
#'   A hierarchical IRT model for criterion-referenced measurement. Journal of
#'   Educational and Behavioral Statistics, 25(3), 285.
#' @keywords datasets
#' @examples
#' data(fcat, package = "nlmeU")
#' summary(fcat)
#' @export
fcat <- NULL

#' prt.fiber Data (2471 x 5)
#'
#' Data from a Progressive Resistance Randomized Trial.
#'
#' PRT trial was aimed for devising evidence-based methods for improving and
#' measuring the mobility and muscle power of elderly men and women.
#'
#' @format A data frame with 2471 rows and 5 columns:
#' \describe{
#'   \item{id}{A factor with 63 levels: \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ..., \code{520}}
#'   \item{iso.fo}{A numeric vector with values of isometric force ranging from 0.16 to 2.565}
#'   \item{spec.fo}{A numeric vector with values of specific force ranging from 80.5 to 290}
#'   \item{occ.f}{A factor with 2 levels: \code{Pre}, \code{Pos}, i.e., pre- and post-intervention}
#'   \item{fiber.f}{A factor with 2 levels: \code{Type 1}, \code{Type 2}, i.e., Type 1 and Type 2 muscle fiber}
#' }
#' @source Claflin, D.R., Larkin, L.M., Cederna, P.S., Horowitz, J.F.,
#'   Alexander, N.B., Cole, N.M., Galecki, A.T., Chen, S., Nyquist, L.V., Carlson,
#'   B.M., Faulkner, J.A., & Ashton-Miller, J.A. (2011). Effects of high- and
#'   low-velocity resistance training on the contractile properties of skeletal
#'   muscle fibers from young and older humans. Journal of Applied Physiology, 111, 1021-1030.
#' @seealso \code{\link{prt}}, \code{\link{prt.subjects}}
#' @keywords datasets
#' @examples
#' data(prt.fiber, package = "nlmeU")
#' summary(prt.fiber)
#' @export
prt.fiber <- NULL

#' prt Data (2471 x 9)
#'
#' Data from a Progressive Resistance Randomized Trial.
#'
#' Data frame \code{prt} was obtained by merging \code{prt.subjects} and \code{prt.fiber}.
#'
#' @format A data frame with 2471 rows and 9 columns:
#' \describe{
#'   \item{id}{A factor with 63 levels: \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ..., \code{520} (subject id)}
#'   \item{prt.f}{A factor with 2 levels: \code{High}, \code{Low}, i.e., training (intervention) intensity}
#'   \item{age.f}{A factor with 2 levels: \code{Young}, \code{Old} (stratifying variable)}
#'   \item{sex.f}{A factor with 2 levels: \code{Female}, \code{Male} (stratifying variable)}
#'   \item{bmi}{A numeric vector with values of BMI at baseline ranging from 18.36 to 32.29}
#'   \item{iso.fo}{A numeric vector with values of isometric force ranging from 0.16 to 2.565}
#'   \item{spec.fo}{A numeric vector with values of specific force ranging from 80.5 to 290}
#'   \item{occ.f}{A factor with 2 levels: \code{Pre}, \code{Pos}, i.e., pre- and post-intervention}
#'   \item{fiber.f}{A factor with 2 levels: \code{Type 1}, \code{Type 2}, i.e., Type 1 and Type 2 muscle fiber}
#' }
#' @source Claflin, D.R., Larkin, L.M., Cederna, P.S., Horowitz, J.F.,
#'   Alexander, N.B., Cole, N.M., Galecki, A.T., Chen, S., Nyquist, L.V., Carlson,
#'   B.M., Faulkner, J.A., & Ashton-Miller, J.A. (2011). Effects of high- and
#'   low-velocity resistance training on the contractile properties of skeletal
#'   muscle fibers from young and older humans. Journal of Applied Physiology, 111, 1021-1030.
#' @seealso \code{\link{prt.fiber}}, \code{\link{prt.subjects}}
#' @keywords datasets
#' @examples
#' data(prt, package = "nlmeU")
#' summary(prt)
#' @export
prt <- NULL

#' prt.subjects Data (63 x 5)
#'
#' Data from a Progressive Resistance Randomized Trial.
#'
#' The working hypothesis was that a 12-week program of PRT would increase:
#' (a) the power output of the overall musculature associated with movements of the ankles, knees, and hips;
#' (b) the cross-sectional area and the force and power of permeabilized single fibers obtained from the vastus lateralis muscle; and
#' (c) the ability of young and elderly men and women to safely arrest standardized falls. The training consisted of repeated leg extensions by
#' shortening contractions of the leg extensor muscles against a resistance that was increased as the subject trained using a specially designed apparatus.
#'
#' @format A data frame with 63 rows and 5 columns:
#' \describe{
#'   \item{id}{A factor with 63 levels: \code{5}, \code{10}, \code{15}, \code{20}, \code{25}, ...}
#'   \item{prt.f}{A factor with 2 levels: \code{High}, \code{Low}}
#'   \item{age.f}{A factor with 2 levels: \code{Young}, \code{Old}}
#'   \item{sex.f}{A factor with 2 levels: \code{Female}, \code{Male}}
#'   \item{bmi}{A numeric vector with values from 18.4 to 32.3}
#' }
#' @source Claflin, D.R., Larkin, L.M., Cederna, P.S., Horowitz, J.F.,
#'   Alexander, N.B., Cole, N.M., Galecki, A.T., Chen, S., Nyquist, L.V., Carlson,
#'   B.M., Faulkner, J.A., & Ashton-Miller, J.A. (2011). Effects of high- and
#'   low-velocity resistance training on the contractile properties of skeletal
#'   muscle fibers from young and older humans. Journal of Applied Physiology, 111, 1021-1030.
#' @keywords datasets
#' @examples
#' data(prt.subjects, package = "nlmeU")
#' summary(prt.subjects)
#' @export
prt.subjects <- NULL

#' SIIdata Data (1190 x 12)
#'
#' Data from Study of Instructional Improvement Project
#'
#' The SII Project was carried out to assess the math achievement scores of
#' first- and third-grade pupils in randomly selected classrooms from a national
#' US sample of elementary schools (Hill et al, 2005). Data were also analyzed
#' in West et al, 2007. The outcome of interest is \code{mathgain} variable.
#' Data were created based on \code{classroom} data from \code{WWGbook} package.
#'
#' @format A data frame with 1190 rows and 12 columns:
#' \describe{
#'   \item{sex}{A factor with 2 levels: \code{M}, \code{F}, i.e., males and females, respectively}
#'   \item{minority}{A factor with 2 levels: \code{Mnrt=No}, \code{Mnrt=Yes}. An indicator variable for the minority status}
#'   \item{mathkind}{An integer vector with values from 290 to 629. This is pupil's math score in the spring of the kindergarten year}
#'   \item{mathgain}{An integer vector with values from -110 to 253. Number represents pupil's gain in the math achievement score
#'     from the spring of kindergarten to the spring of first grade}
#'   \item{ses}{A numeric vector with values from -1.61 to 3.21. Value represents socioeconomic status}
#'   \item{yearstea}{A numeric vector with values from 0 to 40. It is number of years of teacher's experience in teaching in the first grade}
#'   \item{mathknow}{A numeric vector with values from -2.5 to 2.61. Number represents teacher's knowledge of the first-grade math contents (higher
#'     values indicate a higher knowledge of the contents)}
#'   \item{housepov}{A numeric vector containing proportion of households in the neighborhood of
#'     the school below the poverty level with values ranging from 0.012 to 0.564}
#'   \item{mathprep}{A numeric vector with values from 1 to 6. Contains the
#'     number of preparatory courses on the first-grade math contents and methods followed by the teacher}
#'   \item{classid}{A factor with 312 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{312}. Classroom's id}
#'   \item{schoolid}{A factor with 107 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{107}. School's id}
#'   \item{childid}{A factor with 1190 levels: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, ..., \code{1190}. Pupil's id}
#' }
#' @source Hill, H., Rowan, B., and Ball, D. (2005). Effect of teachers' mathematical knowledge for teaching on student achievement. American
#' Educational Research Journal, 42, 371-406.
#' West, B. T., Welch, K. B., and Galecki, A. T. (2007). Linear Mixed Models: A Practical Guide Using Statistical Software. Chapman and Hall/CRC.
#' @keywords datasets
#' @examples
#' data(SIIdata, package = "nlmeU")
#' summary(SIIdata)
#' @export
SIIdata <- NULL
