#'  Ratio of robin males to females in Walker Creek and Knobs Flat, Eglinton Valley
#'
#'  Data from Table 1 of "Birds: incomplete counts—five-minute bird counts Version 1.0"
#'
#'
#' @format ## `robins`
#' A data frame with 2 observations and 6 variables:
#' \describe{
#'   \item{site}{Site name}
#'   \item{aug_05, aug_06, aug_07, aug_08, aug_09}{ratio of male to female robins}
#' }
#' @source <https://www.doc.govt.nz/documents/science-and-technical/inventory-monitoring/im-toolbox-birds-incomplete-five-min-counts.pdf>
"robins"


#'  Rat Weight Data from HLM manual
#'
#'  Data obtained from Rogosa & Saner (1995) which describes the data as:
#'  "Example 1. The rat weight data are taken from the HLM manual (Bryk et al., 1989).
#'  The rat data consist of 10 individuals, with weight measurements (Y) at five
#'  occasions (Weeks 0, 1,2, 3, 4) and a background measure, the mother's weight (Z)."
#'
#' @format ## `rats`
#' A data frame with 10 observations and 7 variables:
#' \describe{
#'   \item{Rat}{Rat identifier}
#'   \item{t0, t1, t2, t3, t4}{Week of weight measure}
#'   \item{Z}{Mother's weight}
#' }
#'
#' @references
#' Bryk, A. S., Raudenbush, S. W., Seltzer, M., & Congdon, R. T. (1989).
#' An introduction to HLM: Computer program and user's guide. Chicago: University of Chicago.
#' \doi{10.1201/9780429246593}
#'
#' Rogosa, D., & Saner, H. (1995). Longitudinal Data Analysis Examples with Random
#' Coefficient Models. Journal of Educational and Behavioral Statistics, 20(2), 149-170.
#' \doi{10.3102/10769986020002149}
#'
#' @source <https://www.taylorfrancis.com/books/mono/10.1201/9780429246593/introduction-bootstrap-bradley-efron-tibshirani>
"rats"
