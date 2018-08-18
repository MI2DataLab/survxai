#' Prediction Breakdown Print
#'
#' @param x the model model of 'surv_prediction_breakdown_explainer' class
#' @param ... other parameters
#' @param digits number of decimal places (round) or significant digits (signif) to be used
#' See the \code{rounding_function} argument
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{signif()} which keeps a specified number of significant digits.
#' Or the default \code{round()} to have the same precision for all components
#'
#' @export
print.surv_prediction_breakdown_explainer <- function(x, ..., digits = 3, rounding_function = round) {
  broken_cumm <- attributes(x)$contribution
  class(broken_cumm) = "data.frame"
  broken_cumm$contribution <- broken_cumm$contribution*100
  broken_cumm$contribution <- rounding_function(broken_cumm$contribution, digits)
  broken_cumm <- broken_cumm[which(abs(broken_cumm$contribution)>=0.01),]
  broken_cumm$contribution <- paste0(broken_cumm$contribution, "%")
  print(broken_cumm[, "contribution", drop=FALSE])
}
