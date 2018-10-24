#' Variable Response Print
#'
#' @param x the model of 'surv_variable_response_explainer' class
#' @param ... further arguments passed to or from other methods
#' 
#' @return a data frame
#'
#' @export

print.surv_variable_response_explainer <- function(x, ...){
  class(x) <- "data.frame"
  print(head(x, ...))
}