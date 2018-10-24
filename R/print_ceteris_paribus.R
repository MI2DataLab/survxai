#' Ceteris Paribus Print
#'
#' @param x the model of 'surv_ceteris_paribus_explainer' class
#' @param ... further arguments passed to or from other methods
#' 
#' @return a data frame
#'
#' @export

print.surv_ceteris_paribus_explainer <- function(x, ...){
  class(x) <- "data.frame"
  print(head(x, ...))
}