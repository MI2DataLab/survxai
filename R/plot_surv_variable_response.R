#' @title Plot for surv_variable_serponse object
#'
#' @description Function plot for surv_variable_response object shows the expected output condition on a selected variable.
#'
#' @param x object of class "surv_variable_response"
#' @param ... other arguments
#'
#' @import ggplot2
#'
#' @method plot surv_variable_response
#' @export

plot.surv_variable_response <- function(x, ...){
  time <- prediction <- value <- NULL

  class(x) <- "data.frame"
  ggplot(x, aes(x = time, y = prediction, colour = as.factor(value))) +
    geom_line() +
    theme_bw() +
    xlab("mean survival proablility")

}
