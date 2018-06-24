#' @title Variable response for survival models
#'
#' @description Function \code{surv_variable response} calculates the expected output condition on a selected variable.
#'
#' @param explainer an object of the class 'surv_explainer'.
#' @param variable character with variable name.
#'
#' @export

surv_variable_response <- function(explainer, variable){
  times <- seq(min(explainer$time), max(explainer$time), length.out = 100)
  values <- unique(explainer$data[ , variable])

  curves_data <- data.frame(time = numeric, prediction = numeric(), value = character())
  for(val in values){
    newdata <- explainer$data
    newdata[ , variable] <- val
    prediction <- explainer$predict_function(explainer$model, newdata, times)
    mean_prediction <- data.frame(time = times, prediction = colMeans(prediction, na.rm=T))
    mean_prediction$value <- val
    curves_data <- rbind(curves_data, mean_prediction)
  }

  class(curves_data) <- "surv_variable_response"
  curves_data
}
