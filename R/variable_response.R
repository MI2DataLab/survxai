#' @title Variable response for survival models
#'
#' @description Function \code{variable response} calculates the expected output condition on a selected variable.
#'
#' @param explainer an object of the class 'surv_explainer'.
#' @param variable character with variable name.
#' @param type character - type of the response to be calculated.
#' Currently following options are implemented: 'pdp' for Partial Dependency.
#' @param trans function - a transformation/link function that shall be applied to raw model predictions. This will be inherited from the explainer.
#' @param ... other parameters
#'
#' @export

variable_response <- function(explainer, variable, type = "pdp", trans = explainer$link, ...){
  if (!("surv_explainer" %in% class(explainer))) stop("The variable_response() function requires an object created with explain() function from survxai package.")
  if (is.null(explainer$data)) stop("The variable_response() function requires explainers created with specified 'data' parameter.")

  switch(type,
         pdp = {
           res <- surv_partial(explainer, variable)
           class(res) <- c("surv_variable_response_explainer", "data.frame", "pdp")
           res
         },
         stop("Currently only 'pdp' method is implemented"))
}




surv_partial <- function(explainer, variable){
  times <- sort(explainer$y[,1])
  tmp_data <- explainer$data
  values <- unique(explainer$data[,variable])

  partial_data <- data.frame(x = numeric(), y = numeric(), value = character())

  for(val in values){
    tmp_data[,variable] <- val
    prediction <- explainer$predict_function(explainer$model, tmp_data, times)
    mean_prediction <- data.frame(x = times, y = colMeans(prediction, na.rm=T))
    mean_prediction <- rbind(mean_prediction, c(0, 1))
    mean_prediction$value <- val
    partial_data <- rbind(partial_data, mean_prediction)
  }
  partial_data$type <- "pdp"
  partial_data$label <- explainer$label
  partial_data$var <- variable
  partial_data
}


