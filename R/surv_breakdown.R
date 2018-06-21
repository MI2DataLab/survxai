#' @title BreakDown for survival models
#'
#' @description Function \code{surv_breakdown} is an extension of a broken function from breakDown package.
#'
#' @param explainer an object of the class 'surv_explainer'
#' @param new_observation a new observation to explain
#' @param times numeric vector with event times.
#'
#' @importFrom breakDown broken
#'
#' @export


surv_breakdown <- function(explainer, new_observation, times = 1){
  broken_in_time <- list()
  
  for(i in 1:length(times)){
    time <- times[i]
    new_pred <- function(model, data){
      explainer$predict_function(model, data, time)
    }
    
    broken_out <- broken(explainer$model, new_observation, explainer$data, predict.function = new_pred)
    broken_in_time[[i]] <- broken_out
  }
  
  names(broken_in_time) <- paste0("time_",times)
  
  broken_list <- list(new_observation = new_observation, surv_explainer = explainer, broken_list = broken_in_time)
  class(broken_list) <- "surv_breakdown"
  return(broken_list)
}