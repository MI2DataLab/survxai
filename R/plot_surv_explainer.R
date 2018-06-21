#' @title Plot for surv_explainer object
#' 
#' @description Function plot for surv_explainer object visualise estimated survival curve of mean probabilities in chosen time points.  
#'
#' @param x object of class "surv_explainer"
#' @param ... other arguments
#'
#'
#' @export

plot.surv_explainer <- function(x, times=1, ...){
   new_pred <- function(model, data){
    x$predict_function(model, data, times)
  }
  
  times <- seq(min(x$time), max(x$time), by=1)
  predicted_values <- new_pred(x$model, x$data)
  predicted_values <- colMeans(predicted_values, na.rm=T)
  plot(predicted_values, type="l")
  
  
} 
