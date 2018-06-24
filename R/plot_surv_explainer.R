#' @title Plot for surv_explainer object
#' 
#' @description Function plot for surv_explainer object visualise estimated survival curve of mean probabilities in chosen time points.  
#'
#' @param x object of class "surv_explainer"
#' @param times numeric vector of time points
#' @param ... other arguments
#'
#' @import ggplot2
#' 
#' @method plot surv_explainer
#' @export 


plot.surv_explainer <- function(x, times=1, ...){
  time <- prob <- NULL
   new_pred <- function(model, data){
    x$predict_function(model, data, times)
  }
  
  times <- seq(min(x$time), max(x$time), by=1)
  times <- floor(times)
  predicted_values <- new_pred(x$model, x$data)
  predicted_values <- colMeans(predicted_values, na.rm=T)
  df <- data.frame(time = times, prob = predicted_values)
  plot_explainer <- ggplot(df, aes(time, prob)) + geom_line() + theme_bw()
  plot_explainer
  
} 
