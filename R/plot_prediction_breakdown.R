#' @title Plot for surv_breakdown object
#' 
#' @description Function plot for surv_breakdown object visualise estimated survival curve of mean probabilities in chosen time points.  
#'
#' @param x object of class "surv_prediction_breakdown_explainer"
#' @param ... other arguments
#'
#' @import ggplot2
#' 
#' @method plot surv_prediction_breakdown_explainer
#' @export

plot.surv_prediction_breakdown_explainer <- function(x, ...){
  
  df <- data.frame(x)
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      class(resp) <- "data.frame"
      df <- rbind(df, resp)
    }
  }
  
  ggplot(df, aes(x=x, y=y, col = factor(value)))+
    geom_step()+
    labs(title = "BreakDown plot",
        x = "time",
        y = "mean survival probability",
        col =  "variable") +
    facet_wrap(~label) +
    theme_bw()
  
} 


