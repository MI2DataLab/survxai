#' @title Plot for surv_breakdown object
#' 
#' @description Function plot for surv_breakdown object visualise estimated survival curve of mean probabilities in chosen time points.  
#'
#' @param x object of class "surv_breakdown"
#' @param table if we want to plot the table of contributions
#' @param ... other arguments
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange tableGrob ttheme_minimal
#' @importFrom tidyr gather
#' 
#' @method plot surv_breakdown
#' @export

plot.surv_breakdown <- function(x, table = FALSE, ...){
  pred <- pred_model <- pred_new <- prob <- time <- NULL 
  
  
  new_pred <- function(model, data){
    x$surv_explainer$predict_function(model, data, times)
  }
  
  times <- seq(min(x$surv_explainer$time), max(x$surv_explainer$time), by=1)
  times <- floor(times)
  predicted_values_model <- new_pred(x$surv_explainer$model, x$surv_explainer$data)
  predicted_values_model <- colMeans(predicted_values_model, na.rm=T)
  
  predicted_values_new <- new_pred(x$surv_explainer$model, x$new_observation)
  predicted_values_new <- predicted_values_new[1,]
  
  df <- data.frame(time = times, pred_model = predicted_values_model, pred_new = predicted_values_new)
  df <- gather(df, pred, prob, pred_model:pred_new)
  
  break_plot <- ggplot(df, aes(time, prob, colour = pred)) + geom_line() + theme_bw()

  times_broken <- as.numeric(substr(names(x$broken_list), 6, 6))
  
  times <- times_broken
  df_lines <- data.frame(time = times_broken, pred_model = colMeans(new_pred(x$surv_explainer$model, x$surv_explainer$data), na.rm=T), 
                         pred_new = new_pred(x$surv_explainer$model, x$new_observation)[1,])
  
  if(table == TRUE){
    
    df_predicted <- t(df_lines)[-1,]
    colnames(df_predicted) <- names(x$broken_list)
    
    df_contributions <- as.data.frame(as.character(x$broken_list[[1]]$variable))
    for(i in 1:length(x$broken_list)){
      contributions <-  x$broken_list[[i]]$contribution
      df_contributions <- cbind(df_contributions, contributions)
      
    }
    
    colnames(df_contributions) <- c("variable", paste0("contribution in ", names(x$broken_list)))
    
    
    g <- tableGrob(df_predicted, theme = ttheme_minimal())
    g_contribution <- tableGrob(df_contributions, theme = ttheme_minimal())
    grid.arrange(break_plot, g_contribution, g, ncol = 2, widths = c(2,1), heights = c(4,1))
    
  }else{
    
    break_plot
  
  }
} 
