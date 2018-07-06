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
  pred <- pred_model <- pred_new <- prob <- time <- start<- stop <- NULL 
  
  
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
  
  
  break_plot <- ggplot() + geom_line(data = df, aes(time, prob, colour = pred)) + theme_bw()
  
  # for(i in x$times){
  #   
  #   df_broken <- x$broken_list[[i]]
  #   class(df_broken) <- "data.frame"
  #   
  #   
  #   df_cummulative <- data.frame(start = df_broken$cummulative[-nrow(df_broken)], end = df_broken$cummulative[-1], sign = df_broken$sign[-1], variable = df_broken$variable[-1])
  #   df_cummulative <- df_cummulative[-nrow(df_cummulative),]
  #   df_cummulative$x <- x$times[i] + (0.2 * as.numeric(as.character(df_cummulative$sign)))
  #   
  #   break_plot <- break_plot + geom_segment(data = df_cummulative, aes(xend = x, yend = end, x = x, y = start, col = sign), lineend = "butt", size = 5)+ 
  #     geom_segment(data = df_cummulative, aes(x = x-0.01, y = end, xend = x+0.01, yend = end, col="black"))#+
  #     #geom_label(data = df_cummulative, aes(label=str_wrap(variable,12), x = x+((max(x)-min(x))/4), col = sign, y=(start + end)/2), size=2)
  # }
  
  df_lines <- sapply(x$broken_list, function(x) x$cummulative[c(1,length(x$cummulative))])
  df_lines <- as.data.frame(t(df_lines))
  colnames(df_lines) <- c("start", "stop")
  df_lines$time <- x$times

  break_plot <- break_plot + geom_segment(data = df_lines, aes(x = time, y = start, 
                                                 xend = time, 
                                                 yend = stop))
  
  times_broken <- x$times
  
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
    grid.arrange(break_plot, g_contribution, g, ncol = 2, widths = c(3,1), heights = c(4,1))
    
  }else{
    
    break_plot
  
    
  }
  
} 

#hlay = rbind(c(1,1), c(2,3))
#grid.arrange(break_plot, plot(x$broken_list$time_1), plot(x$broken_list$time_2), layout_matrix = hlay)
