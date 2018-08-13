#' @title Plot for ceteris_paribus object
#'
#' @description Function plot for ceteris_paribus object visualise estimated survival curve of mean probabilities in chosen time points. Black lines on each plot correspond to survival curve for our new observation specified in the \code{ceteris_paribus} function.
#'
#' @param x object of class "surv_ceteris_paribus_explainer"
#' @param selected_variable name of varaible we want to draw ceteris paribus plot
#' @param ... other arguments
#' @param scale_type type of scale of colors, either "discrete" or "gradient"
#' @param scale_col vector containing values of low and high ends of the gradient, when "gradient" type of scale was chosen
#' @param ncol number of columns for faceting
#'
#' @import ggplot2
#' @importFrom scales seq_gradient_pal
#' @examples
#' \dontrun{
#' library(survxai)
#' library(rms)
#' library(randomForestSRC)
#' data(pbc, package = "randomForestSRC")
#' pbc <- pbc[complete.cases(pbc),]
#' predict_times <- function(model, data, times){
#'                   prob <- rms::survest(model, data, times = times)$surv
#'                   return(prob)
#'                   }
#' cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
#' surve_cph <- explain(model = cph_model, data = pbc[,-c(1,2)], y = Surv(pbc$days/365, pbc$status),
#'              predict_function = predict_times)
#' cp_cph <- ceteris_paribus(surve_cph, pbc[1,-c(1,2)])
#' plot(cp_cph)
#' }
#' @method plot surv_ceteris_paribus_explainer
#' @export

plot.surv_ceteris_paribus_explainer <- function(x, ..., selected_variable = NULL, scale_type = "factor", 
                                                scale_col = NULL, ncol = 1) {
  y_hat <- new_x <- time <- time_2 <- y_hat_2 <- NULL
  new_observation <- attributes(x)$prediction$`observation`
  values <- as.data.frame(t(new_observation[1,]))
  values[,1] <- as.character(values[,1])
  new_observation_legend <- data.frame(vname = colnames(new_observation), val = paste0(colnames(new_observation), "=", values[,1]))
  seq_length <- attributes(x)$grid_points
  
  
  dfl <- c(list(x), list(...))

  all_responses <- do.call(rbind, dfl)
  class(all_responses) <- "data.frame"


  if(!is.null(selected_variable) && !(selected_variable %in% factor(all_responses$vname))){
    stop(paste0("Selected variable ", selected_variable, "not present in surv_ceteris_paribus object."))
  }

  all_predictions <- lapply(dfl, function(tmp) {
    pred <- attr(tmp, "prediction")
    data.frame(prediction = pred$new_y_hat,
               label = tmp$label[1])
  })

  times <- lapply(dfl, function(tmp) {
    pred <- attr(tmp, "prediction")
    data.frame(prediction = pred$times)
  })

  times <- do.call(rbind, times)
  all_predictions <- do.call(rbind, all_predictions)
  all_predictions <- data.frame(t(all_predictions[,-ncol(all_predictions)]))

  all_predictions$time_2 <- times$prediction
  colnames(all_predictions)[1] <- "y_hat_2"

  
  all_responses <- merge(all_responses, new_observation_legend, by="vname")
  if(!is.null(selected_variable)){
    all_responses <- all_responses[which(all_responses$vname == selected_variable),]
    legend <- unique(all_responses$val)
    add_theme <- labs(col = legend)  
    facet <- NULL
    title <- ggtitle(paste("Ceteris paribus plot for variable", selected_variable,"."))
  }else{
    add_theme <- theme(legend.position = "none")
    title <- ggtitle(paste("Ceteris paribus plot for", unique(x$label),"model."))
    facet <- facet_wrap(~val, ncol = ncol)
  }
  
  #######################
  z <- all_responses[,c(1,3)]
  z <- unique(z)
  z$legend <- 1:nrow(z)
  
  all_responses <- merge(all_responses, z, by=c("vname", "new_x"))
  
  #############################
  if(scale_type == "gradient"){
    if(!is.null(scale_col)){
      variables <- unique(all_responses$vname)
      v<- c()
      for(val in variables){
        length <- length(unique(all_responses[all_responses$vname==val,2]))
        cc <- seq_gradient_pal(scale_col[1],scale_col[2])(seq(0,1,length.out=length))
        v <- c(v,cc)
      }
      if(!is.null(selected_variable)){
        scale <- scale_colour_manual(values = v, labels = factor(unique(all_responses$new_x)))
      }else{
      scale <- scale_colour_manual(values=v)
      }
    }else{
      message("Please specify the low and high ends of gradient")
      scale <- NULL
    }
  }else{
    scale <- NULL
  }

    
  
  pl <- ggplot(all_responses, aes(x = time, y = y_hat, col = factor(legend)))+
    geom_step()+
    geom_step(data = all_predictions, aes(x = time_2, y = y_hat_2), col="black", lty = 2, size = 1) +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       limits = c(0,1),
                       labels = paste(seq(0,100,10),"%"),
                       name = "survival probability")

  pl <- pl + facet +
    theme_mi2()+
    add_theme+
    title
  
  pl + scale
}
