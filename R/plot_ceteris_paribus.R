#' @title Plot for ceteris_paribus object
#'
#' @description Function plot for ceteris_paribus object visualise estimated survival curve of mean probabilities in chosen time points. Black lines on each plot correspond to survival curve for our new observation specified in the \code{ceteris_paribus} function.
#'
#' @param x object of class "surv_ceteris_paribus_explainer"
#' @param selected_variable g
#' @param ... other arguments
#'
#' @import ggplot2
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

plot.surv_ceteris_paribus_explainer <- function(x, selected_variable = NULL, ...) {
  y_hat <- new_x <- time <- NULL
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
  
  if(!is.null(selected_variable)){
    all_responses <- all_responses[which(all_responses$vname == selected_variable),]
    legend <- unique(all_responses$vname)
    add_theme <- labs(col = legend)
    facet <- NULL
  }else{
    add_theme <- theme(legend.position = "none")
    facet <- facet_wrap(~vname)
  }
  
  pl <- ggplot(all_responses, aes(x = time, y = y_hat, col = factor(new_x)))+
    geom_step()+
    geom_step(data = all_predictions, aes(x = time_2, y = y_hat_2), col="black")
  
  pl + facet +
    theme_mi2()+
    add_theme
}
