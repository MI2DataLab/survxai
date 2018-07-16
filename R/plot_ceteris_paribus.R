#' @title Plot for ceteris_paribus object
#'
#' @description Function plot for ceteris_paribus object visualise estimated survival curve of mean probabilities in chosen time points.
#'
#' @param x object of class "surv_ceteris_paribus_explainer"
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

plot.surv_ceteris_paribus_explainer <- function(x, ...) {
  #na razie dziala dla jednego obiektu ceteris paribus
  y_hat <- new_x <- time <- NULL
  dfl <- c(list(x), list(...))
  
  all_responses <- do.call(rbind, dfl)
  class(all_responses) <- "data.frame"
  
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
  
  df <- data.frame(vname = all_responses$vname, new_x = all_responses$new_x)
  df <- unique(df)
  
  pl <- ggplot(all_responses, aes(x = time, y = y_hat, col = factor(new_x)))+
    geom_step()#+
    #geom_text(data = df, aes(label=factor(new_x)), vjust = "inward", hjust = "inward")
  
  pl + facet_wrap(~vname)+
    theme_mi2()+
    theme(legend.position = "none")
  #zaznaczyc na kazdym  malym wykresie nasza obserwacje i dopisac do linii wartosc zamiast legendy
}
