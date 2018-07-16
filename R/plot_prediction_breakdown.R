#' @title Plot for surv_breakdown object
#'
#' @description Function plot for surv_breakdown object visualise estimated survival curve of mean probabilities in chosen time points.
#'
#' @param x object of class "surv_prediction_breakdown_explainer"
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
#' broken_prediction <- prediction_breakdown(surve_cph, pbc[1,-c(1,2)])
#' plot(broken_prediction)
#' }
#' @method plot surv_prediction_breakdown_explainer
#' @export

plot.surv_prediction_breakdown_explainer <- function(x, ...){
  y <- col <- label <- value <- NULL

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
    theme_mi2()

}


