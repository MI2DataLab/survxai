#' @title Plot for surv_model_performance object
#'
#' @description Function plot for surv_model_perfomance object.
#'
#' @param x object of class "surv_model_performance"
#' @param ... other arguments
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library(survxai)
#' library(rms)
#' data("pbcTest")
#' data("pbcTrain")
#' predict_times <- function(model, data, times){
#'                   prob <- rms::survest(model, data, times = times)$surv
#'                   return(prob)
#'                   }
#' cph_model <- cph(Surv(years, status)~., data=pbcTrain, surv=TRUE, x = TRUE, y=TRUE)
#'surve_cph <- explain(model = cph_model, data = pbcTest[,-c(1,5)], 
#'                     y = Surv(pbcTest$years, pbcTest$status), predict_function = predict_times)
#' mp_cph <- model_performance(surve_cph, data = pbcTest)
#' plot(mp_cph)
#' }
#'
#' @method plot surv_model_performance_explainer
#' @export

plot.surv_model_performance_explainer <- function(x, ...){
  time <- err <- label <- NULL

  df <- data.frame(x)
  type <- attributes(x)$type
  if (type == "BS") type <- "Brier Score"

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      class(resp) <- "data.frame"
      df <- rbind(df, resp)
    }
  }


  ggplot(df, aes(x = time, y = err, color = label)) +
    geom_step() +
    labs(title = paste("Prediction Error Curve for", type,"method"),
         x = "time",
         y = "prediction error") +
    theme_mi2()+
    scale_y_continuous(breaks = seq(0,1,0.1),
                       limits = c(0,1),
                       labels = paste(seq(0,100,10),"%"))


}
