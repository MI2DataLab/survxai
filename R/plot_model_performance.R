#' @title Plot for surv_model_performance object
#'
#' @description Function plot for surv_model_perfomance object.
#'
#' @param x object of class "surv_model_performance"
#' @param reference Logical. If TRUE reference level will be plotted.
#' @param ... other arguments
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#'    library(survxai)
#'    library(randomForestSRC)
#'    library(rms)
#'    data(pbc, package = "randomForestSRC")
#'    pbc <- pbc[complete.cases(pbc),]
#'    cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
#'    surve_cph <- explain(model = cph_model, data = pbc[,-c(1,2)], y = Surv(pbc$days/365, pbc$status))
#'    mp_cph <- model_performance(surve_cph, data = pbc, reference_formula = Surv(days/365, status)~1)
#'    plot(mp_cph)
#' }
#'
#' @method plot surv_model_performance_explainer
#' @export

plot.surv_model_performance_explainer <- function(x, reference = TRUE, ...){
  time <- err <- label <- NULL

  df <- data.frame(x)
  type <- attributes(x)$type
  if(type == "BS"){
    type <- "Brier Score"
  }

  if(reference == TRUE) {
    df_ref <- data.frame(time = df$time, err = df$err_ref, err_ref = df$err_ref)
    df_ref$label <- "reference"
    df <- rbind(df, df_ref)
  }

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
                       labels = paste(seq(0,100,10),"%"))


}
