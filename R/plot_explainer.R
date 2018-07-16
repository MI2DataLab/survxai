#' @title Plot for surv_explainer object
#'
#' @description Function plot for surv_explainer object visualise estimated survival curve of mean probabilities in chosen time points.
#'
#' @param x object of class "surv_explainer"
#' @param ... other arguments for function \code{\link[survminer]{ggsurvplot}}
#'
#' @import ggplot2
#' @importFrom survival survfit
#' @importFrom survminer ggsurvplot
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
#' plot(surve_cph)
#' }
#' @method plot surv_explainer
#' @export


plot.surv_explainer <- function(x, ...){
  fit <- survfit(x$model, data = x$data)
  ggsurvplot(fit, ...)

}
