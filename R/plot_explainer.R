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
#' \donttest{
#' library(survxai)
#' library(rms)
#' data("pbcTest")
#' data("pbcTrain")
#' predict_times <- function(model, data, times){
#'                   prob <- rms::survest(model, data, times = times)$surv
#'                   return(prob)
#'                   }
#' cph_model <- cph(Surv(years, status)~sex + bili + stage, data=pbcTrain, surv=TRUE, x = TRUE, y=TRUE)
#' surve_cph <- explain(model = cph_model, data = pbcTest[,-c(1,5)],
#'              y = Surv(pbcTest$years, pbcTest$status), predict_function = predict_times)
#' plot(surve_cph)
#' }
#' @method plot surv_explainer
#' @export


plot.surv_explainer <- function(x, ...){
  fit <- survfit(x$model, data = x$data)
  ggsurvplot(fit, data = x$data,...)

}
