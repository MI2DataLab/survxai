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
#'
#' @method plot surv_explainer
#' @export


plot.surv_explainer <- function(x, ...){
  fit <- survfit(x$model, data = x$data)
  ggsurvplot(fit, ...)

}
