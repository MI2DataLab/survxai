#' @title Create Survival Model Explainer
#'
#' @description  Survival models may have very different structures.
#' This function creates a unified representation of a survival model, which can be further processed by various survival
#' explainers (see also \code{\link[DALEX]{explain}}).
#'
#' Please NOTE, that the \code{model} is actually the only required argument.
#' But some survival explainers may require additional arguments.
#'
#' @param model object - a survival model to be explained
#' @param data data.frame, tibble or matrix - data that will be used by survival explainers. If not provided then will be extracted from the model
#' @param y object of class 'surv', contains event status and times
#' @param times optional argument, the vector of time points on which survival probability will be predicted
#' @param predict_function function that takes three arguments: model, new data, vector with times, and returns numeric vector or matrix with predictions. If not passed, function \code{\link[pec]{predictSurvProb}} is used.
#' @param link function - a transformation/link function that shall be applied to raw model predictions
#' @param label character - the name of the survival model. By default it's extracted from the 'class' attribute of the model.
#' @param ... other parameters
#'
#' @return An object of the class 'surv_explainer'.
#'
#' It's a list with following fields:
#'
#' \itemize{
#' \item \code{model} the explained model
#' \item \code{data} the dataset
#' \item \code{y} event statuses and times
#' \item \code{times} time points on which survival probability is predicted
#' \item \code{predict_function} function that may be used for model predictions, shall return a single numerical value for each time.
#' \item \code{link} function - a transformation/link function that shall be applied to raw model predictions
#' \item \code{class} class/classes of a model
#' \item \code{label} label, by default it's the last value from the \code{class} vector, but may be set to any character.
#' }
#'
#' @rdname explain
#' @importFrom stats predict model.frame
#' @importFrom utils head tail
#'
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
#' }
#' @export

explain.default <- function(model, data = NULL, y, times = NULL, predict_function = yhat, link = I, label = tail(class(model), 1), ...) {
  if (is.null(times)) times <- y[,1]

  if (is.null(data)) {
    possible_data <- try(model.frame(model), silent = TRUE)
    if (class(possible_data) != "try-error") data <- possible_data
  }
  # if data is in the tibble format then needs to be translated to data.frame
  if ("tbl" %in% class(data)) data <- as.data.frame(data)

  surv_explainer <- list(
                      model = model,
                      data = data,
                      y = y,
                      times = times,
                      predict_function = predict_function,
                      link = link,
                      class = class(model),
                      label = label
                    )
  surv_explainer <- c(surv_explainer, list(...))
  class(surv_explainer) <- "surv_explainer"
  attr(surv_explainer, "formula") <- deparse(substitute(y))
  return(surv_explainer)
}


#' @export
#' @rdname explain
explain <- explain.default

#' @method predictSurvProb surv_explainer
#' @export
predictSurvProb.surv_explainer <- function(object, newdata, times, ...) {
  object$predict_function(object$model, newdata, times)
}

yhat <- function(X.model, newdata, times) {
  predictSurvProb(X.model, newdata, times)
}
