#' @title Create Survival Model Explainer
#'
#' @description  Survival models may have very different structures.
#' This function creates a unified representation of a survival model, which can be further processed by various survival explainers.
#'
#' Please NOTE, that the \code{model} is actually the only required argument.
#' But some survival explainers may require that others will be provided too.
#'
#' @param model object - a survival model to be explained
#' @param data data.frame or matrix - data that will be used by survival explainers. If not provided then will be extracted from the model
#' @param y objeect of class surv contains event status and times
#' @param predict_function function that takes three arguments: model, new data, vector with times, and returns numeric vector or matrix with predictions.
#' @param link function - a transformation/link function that shall be applied to raw model predictions
#' @param ... other parameters
#' @param label character - the name of the survival model. By default it's extracted from the 'class' attribute of the model.
#'
#'
#' @return An object of the class 'surv_explainer'.
#'
#' It's a list with following fields:
#'
#' \itemize{
#' \item \code{model} the explained model
#' \item \code{data} the dataset
#' \item \code{y} event statuses and times
#' \item \code{predict_function} function that may be used for model predictions, shall return a single numerical value for each time.
#' \item \code{link} function - a transformation/link function that shall be applied to raw model predictions
#' \item \code{class} class/classes of a model
#' \item \code{label} label, by default it's the last value from the \code{class} vector, but may be set to any character.
#' }
#'
#' @rdname explain
#' @export
#' @importFrom stats predict model.frame
#' @importFrom utils head tail
#'
#'
explain.default <- function(model, data = NULL, y, predict_function = yhat, link = I, ..., label = tail(class(model), 1)) {
  if (is.null(data)) {
    possible_data <- try(model.frame(model), silent = TRUE)
    if (class(possible_data) != "try-error") {
      data <- possible_data
    }
  }

  # if data is in the tibble format then needs to be translated to data.frame
  if ("tbl" %in% class(data)) {
    data <- as.data.frame(data)
  }

  surv_explainer <- list(model = model,
                    data = data,
                    y = y,
                    predict_function = predict_function,
                    link = link,
                    class = class(model),
                    label = label)
  surv_explainer <- c(surv_explainer, list(...))
  class(surv_explainer) <- "surv_explainer"
  surv_explainer
}

#' @export
#' @rdname explain
explain <- explain.default

yhat <- function(X.model, newdata, ...) {
    predict(X.model, newdata, ...)
}
