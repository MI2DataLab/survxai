#' Ceteris Paribus
#'
#' @description The \code{ceteris_paribus()} function computes the predictions for the neighbor of our chosen observation. The neighbour is defined as the observations with changed value of one of the variable.
#'
#' @param explainer a model to be explained, preprocessed by the 'survxai::explain' function
#' @param observation a new observation for which predictions need to be explained
#' @param grid_points grid_points number of points used for response path
#' @param selected_variables if specified, then only these variables will be explained
#'
#' @return An object of the class surv_ceteris_paribus_explainer.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom utils head
#'
#' @examples
#' \donttest{
#' library(survxai)
#' library(rms)
#' data("pbcTrain")
#' data("pbcTest")
#' predict_times <- function(model, data, times){
#'                     prob <- rms::survest(model, data, times = times)$surv
#'                     return(prob)
#'                   }
#' cph_model <- cph(Surv(years, status)~ sex + bili + stage, data = pbcTrain, surv = TRUE, x = TRUE, y=TRUE)
#' surve_cph <- explain(model = cph_model, data = pbcTest[,-c(1,5)],
#'              y = Surv(pbcTest$years, pbcTest$status),
#'              predict_function = predict_times)
#' cp_cph <- ceteris_paribus(surve_cph, pbcTest[1,-c(1,5)])
#' }
#' @export

ceteris_paribus <- function(explainer, observation, grid_points = 5, selected_variables = NULL){
  if (!("surv_explainer" %in% class(explainer)))
    stop("The ceteris_paribus() function requires an object created with explain() function from survxai package.")
  if (is.null(explainer$data))
    stop("The ceteris_paribus() function requires explainers created with specified 'data' parameter.")

  data <- base::as.data.frame(explainer$data)
  model <- explainer$model
  predict_function <- explainer$predict_function
  names_to_present <- colnames(data)
  grid_points <- grid_points

  if (!is.null(selected_variables)) {
    names_to_present <- intersect(names_to_present, selected_variables)
  }

  times <- explainer$times
  times <- sort(times)

  responses <- lapply(names_to_present, function(vname, times_s, observation_s, model_s, explainer_s, grid_points_s, data_s, predict_function_s) calculate_responses(vname,times_s = times, observation_s = observation, model_s = model, explainer_s = explainer, grid_points_s = grid_points, data_s = data, predict_function_s = predict_function))

  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation, times)
  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat, times = times)
  attr(all_responses, "grid_points") <- grid_points

  class(all_responses) <- c("surv_ceteris_paribus_explainer", "data.frame")
  all_responses
}
