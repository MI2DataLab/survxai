#' @title BreakDown for survival models
#'
#' @description Function \code{surv_breakdown} is an extension of a broken function from breakDown package.
#'
#' @param explainer an object of the class 'surv_explainer'
#' @param observation a new observation to explain
#' @param time time point at which variable contributions are computed. If NULL median time is taken.
#' @param prob survival probability at which variable contributions are computed
#' @param ... other parameters
#'
#' @return An object of class surv_prediction_breakdown_explainer
#'
#' @importFrom breakDown broken
#' @importFrom stats median
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
#' broken_prediction <- prediction_breakdown(surve_cph, pbc[1,-c(1,2)])
#' }
#' @export


prediction_breakdown <- function(explainer, observation, time = NULL, prob = NULL, ...){
  if (!("surv_explainer" %in% class(explainer))) stop("The prediction_breakdown() function requires an object created with explain() function form survxai package.")
  if (is.null(explainer$data)) stop("The prediction_breakdown() function requires explainers created with specified 'data' parameter.")
  if (!is.null(time) & !is.null(prob)) stop("Only one of the parameters 'time', 'prob' should be provided.")

  # breakDown
  if (is.null(prob)) {
    if (is.null(time)) time <- median(explainer$times)

    new_pred <- function(model, data){
      explainer$predict_function(model, data, times = time)
    }
  } else {
    times_sorted <- sort(explainer$times)

    find_time <- function(x){
      tim <- (x < prob)
      index <- c(min(which(tim == TRUE)) -1, min(which(tim == TRUE)))
      closest_times <- times_sorted[index]
      weighted.mean(closest_times, x[index])
    }

    new_pred <- function(model, data){
      probabilities <- explainer$predict_function(model, data, times = explainer$times)
      probabilities <- as.data.frame(probabilities)

     res <- apply(probabilities, MARGIN = 1, FUN = find_time)
     res <- na.omit(res)
     return(res)

    }

    npred <- new_pred(explainer$model, explainer$data)
    message("Number of observations with prob > ", prob, ": ", nrow(explainer$data) - length(npred))
  }




  oldw <- getOption("warn")
  options(warn = -1)
  res<- broken(model = explainer$model,
                  new_observation = observation,
                  data = explainer$data,
                  predict.function = new_pred,
                  baseline = "Intercept")
  options(warn = oldw)

  class(res) <- "data.frame"


  result <- data.frame(x = numeric(), y = numeric(), variable = character(), label = character(), position = numeric(), value = character())
  res <- res[-c(1, nrow(res)),]


  times <- sort(explainer$times)

  #baseline
  prediction <- explainer$predict_function(explainer$model, explainer$data, times)
  mean_prediction <- data.frame(x = times, y = colMeans(prediction, na.rm=T))
  mean_prediction <- rbind(mean_prediction, c(0, 1))
  mean_prediction$variable<- "Intercept"
  mean_prediction$label <- explainer$label
  mean_prediction$position <- 1
  mean_prediction$value <- "Intercept"

  result <- rbind(result, mean_prediction)


  #one observation
  prediction <- explainer$predict_function(explainer$model, observation, times)
  mean_prediction <- data.frame(x = times, y = prediction[1,])
  mean_prediction <- rbind(mean_prediction, c(0, 1))
  mean_prediction$variable<- "Observation"
  mean_prediction$label <- explainer$label
  mean_prediction$position <- nrow(res)+2
  mean_prediction$value <- "Observation"
  result <- rbind(result, mean_prediction)

  tmp_data <- explainer$data

  for (i in 1:nrow(res)){
    variable <- res[i, "variable_name"]
    tmp_data[,as.character(variable)] <-  observation[[as.character(variable)]]
    prediction <- explainer$predict_function(explainer$model, tmp_data, times)
    mean_prediction <- data.frame(x = times, y = colMeans(prediction, na.rm=T))
    mean_prediction <- rbind(mean_prediction, c(0, 1))
    mean_prediction$variable<- variable
    mean_prediction$label <- explainer$label
    mean_prediction$position <- res[i, "position"]
    mean_prediction$value <- res[i, "variable"]
    result <- rbind(result, mean_prediction)
  }

  res <- res[,c("contribution", "variable_name")]
  colnames(res)[2] <- "variable"
  attr(result, "contribution") <- res
  attr(result, "time") <- time
  attr(result, "prob") <- prob
  class(result) <- c("surv_prediction_breakdown_explainer", "data.frame")
  result

}
