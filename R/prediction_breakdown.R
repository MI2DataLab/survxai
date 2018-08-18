#' @title BreakDown for survival models
#'
#' @description Function \code{surv_breakdown} is an extension of a broken function from breakDown package. It computes the contribution in prediction for the varaibles in the model.
#' The contribution is defined as the difference between survival probabilities for model with added specific value of variable and with the random levels of this variable.
#'
#' @param explainer an object of the class 'surv_explainer'
#' @param observation a new observation to explain
#' @param time a time point at which variable contributions are computed. If NULL median time is taken.
#' @param prob a survival probability at which variable contributions are computed
#' @param ... other parameters
#'
#' @return An object of class surv_prediction_breakdown_explainer
#'
#' @importFrom breakDown broken
#' @importFrom stats weighted.mean na.omit median
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
#' surve_cph <- explain(model = cph_model, data = pbcTest[,-c(1,5)], 
#'                     y = Surv(pbcTest$years, pbcTest$status), predict_function = predict_times)
#' broken_prediction <- prediction_breakdown(surve_cph, pbcTest[1,-c(1,5)])
#' }
#' @export


prediction_breakdown <- function(explainer, observation, time = NULL, prob = NULL, ...){
  if (!("surv_explainer" %in% class(explainer))) stop("The prediction_breakdown() function requires an object created with explain() function form survxai package.")
  if (is.null(explainer$data)) stop("The prediction_breakdown() function requires explainers created with specified 'data' parameter.")
  if (!is.null(time) & !is.null(prob)) stop("Only one of the parameters 'time', 'prob' should be provided.")

  # breakDown
  new_pred <- predict_fun(prob, time, explainer)

  oldw <- getOption("warn")
  options(warn = -1)
  res<- broken(model = explainer$model,
                  new_observation = observation,
                  data = explainer$data,
                  predict.function = new_pred)
  options(warn = oldw)

  class(res) <- "data.frame"
  
  intercept <- res$contribution[res$variable_name=="Intercept"]
  observ <- res$contribution[res$variable=="final_prognosis"]

  result <- data.frame(x = numeric(), y = numeric(), variable = character(), label = character(), position = numeric(), value = character())
  res <- res[-c(1, nrow(res)),]


  times <- sort(explainer$times)

  #baseline
  mean_prediction <- calculate_prediction_intercept(explainer, times)
  result <- rbind(result, mean_prediction)
  #one observation
  mean_prediction <- calculate_prediction_observation(explainer, observation, times, res)
  result <- rbind(result, mean_prediction)
  tmp_data <- explainer$data

  for (i in 1:nrow(res)){
    #explainer <- explainer
    variable <- res[i, "variable_name"]
    tmp_data[,as.character(variable)] <-  observation[[as.character(variable)]]
    mean_prediction <- calculate_prediction(explainer, tmp_data, times, res, i, variable)
    result <- rbind(result, mean_prediction)
  }

  res <- res[,c("contribution", "variable_name")]
  colnames(res)[2] <- "variable"
  attr(result, "contribution") <- res
  attr(result, "time") <- time
  attr(result, "prob") <- prob
  attr(result, "Intercept") <- intercept
  attr(result, "Observation") <- observ
  class(result) <- c("surv_prediction_breakdown_explainer", "data.frame")
  result

}


predict_fun <- function(prob, time, explainer){
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
  
  return(new_pred)
}



calculate_prediction_intercept <- function(explainer, times){
  prediction <- explainer$predict_function(explainer$model, explainer$data, times)
  mean_prediction <- data.frame(x = times, y = colMeans(prediction, na.rm=T))
  mean_prediction <- rbind(mean_prediction, c(0, 1))
  mean_prediction$variable<- "Intercept"
  mean_prediction$label <- explainer$label
  mean_prediction$position <- 1
  mean_prediction$value <- "Intercept"
  return(mean_prediction)
}

calculate_prediction_observation <- function(explainer, observation, times, res){
  prediction <- explainer$predict_function(explainer$model, observation, times)
  mean_prediction <- data.frame(x = times, y = prediction[1,])
  mean_prediction <- rbind(mean_prediction, c(0, 1))
  mean_prediction$variable<- "Observation"
  mean_prediction$label <- explainer$label
  mean_prediction$position <- nrow(res)+2
  mean_prediction$value <- "Observation"
  return(mean_prediction)
}


calculate_prediction <- function(explainer, tmp_data, times, res, i, variable){
  prediction <- explainer$predict_function(explainer$model, tmp_data, times)
  mean_prediction <- data.frame(x = times, y = colMeans(prediction, na.rm=T))
  mean_prediction <- rbind(mean_prediction, c(0, 1))
  mean_prediction$variable<- variable
  mean_prediction$label <- explainer$label
  mean_prediction$position <- res[i, "position"]
  mean_prediction$value <- res[i, "variable"]
  return(mean_prediction)
}