#' Ceteris Paribus Plot
#'
#' @param explainer a model to be explained, preprocessed by the 'survxai::explain' function
#' @param observation a new observarvation for which predictions need to be explained
#' @param grid.points grid_points number of points used for response path
#' @param selected_variables if specified, then only these variables will be explained
#'
#' @return An object of the class surv_ceteris_paribus_explainer.
#' It's a data frame with calculated average responses.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom utils head
#'

ceteris_paribus <- function(explainer, observation, grid.points = 5, selected_variables = NULL) {
  if (!("surv_explainer" %in% class(explainer)))
    stop("The ceteris_paribus() function requires an object created with explain() function from survxai package.")
  if (is.null(explainer$data))
    stop("The ceteris_paribus() function requires explainers created with specified 'data' parameter.")
  
  data <- base::as.data.frame(explainer$data)
  model <- explainer$model
  predict_function <- explainer$predict_function
  var_to_present <- which(sapply(data, is.numeric))
  names_to_present <- colnames(data)[var_to_present]
  
  if (!is.null(selected_variables)) {
    names_to_present <- intersect(names_to_present, selected_variables)
  }
  times <- explainer$y[,1]
  
  
  responses <- lapply(names_to_present, function(vname) {
    probs <- seq(0, 1, length.out = grid.points)
    new_x <- quantile(data[,vname], probs = probs)
    quant_x <- mean(observation[1,vname] >= data[,vname], na.rm = TRUE)
    new_data <- observation[rep(1, grid.points),]
    new_data[,vname] <- new_x
    y_hat <- t(predict_function(model, new_data, times))
    
    
    res <- data.frame(y_hat=numeric(), time = numeric(), vname = character(), new_x = numeric(), 
                      x_quant = numeric(), quant = numeric(), relative_quant = numeric(), label = character())
    for(i in 1:grid.points){
      tmp <- data.frame(y_hat = y_hat[,i])
      tmp$new_x <- new_x[i]
      tmp$vname <- vname
      tmp$x_quant <- quant_x
      tmp$quant <- probs[i]
      tmp$relative_quant <- probs[i] - quant_x
      tmp$label <- explainer$label
      tmp$time <- times
      
      res <- rbind(res, tmp)
    }
    res
  })
  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation, times)

  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat)
  class(all_responses) = c("surv_ceteris_paribus_explainer", "data.frame")
  all_responses
}