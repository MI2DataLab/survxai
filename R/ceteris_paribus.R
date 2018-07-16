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
#' cp_cph <- ceteris_paribus(surve_cph, pbc[1,-c(1,2)])
#' }
#' @export

ceteris_paribus <- function(explainer, observation, grid.points = 5, selected_variables = NULL){
  if (!("surv_explainer" %in% class(explainer)))
    stop("The ceteris_paribus() function requires an object created with explain() function from survxai package.")
  if (is.null(explainer$data))
    stop("The ceteris_paribus() function requires explainers created with specified 'data' parameter.")
  
  data <- base::as.data.frame(explainer$data)
  model <- explainer$model
  predict_function <- explainer$predict_function
  #var_to_present <- which(sapply(data, is.numeric))
  names_to_present <- colnames(data)
  
  if (!is.null(selected_variables)) {
    names_to_present <- intersect(names_to_present, selected_variables)
  }
  times <- explainer$y[,1]
  times <- sort(times)
  
  responses <- lapply(names_to_present, function(vname) {
    
    if(class(data[,vname])=="numeric" || class(data[,vname])=="integer"){
      probs <- seq(0, 1, length.out = grid.points)
      new_x <- quantile(data[,vname], probs = probs)
      quant_x <- mean(observation[1,vname] >= data[,vname], na.rm = TRUE)
      new_data <- observation[rep(1, grid.points),]
      new_data[,vname] <- new_x
      
      y_hat <- t(predict_function(model, new_data, times))
      
      res <- data.frame(y_hat=numeric(), time = numeric(), vname = character(), new_x = numeric(), 
                        x_quant = numeric(), quant = numeric(), relative_quant = numeric(), label = character(), 
                        class = character())
      
      for(i in 1:grid.points){
        tmp <- data.frame(y_hat = y_hat[,i])
        tmp$new_x <- as.character(new_x[i])
        tmp$vname <- vname
        tmp$x_quant <- quant_x
        tmp$quant <- probs[i]
        tmp$relative_quant <- probs[i] - quant_x
        tmp$label <- explainer$label
        tmp$time <- times
        tmp$class <- "numeric"
        
        res <- rbind(res, tmp)
      }
    }
    if(class(data[,vname])=="character" || class(data[,vname])=="factor"){
      data[,vname] <- as.factor(data[,vname])
      new_data <- observation[rep(1, length(levels(data[,vname]))),]
      new_data[,vname] <- as.factor(new_data[,vname])
      new_x <- levels(data[,vname])
      new_data[,vname] <- new_x
      
      f <- sapply(data, is.factor)
      cols <- names(which(f))
      
      new_data[cols] <- lapply(new_data[cols], as.factor)
      
      
      y_hat <- t(predict_function(model, new_data, times))
      
      res <- data.frame(y_hat=numeric(), time = numeric(), vname = character(), new_x = character(), 
                        x_quant = numeric(), quant = numeric(), relative_quant = numeric(), label = character(), 
                        class = character())
      for(i in 1:length(levels(data[,vname]))){
        tmp <- data.frame(y_hat = y_hat[,i])
        tmp$new_x <- new_x[i]
        tmp$vname <- vname
        tmp$x_quant <- 0
        tmp$quant <- 0
        tmp$relative_quant <- 0
        tmp$label <- explainer$label
        tmp$time <- times
        tmp$class <- "factor"
        res <- rbind(res, tmp)
      }
      
    }
    return(res)
  })
  
  
  all_responses <- do.call(rbind, responses)
  new_y_hat <- predict_function(model, observation, times)

  attr(all_responses, "prediction") <- list(observation = observation, new_y_hat = new_y_hat, times = times)
  class(all_responses) = c("surv_ceteris_paribus_explainer", "data.frame")
  all_responses
}
