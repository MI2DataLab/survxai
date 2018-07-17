#' @title Model performance for survival models
#'
#' @description Function \code{model_performance} calculates the residuals and model performance measures.
#'
#' @param explainer an object of the class 'surv_explainer'.
#' @param type character - type of the response to be calculated.
#' Currently following options are implemented: 'BS' for Expected Brier Score.
#' @param ... other parameters
#'
#' @importFrom pec pec
#'
#' @export

variable_response <- function(explainer, type = "BS", ...){
  if (!("surv_explainer" %in% class(explainer))) stop("The model_performance() function requires an object created with explain() function from survxai package.")
  if (is.null(explainer$data)) stop("The model_performance() function requires explainers created with specified 'data' parameter.")

  switch(type,
         BS = {
           reference_formula <- as.formula(paste0(attributes(explainer)$formula,"~ 1"))
           p <- pec(explainer$model, data = explainer$data, splitMethod = "none", formula = Surv(pbc$days/365, pbc$status) ~ 1)

           class(res) <- c("surv_model_performance_explainer", "data.frame", "BS")
           return(res)
         },
         stop("Currently only 'BS' method is implemented"))
}




