#' @title Model performance for survival models
#'
#' @description Function \code{model_performance} calculates the residuals and model performance measures.
#'
#' @param explainer an object of the class 'surv_explainer'.
#' @param type character - type of the response to be calculated.
#' Currently following options are implemented: 'BS' for Expected Brier Score.
#' @param data data with time and status.
#' @param reference_formula A survival formula as obtained either with prodlim::Hist or survival::Surv. The left hand side is used to find the status response variable in data. For right censored data, the right hand side of the formula is used to specify conditional censoring models. As in \code{\link[pec]{pec}}.
#' @param ... other parameters
#'
#' @examples
#' \dontrun{
#'    library(survxai)
#'    library(rms)
#'    library(randomForestSRC)
#'    data(pbc, package = "randomForestSRC")
#'    pbc <- pbc[complete.cases(pbc),]
#'    cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
#'    surve_cph <- explain(model = cph_model, data = pbc[,-c(1,2)], y = Surv(pbc$days/365, pbc$status))
#'    mp_cph <- model_performance(surve_cph, data = pbc, reference_formula = Surv(days/365, status)~1)
#' }
#'
#' @references Ulla B. Mogensen, Hemant Ishwaran, Thomas A. Gerds (2012). Evaluating Random Forests for Survival Analysis Using Prediction Error Curves. Journal of Statistical Software, 50(11), 1-23. URL http://www.jstatsoft.org/v50/i11/.
#'
#' @import pec
#' @importFrom prodlim Hist
#' @importFrom stats as.formula
#'
#' @export

model_performance <- function(explainer, type = "BS", data = NULL, reference_formula = NULL, ...){
  if (!("surv_explainer" %in% class(explainer))) stop("The model_performance() function requires an object created with explain() function from survxai package.")
  if (is.null(data)) stop("The model_performance() function requires parameter 'data'. This data.frame should contain also time and status")
  if (is.null(reference_formula)) {
    reference_formula <- as.formula(paste0(attributes(explainer)$formula,"~ 1"))
    message("Reference formula is taken from model object. May cause errors. Providing custom formula is recommended.")
  }
  
  switch(type,
         BS = {
           p <- tryCatch({
             p <- pec(explainer$model, data = data, splitMethod = "none", formula = reference_formula)
           },  error = function(e) {
             p <- pec(explainer, data = data, splitMethod = "none", formula = reference_formula)
             return(p)
           })
           res <- data.frame(time = p$time, err = p$AppErr[[2]], err_ref = p$AppErr[[1]], label = explainer$label)
           class(res) <- c("surv_model_performance_explainer", "data.frame", "BS")
           return(res)
         },
         stop("Currently only 'BS' method is implemented"))
}




