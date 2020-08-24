#' @title Model performance for survival models
#'
#' @description Function \code{model_performance} calculates the prediction error for chosen survival model.
#'
#' @param explainer a model to be explained, preprocessed by the 'survxai::explain' function
#' @param type character - type of the response to be calculated
#' Currently following options are implemented: 'BS' for Expected Brier Score
#'
#' @details
#' For \code{type = "BS"} prediction error is the time dependent estimates of the population average Brier score.
#' At a given time point t, the Brier score for a single observation is the squared difference between observed survival status
#' and a model based prediction of surviving time t.
#'
#' @examples
#' \donttest{
#'    library(survxai)
#'    library(rms)
#'    data("pbcTrain")
#'    data("pbcTest")
#'    cph_model <- cph(Surv(years, status)~ sex + bili + stage,
#'      data=pbcTrain, surv=TRUE, x = TRUE, y=TRUE)
#'    surve_cph <- explain(model = cph_model, data = pbcTest[,-c(1,5)],
#'                         y = Surv(pbcTest$years, pbcTest$status))
#'    mp_cph <- model_performance(surve_cph)
#' }
#'
#' @references Ulla B. Mogensen, Hemant Ishwaran, Thomas A. Gerds (2012). Evaluating Random Forests for Survival Analysis Using Prediction Error Curves. Journal of Statistical Software, 50(11), 1-23. URL http://www.jstatsoft.org/v50/i11/.
#'
#' @import pec
#' @importFrom prodlim Hist
#' @importFrom stats as.formula
#'
#' @export

model_performance <- function(explainer, type = "BS"){
  if (!("surv_explainer" %in% class(explainer))) stop("The model_performance() function requires an object created with explain() function from survxai package.")
  reference_formula <- eval(explainer$model$call[[2]])
  # trick for mlr, to remove third param in Surv
  if(length(reference_formula[[2]]) > 3){
    reference_formula[[2]][4] <- NULL
  }
  reference_formula[3] <- 1
  surv_vars <- all.vars(explainer$model$call[[2]][[2]])
  data <- cbind(explainer$y[,1], explainer$y[,2], explainer$data)
  colnames(data)[1:2] <- surv_vars

  switch(type,
         BS = {
           p <- tryCatch({
             p <- pec(explainer$model, data = data, splitMethod = "none", formula = reference_formula)
           },  error = function(e) {
             p <- pec(explainer, data = data, splitMethod = "none", formula = reference_formula, reference = TRUE)
             return(p)
           })
           res <- data.frame(time = p$time, err = p$AppErr[[2]], err_ref = p$AppErr[[1]], label = explainer$label)
           class(res) <- c("surv_model_performance_explainer", "data.frame", "BS")
           attr(res, "type") <- type
           attr(res, "time") <- explainer$y[,1]
           return(res)
         },
         stop("Currently only 'BS' method is implemented"))
}




