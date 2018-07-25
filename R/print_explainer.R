#' Print Survival Explainer Summary
#'
#' @param x a model survival expaliner created with the `explain()` function
#' @param ... other parameters
#'
#' @export

print.surv_explainer <- function(x, ...) {
  cat("Model label: ", x$label, "\n")
  cat("Model class: ", paste(x$class, collapse = ","), "\n")
  cat("Data head  :\n")
  print(head(x$data,2))
  return(invisible(NULL))
}