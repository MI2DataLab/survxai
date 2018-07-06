#' @title Plot for surv_variable_serponse object
#'
#' @description Function plot for surv_variable_response object shows the expected output condition on a selected variable.
#'
#' @param x object of class "surv_variable_response"
#' @param ... other arguments
#' @param split a character, either "model" or "level". Sets the variable for faceting.
#'
#' @import ggplot2
#' @importFrom stats aggregate quantile
#'
#' @method plot surv_variable_response_explainer
#' @export

plot.surv_variable_response_explainer <- function(x, ..., split = "model"){
  y <- color <- NULL

  df <- data.frame(x)
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      class(resp) <- "data.frame"
      df <- rbind(df, resp)
    }
  }

  if (is.numeric(df$value) & length(unique(df$value))>=4) {
    df$value <- cut(df$value, quantile(df$value, prob = seq(0, 1, length.out = 6)), include.lowest = TRUE)
  }

  if (split == "level") {
    add_facet <- facet_wrap(~value, ncol = 1)
    df$color <- factor(df$label)
    legend <- "label"
  } else {
    add_facet <- facet_wrap(~label, ncol = 1)
    df$color <- factor(df$value)
    legend <- x$var[1]
  }


  ggplot(df, aes(x, y, color = color)) +
    geom_step() +
    labs(title = paste0("Partial Dependency Plot of variable ", df$var[1]),
         x = "time",
         y = "mean survival probability",
         col = legend) +
    add_facet +
    theme_bw()


}
