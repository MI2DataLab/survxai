#' @title Plot for surv_breakdown object
#'
#' @description Function plot for surv_breakdown object visualise estimated survival curve of mean probabilities in chosen time points.
#'
#' @param x object of class "surv_prediction_breakdown_explainer"
#' @param ... other arguments
#'
#' @import ggplot2
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
#' plot(broken_prediction)
#' }
#' @method plot surv_prediction_breakdown_explainer
#'
#' @importFrom scales seq_gradient_pal
#' @export

plot.surv_prediction_breakdown_explainer <- function(x, ...){
  y <- col <- label <- value <- position <- legend <-  NULL

  df <- data.frame(x)
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      class(resp) <- "data.frame"
      df <- rbind(df, resp)
    }
  }

  if(length(dfl)==0){
    add_facet <- NULL
  }else{
    add_facet <- facet_wrap(~label, ncol = 1)
  }

  if(!is.null(attributes(x)$prob)){
    line <- geom_hline(yintercept = attributes(x)$prob)
  }else{
    line <- geom_vline(xintercept = attributes(x)$time)
  }

  df$legend <- paste0(df$position,": ", df$value)
  broken_cumm <- attributes(x)$contribution
  broken_cumm$contribution <- round(broken_cumm$contribution, digits = 2)
  broken_cumm$contribution <- paste0("(", broken_cumm$contribution, ")")
  broken_cumm$variable <- as.character(broken_cumm$variable)
  broken_cumm <- rbind(broken_cumm, c("", "Intercept"))
  broken_cumm <- rbind(broken_cumm, c("", "Observation"))

  df <- merge(df, broken_cumm, by = "variable")
  df$legend <- paste(df$legend, df$contribution)
  df$legend <- factor(df$legend, levels = unique(df$legend[order(df$position)]))

  #colors
  cc <- seq_gradient_pal("#010059","#e0f6fb")(seq(0,1,length.out=length(unique(df$legend))))

  #labels
  median_time <- median(unique(df$x))
  median <- which.min(abs(unique(df$x) - median_time))
  median <- unique(df$x)[median]


  ggplot(df, aes(x=x, y=y, col = factor(legend)))+
    geom_step()+
    geom_text(data = df[df$x == median,], aes(label = position), color = "black", show.legend = FALSE, hjust = 0, vjust = 0)+
    labs(title = "BreakDown plot",
        x = "time",
        y = "mean survival probability",
        col =  "variable") +
    add_facet +
    theme_mi2()+
    scale_colour_manual(values=cc)+
    line+
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = paste(seq(0,100,10),"%"),
                       name = "survival probability")


}


