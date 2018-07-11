library(survxai)
library(rms)
library(randomForestSRC)

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

predict_times <- function(model, data, times){
  prob <- rms::survest(model, data, times = times)$surv
  return(prob)
}

predict_times_rf<- function(object, newdata, times, ...){
  ptemp <- predict(object,newdata=newdata,importance="none")$survival
  pos <- prodlim::sindex(jump.times=object$time.interest,eval.times=times)
  p <- cbind(1,ptemp)[,pos+1,drop=FALSE]
  if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",NROW(newdata)," x ",length(times),"\nProvided prediction matrix: ",NROW(p)," x ",NCOL(p),"\n\n",sep=""))
  p
}

rf_model <- rfsrc(Surv(year, status) ~ ., data = pbc[,-1], ntree = 100)
cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
cph_model2 <- cph(Surv(days/365, status)~sex+bili, data=pbc, surv=TRUE, x = TRUE, y=TRUE)

surve_cph <- explain(model = cph_model,
                  data = pbc[,-c(1,2)], y = Surv(pbc$days/365, pbc$status),
                  predict_function = predict_times)
surve_cph2 <- explain(model = cph_model2,
                     data = pbc, y = Surv(pbc$days/365, pbc$status),
                     predict_function = predict_times, label = "2")
surve_rf <- explain(model = rf_model,
                    data = pbc, y = Surv(pbc$days/365, pbc$status),
                    predict_function = predict_times_rf)

surve_cph_null_data <- explain(model = cph_model, y = Surv(pbc$days/365, pbc$status),
                                    predict_function = predict_times)
surve_cph_null_data$data <- NULL

explainer <- surve_cph
class(explainer)  <- "explainer"



broken_prediction <- prediction_breakdown(surve_cph, pbc[1,-c(1,2)])
broken_prediction2 <- prediction_breakdown(surve_cph2, pbc[1,-c(1,2)])
svr_cph <- variable_response(surve_cph, "sex")
svr_cph2 <- variable_response(surve_cph2, "sex")
svr_cph_group <- variable_response(surve_cph, "bili")


plot_var_resp <- plot(svr_cph)
plot_var_resp_levels <- plot(svr_cph, svr_cph2, split = "level")
plot_var_resp_default <- plot(svr_cph, svr_cph2)
