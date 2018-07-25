library(survxai)
library(rms)
library(randomForestSRC)
library(prodlim) #dlaczego nie widzi w importach?
library(pec)


data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]
pbc$sex <- as.factor(pbc$sex)

pbc <- pbc[1:100,]

pbc2 <- pbc
pbc2 <- tibble::as_tibble(pbc2)

predict_times <- function(object, newdata, times){
  prob <- rms::survest(object, newdata, times = times)$surv
  return(prob)
}

predict_times_rf<- function(object, newdata, times, ...){
  f <- sapply(new_data, is.integer)
  cols <- names(which(f))
  object$xvar[cols] <- lapply(object$xvar[cols], as.integer)
  ptemp <- predict(object,newdata=newdata,importance="none")$survival
  pos <- prodlim::sindex(jump.times=object$time.interest,eval.times=times)
  p <- cbind(1,ptemp)[,pos+1,drop=FALSE]
  if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
    stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",NROW(newdata)," x ",length(times),"\nProvided prediction matrix: ",NROW(p)," x ",NCOL(p),"\n\n",sep=""))
  p
}

rf_model <- rfsrc(Surv(days/365, status)~., data  = pbc, ntree = 100)
cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
cph_model2 <- cph(Surv(days/365, status)~sex+bili, data=pbc, surv=TRUE, x = TRUE, y=TRUE)

cph_model_different_class <- cph_model
class(cph_model_different_class) <- "custom_model"


surve_cph <- explain(model = cph_model,
                  data = pbc[,-c(1,2)], y = Surv(pbc$days/365, pbc$status),
                  predict_function = predict_times)
surve_cph2 <- explain(model = cph_model2,
                     data = pbc, y = Surv(pbc$days/365, pbc$status),
                     predict_function = predict_times, label = "2")


predict_cph <- function(object, newdata, times){
  class(object) <- c("cph", "rms","coxph")
  p <- predictSurvProb(object, newdata, times)
  p
}

surve_cph_artificial <- explain(model = cph_model_different_class,
                                data = pbc, y = Surv(pbc$days/365, pbc$status),
                                predict_function = predict_cph)

surve_cph_tbl <- explain(model = cph_model2,
                         data = pbc2, y = Surv(pbc2$days/365, pbc2$status),
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
cp_cph <- ceteris_paribus(surve_cph, pbc[1,-c(1,2)])
mp_cph <- model_performance(surve_cph, data = pbc, reference_formula = Surv(days/365, status)~1)
mp_cph_artificial <- model_performance(surve_cph_artificial, data = pbc,
                                       reference_formula = Surv(days/365, status)~1)


plot_var_resp <- plot(svr_cph)
plot_var_resp_levels <- plot(svr_cph, svr_cph2, split = "level")
plot_var_resp_default <- plot(svr_cph, svr_cph2)
plot_cp <- plot(cp_cph)
plot_mp <- plot(mp_cph)

