library(survxai)
library(rms)
library(randomForestSRC)

data(pbc, package = "randomForestSRC")


predict_times <- function(model, new_observation, times){
  prob <- rms::survest(model, new_observation, times=times)$surv
  return(prob)
}

cph_model <- cph(Surv(days/365, status)~., data=pbc[1:300,], surv=TRUE)

surve_cph <- surv_explain(model = cph_model,
                  data = pbc, time = pbc$days/365, status = pbc$status,
                  predict_function = predict_times)
