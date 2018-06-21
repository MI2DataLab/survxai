library(survxai)
library(rms)
library(randomForestSRC)

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

predict_times <- function(model, new_observation, times=1){
  prob <- rms::survest(model, new_observation, times=times)$surv
  return(prob)
}

cph_model <- cph(Surv(days/365, status)~., data=pbc[1:300,], surv=TRUE, x = TRUE, y=TRUE)

surve_cph <- surv_explain(model = cph_model,
                  data = pbc, time = pbc$days/365, status = pbc$status,
                  predict_function = predict_times)

broken_list <- surv_breakdown(surve_cph, pbc[1,], times = c(1,2))
