library(survxai)
library(rms)
library(randomForestSRC)

data(pbc, package = "randomForestSRC")
pbc <- pbc[complete.cases(pbc),]

predict_times <- function(model, data, times){
  prob <- rms::survest(model, data, times = times)$surv
  return(prob)
}

cph_model <- cph(Surv(days/365, status)~., data=pbc, surv=TRUE, x = TRUE, y=TRUE)
cph_model2 <- cph(Surv(days/365, status)~sex+bili, data=pbc, surv=TRUE, x = TRUE, y=TRUE)

surve_cph <- explain(model = cph_model,
                  data = pbc, y = Surv(pbc$days/365, pbc$status),
                  predict_function = predict_times)
surve_cph2 <- explain(model = cph_model2,
                     data = pbc, y = Surv(pbc$days/365, pbc$status),
                     predict_function = predict_times, label = "2")

surve_cph_null_data <- explain(model = cph_model, y = Surv(pbc$days/365, pbc$status),
                                    predict_function = predict_times)

broken_prediction <- prediction_breakdown(surve_cph, pbc[1,-c(1,2)])
svr_cph <- variable_response(surve_cph, "sex")
svr_cph2 <- variable_response(surve_cph2, "sex")
svr_cph_group <- variable_response(surve_cph, "bili")

# plot_curves <- plot(broken_list)
# plot_curves_and_table <- plot(broken_list, table=T)
plot_var_resp <- plot(svr_cph)
plot_var_resp_levels <- plot(svr_cph, svr_cph2, split = "level")
plot_var_resp_default <- plot(svr_cph, svr_cph2)
