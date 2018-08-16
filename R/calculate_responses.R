calculate_responses<- function(vname, times_s, observation_s, model_s, explainer_s, grid_points_s, data_s, predict_function_s) {
  if(class(data_s[,vname])=="numeric" || class(data_s[,vname])=="integer"){
    probs <- seq(0, 1, length.out = grid_points_s)
    new_x <- quantile(data_s[,vname], probs = probs)
    quant_x <- mean(observation_s[1,vname] >= data_s[,vname], na.rm = TRUE)
    new_data <- observation_s[rep(1, grid_points_s),]
    new_data[,vname] <- new_x
    y_hat <- t(predict_function_s(model_s, new_data, times_s))
    
    res <- data.frame(y_hat=numeric(), time = numeric(), vname = character(), new_x = numeric(), 
                      x_quant = numeric(), quant = numeric(), relative_quant = numeric(), label = character(), 
                      class = character())
    
    for(i in 1:grid_points_s){
      tmp <- data.frame(y_hat = y_hat[,i])
      tmp$new_x <- as.character(new_x[i])
      tmp$vname <- vname
      tmp$x_quant <- quant_x
      tmp$quant <- probs[i]
      tmp$relative_quant <- probs[i] - quant_x
      tmp$label <- explainer_s$label
      tmp$time <- times_s
      tmp$class <- "numeric"
      res <- rbind(res, tmp)
    }
  }
  if(class(data_s[,vname])=="character" || class(data_s[,vname])=="factor"){
    data_s[,vname] <- as.factor(data_s[,vname])
    new_data <- observation_s[rep(1, length(levels(data_s[,vname]))),]
    new_data[,vname] <- as.factor(new_data[,vname])
    new_x <- levels(data_s[,vname])
    new_data[,vname] <- new_x
    f <- sapply(data_s, is.factor)
    cols <- names(which(f))
    new_data[cols] <- lapply(new_data[cols], as.factor)
    y_hat <- t(predict_function_s(model_s, new_data, times_s))
    
    res <- data.frame(y_hat=numeric(), time = numeric(), vname = character(), new_x = character(), 
                      x_quant = numeric(), quant = numeric(), relative_quant = numeric(), label = character(), 
                      class = character())
    
    for(i in 1:length(levels(data_s[,vname]))){
      tmp <- data.frame(y_hat = y_hat[,i])
      tmp$new_x <- new_x[i]
      tmp$vname <- vname
      tmp$x_quant <- 0
      tmp$quant <- 0
      tmp$relative_quant <- 0
      tmp$label <- explainer_s$label
      tmp$time <- times_s
      tmp$class <- "factor"
      res <- rbind(res, tmp)
    }
  }
  return(res)
}
