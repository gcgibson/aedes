
### NULL MODEL
null_model_fit <- function(data,target){
  return (NULL)
}

null_model_predict <- function(data,target,params){
  if (target == "Ae.-aegypti"){
    return (.6)
  } else {
    return (.5)
  }
}

hmm_model_fit <- function(data,target){
  library(bsts)
  if (target == "Ae.-aegypti"){
    data_to_train <- ifelse(data$num_aegypti_collected >0 ,1,0)
  } else{
    data_to_train <- ifelse(data$num_albopictus_collected >0 ,1,0)
  }
  
  s <- AddLocalLevel(list(),
                     sigma.prior = SdPrior(sigma.guess = .1,
                                           sample.size = 1,
                                           upper.limit = 1),
                     initial.state.prior = NormalPrior(0, 5))
  ts.model <- bsts(ts(data_to_train)-1 , s, data = data, niter = 1000,family = "logit")
  return (ts.model)
}


hmm_model_predict <- function(data,target,params){
  pred_obj <- predict(params,horizon=1)
  return (pred_obj$mean)
}