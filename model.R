
### NULL MODEL
null_model_fit <- function(data){
  return (NULL)
}

null_model_predict <- function(data,params){
  return (.6)
}

hmm_model_fit <- function(data){
  library(bsts)
  data$binary_mosquito <- as.factor(ifelse(data$num_aegypti_collected >0,1,0))
  s <- AddLocalLevel(list(),
                     sigma.prior = SdPrior(sigma.guess = .1,
                                           sample.size = 1,
                                           upper.limit = 1),
                     initial.state.prior = NormalPrior(0, 5))
  ts.model <- bsts(ts(data$binary_mosquito)-1 , s, data = data, niter = 10000,family = "logit")
  pred_obj <- predict(ts.model,horizon=1)
  return (pred_obj$mean)
  
}