
### NULL MODEL
null_model_fit <- function(data,target){
  return (NULL)
}

null_model_predict <- function(data,target,params){
  if (target == "Ae.-aegypti"){
    return (.5)
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

### first-order MM
fomm_fit <- function(data,target){
  library(markovchain)
  if (target == "Ae.-aegypti"){
    data_to_train <- ifelse(data$num_aegypti_collected >0 ,1,0)
  } else{
    data_to_train <- ifelse(data$num_albopictus_collected >0 ,1,0)
  }
  fomm<- markovchainFit(data = data_to_train, method = "map")
  return (fomm)
}

fomm_predict <- function(data,target,location,params){
  library(data.table)
  dat1 <-data %>% dplyr::select(-"trap_type") 
  aggdata<-aggregate(. ~ state+statefp+county+countyfp+year+month+ym, dat1, sum, na.rm = TRUE)
  ord<- aggdata %>% group_by(state,county,ym) %>% arrange(state,county,year, month)
  prevMO <- ifelse(first_score_month==1,12,first_score_month-1)
  prevYR <- ifelse(first_score_month==1,first_test_season-1,first_test_season)
  DT <- as.data.table(ord)
  if(prevMO < 5){
    DT1 <-DT[year == prevYR|year == prevYR-1|year == prevYR-2][, .SD[.N], by = state:county]}else{
      DT1 <-DT[year == prevYR|year == prevYR-1|year == prevYR-2][, .SD[prevMO-3], by = state:county]}
  DT2 <- DT1[which(DT1$state == gsub("-.*$", "", location) & DT1$county == gsub("^.*-(.*?)","", location)),]
  if(target=="Ae.-aegypti"){  
    state<-ifelse(DT2$num_aegypti_collected >0, 1, 0)
    trans.p<-matrix(c(params$estimate[1],params$estimate[2]),nrow=2)
    nprob<-ifelse(state==1,trans.p[2,2],trans.p[1,2])}else{  
      state<-ifelse(DT2$num_albopictus_collected >0, 1,0)
      trans.p<-matrix(c(params$estimate[1],params$estimate[2]),nrow=2)
      nprob<-ifelse(state==1,trans.p[2,2],trans.p[1,2])}
  pred_obj <- nprob
  return (pred_obj)
}

