source('read_data.R')
source('model.R')
library(readr)
library(dplyr)
models_to_run <- c("null","hmm","fomm")

#Subset training data 
cutoff_date <- 201810
training_data1 <- total_data[total_data$ym <= cutoff_date,]
countylist<-unique(total_data$location)

for (i in 1:length(countylist)){ 

  training_data2 <- training_data1 %>% filter(location==countylist[i])
  
for (model in models_to_run){
  for (target in c("Ae.-aegypti", "Ae.-albopictus")){
    if(model == "null"){
      model_fit <-null_model_fit(training_data2,target)
    } else if(model == "hmm"){
      model_fit <-hmm_model_fit(training_data2,target)
    } else if(model == "fomm"){
      model_fit <-fomm_fit(training_data2,target)
    }
    write_rds(model_fit,paste0("./model_fits/",model,"-",target,"-",cutoff_date,"-",countylist[i]))
  }
}
}
