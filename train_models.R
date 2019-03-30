source('read_data.R')
source('model.R')
library(readr)
models_to_run <- c("null","hmm","fomm")

#Subset training data 
cutoff_date <- 201810
training_data <- total_data[total_data$ym <= cutoff_date,]


for (model in models_to_run){
  for (target in c("Ae.-aegypti", "Ae.-albopictus")){
    if(model == "null"){
      model_fit <-null_model_fit(training_data,target)
    } else if(model == "hmm"){
      model_fit <-hmm_model_fit(training_data,target)
    } else if(model == "fomm"){
      model_fit <-fomm_fit(training_data,target)
    }
    write_rds(model_fit,paste0("./model_fits/",model,"-",target,"-",cutoff_date))
  }
}
