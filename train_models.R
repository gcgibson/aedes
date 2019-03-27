source('read_data.R')
source('model.R')
library(readr)
models_to_run <- c("null")

#Subset training data 
cutoff_date <- 201512
training_data <- total_data[total_data$ym <= cutoff_date,]


for (model in models_to_run){
  if(model == "null"){
    model_fit <-null_model_fit(training_data)
  }
  write_rds(model_fit,paste0("./model_fits/",model,"-",cutoff_date))
}