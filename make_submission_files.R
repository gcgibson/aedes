source('read_data.R')
source('model.R')
library(dplyr)
library(plyr)

models_to_run <- c("null")

first_test_season <- 2016
first_score_month <- 01

predict_model <- function()

for (model in models_to_run){
  template <- read.csv("/Users/gcgibson/Downloads/submission_template.csv")
  model_fit_and_date_string <- paste0(model,"-",first_test_season-1,12)
  fitted_model_params <- readRDS(paste0("./model_fits/",model_fit_and_date_string))
  
  predictions <- ddply(template,.(location,target,type,unit),function(row){
    if (model == "null"){
       prediction_from_fitted_model <- null_model_predict(total_data,fitted_model_params)
    }
    row$value <- prediction_from_fitted_model
    return (row)
  })
  
  write.csv(predictions,paste0("./submissions/ew","-",tail(total_data$ym,1),"-",model))
}

    
#write.csv(prob,paste0("./submissions/",first_test_season,"-",first_test_month,"-",model ,".csv"))
 