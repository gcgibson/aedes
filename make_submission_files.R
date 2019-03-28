source('read_data.R')
source('model.R')
library(dplyr)
library(plyr)

models_to_run <- c("null","hmm")

first_test_season <- 2016
first_score_month <- 01


for (model in models_to_run){
  template <- read.csv("./docs/submission_template.csv")
  
  predictions <- ddply(template,.(location,target,type,unit),function(row){
    model_fit_and_date_string <- paste0(model,"-",gsub(" ","-",row$target),"-",first_test_season-1,12)
    fitted_model_params <- readRDS(paste0("./model_fits/",model_fit_and_date_string))
    
    if (model == "null"){
       prediction_from_fitted_model <- null_model_predict(total_data,gsub(" ","-",row$target),fitted_model_params)
    } else if (model == "hmm"){
      prediction_from_fitted_model <- hmm_model_predict(total_data,gsub(" ","-",row$target),fitted_model_params)
    }
    row$value <- prediction_from_fitted_model
    return (row)
  })
  
  write.csv(predictions,paste0("./submissions/ew","-",tail(total_data$ym,1),"-",model),row.names = F)
}

    
#write.csv(prob,paste0("./submissions/",first_test_season,"-",first_test_month,"-",model ,".csv"))
 