data_ca <- read.csv("./data/aedes_collections_california.csv")
data_ct <- read.csv("./data/aedes_collections_connecticut.csv")
data_fl <- read.csv("./data/aedes_collections_florida.csv")
data_nj <- read.csv("./data/aedes_collections_new_jersey.csv")
data_ny <- read.csv("./data/aedes_collections_new_york.csv")
data_nc <- read.csv("./data/aedes_collections_north_carolina.csv")
data_tx <- read.csv("./data/aedes_collections_texas.csv")
data_wi <- read.csv("./data/aedes_collections_wisconsin.csv")




total_data <- rbind(data_ca,data_ct,data_fl,data_nj,data_ny,data_nc,data_tx,data_wi)


ym <- c()
for (row in 1:nrow(total_data)){
  if (total_data[row,]$month < 10){
    ym <- c(ym,paste0(total_data[row,]$year,paste0("0",total_data[row,]$month)))
    }else{
    ym <- c(ym,paste0(total_data[row,]$year,total_data[row,]$month))
  }
}
total_data$ym <- ym
### Exploration plots
library(ggplot2)
ggplot(total_data,aes(x=ym,num_aegypti_collected,col=state)) + geom_line()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
