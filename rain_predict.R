rain_predict<-function(rmax,rmin)
{
  library(caret)
  library(rpart)
  library(tidyverse)
  
  weather1<-read.csv("seattleWeather.csv") 
  
  weather<-weather1[-c(which(is.na(weather1$RAIN))),] 
  #removing NA values from Rain column
  
  index<-createDataPartition(weather$RAIN,p=0.8, list = FALSE) 
  #Seperating 80% data and storing their indexes in collection index
  
  weather_train<-weather[index,]
  #All the values with indexes from index collection
  
  weather_test<-weather[-index,]
  #All the remaining data aside from indexes
  
  weather_model<-rpart(RAIN~TMAX+TMIN, method="class", control=rpart.control(minsplit = 5, cp=0.000001),data=weather_train )
  #Comparing Rain data with tmax and tmin () 
  #minsplit:: the minimum number of observations that must exist in a node in order for a split to be attempted.
  
  
  data<-data.frame(TMAX=rmax,TMIN=rmin)
  #adding parameters to dataframe so we can predict them later
  
  weather_model %>% predict(data,"class")
  #Actual prediction
  
}

rain_predict(45,29)