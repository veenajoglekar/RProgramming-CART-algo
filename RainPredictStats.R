
#Statistics -Accuracy of prediction model on training and test data

weather1<-read.csv("C:/Users/asus/Downloads/seattleWeather_1948-2017.csv") 

weather<-weather1[-c(which(is.na(weather1$RAIN))),] 
#removing NA values from Rain column

index<-createDataPartition(weather$RAIN,p=0.8, list = FALSE) 
#Seperating 80% data and storing their indexes in collection index

weather_train<-weather[index,]
#All the values with indexes from index collection

weather_test<-weather[-index,]
#All the remaining data aside from indexes

weather_model<-rpart(RAIN~TMAX+TMIN, method="class", control=rpart.control(minsplit = 5, cp=0.000001),data=weather_train )

#TRAINING DATA

weather_model<-rpart(RAIN~TMAX+TMIN, method="class", control=rpart.control(minsplit = 5, cp=0.000001),data=weather_train )

weather_pred<-predict(weather_model,type = "class")

conf_matrix<-table(weather_pred,weather_train$RAIN)

print(conf_matrix)


#TEST DATA

weather_model<-rpart(RAIN~TMAX+TMIN, method="class", control=rpart.control(minsplit = 5, cp=0.000001),data=weather_test )

weather_pred<-predict(weather_model,type = "class")

conf_matrix<-table(weather_pred,weather_test$RAIN)

print(conf_matrix)