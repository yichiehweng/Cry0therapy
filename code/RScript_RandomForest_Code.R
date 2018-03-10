#Preliminaries
install.packages("randomForest")
require(randomForest)
#Load Data
df=read.csv("/home/yichiehweng/python/machineLearning/RandomForest/Cryotherapy/data/Cryotherapy.csv")
sapply(df, function(x) sum(is.na(x)))
df$Result_of_Treatment<-as.factor(df$Result_of_Treatment)
head(df,5)
#Create train and test data
ind <- sample(2,nrow(df),replace=TRUE,prob=c(0.8,0.2))
set.seed(555)  
train <- df[ind==1,]
test <- df[ind==2,]
#Train The Random Forest Model
model=randomForest(Result_of_Treatment~.,data=train,importance=TRUE,proximity=TRUE,ntree=500, na.action = na.fail)
print(model)
#Pridiction
Prediction<-predict(model,newdata=test)
table(Prediction, test$Result_of_Treatment)
confus.matrix = table(test$Result_of_Treatment, predict=Prediction)
sum(diag(confus.matrix))/sum(confus.matrix)