# Task 2:Supervised learning-Predicting the percentage of
# marks that a student is expected to score based upon the
# number of hours they studied. 

studentrecord=read.csv("student_scores.csv") 
str(studentrecord)
head(studentrecord) #display the first six rows of our dataset 

#visualization of the data set using ggplot
library(ggplot2)
ggplot()+
  geom_point(data=studentscores,aes(x=Hours,y=Scores),col="red")+ 
  scale_x_continuous(limits = c(1,10),breaks = seq(1,10,1))+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10))

#creating training and test data
trainingset=sample(1:nrow(studentrecord),0.7*nrow(studentrecord))
trainingset
trainingdata <- studentrecord[trainingset, ]
trainingdata

testData<-studentrecord[-trainingset, ]
testData

#building a linear model for the dataset
model<- lm(Scores~Hours, data=trainingdata)
model
#Checking summary of the model
summary(model)

#Plotting regression line on graph
ggplot(data = studentscores, aes(x = Hours, y = Scores)) +
  scale_x_continuous(limits = c(1,10),breaks = seq(1,10,1))+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10))+
  geom_point(color="red") +
  geom_smooth(method = "lm")

#predicting the value of Scores based on training dataset
Predicted_trainingScore = predict (model, trainingdata )
Predicted_trainingScore
library(Metrics)
rmse(trainingdata$Scores, Predicted_trainingScore)#calculating root mean square error


#predicting the value of Scores based on test dataset
Predicted_testScore = predict (model, testData )
Predicted_testScore
rmse(testData$Scores, Predicted_testScore)

#predicting the value of Scores based on a new data 
NewData = data.frame(Hours=c(2, 4, 6.75, 9.25, 3.5))
NewData
Predicted_score = predict (model, NewData)
Predicted_score
