library(tidyverse)
dataset=read.csv('heart.data.csv')
actual=read.csv('Heart_validation.csv')

view(actual)


#explore the data
view(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#imputing missing values for biking and smoking and heart disease 
colSums(is.na(dataset))

#for biking
ggplot(data=dataset,
       aes(biking))+
  geom_histogram()
biking_mean=mean(dataset$biking, na.rm=TRUE)  
dataset$biking=ifelse(is.na(dataset$biking),
                   biking_mean,
                   dataset$biking)
colSums(is.na(dataset))

#for smoking
ggplot(data=dataset,
       aes(smoking))+
  geom_histogram()
smoking_mean=mean(dataset$smoking, na.rm=TRUE)  
dataset$smoking=ifelse(is.na(dataset$smoking),
                      smoking_mean,
                      dataset$smoking)
colSums(is.na(dataset))

#for heart disease
ggplot(data=dataset,
       aes(heart.disease))+
  geom_histogram()
heart.disease_mean=mean(dataset$heart.disease, na.rm=TRUE)  
dataset$heart.disease=ifelse(is.na(dataset$heart.disease),
                      heart.disease_mean,
                      dataset$heart.disease)
colSums(is.na(dataset))

#splitting the data in to two sets (#80 percent training 20 percent test set)
library(caTools)
set.seed(100)
split=sample.split(dataset$heart.disease, SplitRatio=0.8) 
training_set=subset(dataset, split==TRUE)
Test_set=subset(dataset, split=FALSE)

#multiple linear regression training 
names(dataset)
mlr=lm(formula=heart.disease~ .,
       data=training_set)
summary(mlr)






#mean square error
summ=summary(mlr)
MSE=(mean(summ$residuals^2))
paste('The mean square error:' , MSE)


#R-Square
summary(mlr)


#testing set prediction

y_pred=predict(mlr, newdata=Test_set)
data=data.frame(Test_set$heart.disease, y_pred)
head(data)


#validation

head(actual)
actual_x=actual [c(1:2)]
data.frame(actual[c(3)], predict(mlr, newdata=actual_x))


#The predictions were solid. They were pretty close to the actual, 
#so the model was good
