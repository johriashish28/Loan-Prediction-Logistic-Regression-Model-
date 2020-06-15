setwd("C:/Users/S.K/Desktop/Analytics_Vidya_Projects/Loan_Prediction")

DS<- read.csv("Train_Data.csv")
test<- read.csv("Test_Data.csv")
View(DS)

test<- test[,-1]

X<- which(!complete.cases(DS))
DS1<- DS[-X,]
View(DS1)


DSNEW <- DS1[,2:13]
View(DSNEW)

library(plyr)

#str(DSNEW)

DSNEW$Gender = revalue(DSNEW$Gender, c("Male"="1", "Female"="2"))
DSNEW$Married = revalue(DSNEW$Married, c("Yes"="1", "No"="0"))
DSNEW$Dependents = revalue(DSNEW$Dependents, c("0"="0", "1"="1", "2"="2", "3+"="3"))
DSNEW$Education = revalue(DSNEW$Education, c("Graduate"="1", "Not Graduate"="2"))
DSNEW$Self_Employed = revalue(DSNEW$Self_Employed, c("Yes"="1", "No"="0"))
DSNEW$Property_Area = revalue(DSNEW$Property_Area, c("Urban"="1", "Semiurban"="2", "Rural"="3"))
DSNEW$Loan_Status = revalue(DSNEW$Loan_Status, c("Y"="1", "N"="0"))

test$Gender = revalue(test$Gender, c("Male"="1", "Female"="2"))
test$Married = revalue(test$Married, c("Yes"="1", "No"="0"))
test$Dependents = revalue(test$Dependents, c("0"="0", "1"="1", "2"="2", "3+"="3"))
test$Education = revalue(test$Education, c("Graduate"="1", "Not Graduate"="2"))
test$Self_Employed = revalue(test$Self_Employed, c("Yes"="1", "No"="0"))
test$Property_Area = revalue(test$Property_Area, c("Urban"="1", "Semiurban"="2", "Rural"="3"))


#DSNEW$Gender = as.integer(DSNEW$Gender)
#DSNEW$Married = as.integer(DSNEW$Married)
#DSNEW$Dependents = as.integer(DSNEW$Dependents)
#DSNEW$Education = as.integer(DSNEW$Education)
#DSNEW$Self_Employed = as.integer(DSNEW$Self_Employed)
#DSNEW$Property_Area = as.integer(DSNEW$Property_Area)
#DSNEW$Loan_Status = as.integer(DSNEW$Loan_Status)


#train<- DSNEW[,10:12] 
#train[,1:2] = scale(train[,1:2])


#str(DSNEW)

#XYZ<-scale(DSNEW)
#model <- glm(Loan_Status ~ . ,train,family = "binomial")
model <- glm(formula = Loan_Status ~ Credit_History + Property_Area ,
             family = binomial,
             data = DSNEW)
summary(model)


#table(DSNEW$Loan_Status, DSNEW$Gender)

proc_pred = predict(model, type = 'response', newdata = DSNEW)
DSNEW$Pre = ifelse(proc_pred > 0.5, 1, 0)


#Confusion Matrix
tab1<- table(Predicted = DSNEW$Pre, Actual = DSNEW$Loan_Status)
tab1

#Misclassification Error
1-sum(diag(tab1))/sum(tab1)


proc_pred = predict(model, type = 'response', newdata = test)
test$Pre = ifelse(proc_pred > 0.5, 1, 0)

write.csv(test, "test.csv", row.names = FALSE)
