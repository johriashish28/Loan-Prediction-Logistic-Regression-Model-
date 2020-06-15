# Loan-Prediction-Logistic-Regression-Model

OBJECTIVE - Predict Loan Eligibility for Housing Finance Company

DESCRIPTION - Housing Finance company deals in all kinds of home loans. They have presence across all areas (urban, semi urban and rural). Company wants to automate the loan eligibility process (real time) based on customer details provided while filling online application form. So that it will save time and it should not be biased. These details are Gender, Marital Status, Education, Number of Dependents, Income, Loan Amount, Credit History and others. To automate this process, they have provided a dataset to identify the customers segments that are eligible for loan amount so that they can specifically target these customers. 

DATA OVERVIEW - 2 Data files were provided Train_Data.csv which will be used to build a ML model and Test_Data.csv is provided to predict whether loan will be approved or not.

Loan_ID                    - Unique Loan ID
Gender                     - Male/ Female
Married	                   - Applicant married (Y/N)
Dependents                 - Number of dependents
Education                  - Applicant Education (Graduate/ Under Graduate)
Self_Employed              - Self employed (Y/N)
ApplicantIncome            - Applicant income
CoapplicantIncome          - Coapplicant income
LoanAmount                 - Loan amount in thousands
Loan_Amount_Term           - Term of loan in months
Credit_History             - credit history meets guidelines
Property_Area              - Urban/ Semi Urban/ Rural
Loan_Status                - Loan approved (Y/N)


DATA ANALYSIS CODE in R - 

#Setting my working directory

setwd("C:/Users/S.K/Desktop/Analytics_Vidya_Projects/Loan_Prediction")

#Reading both the data files

train<- read.csv("Train_Data.csv")

test<- read.csv("Test_Data.csv")

#Removing Blanks from the train dataset

X<- which(!complete.cases(train))

DS<- train[-X,]

View(DS)




library(plyr)


#to check the structure of all variables

str(DSNEW)



#Recoding of factor variables

DS$Gender = revalue(DS$Gender, c("Male"="1", "Female"="2"))

DS$Married = revalue(DS$Married, c("Yes"="1", "No"="0"))

DS$Dependents = revalue(DS$Dependents, c("0"="0", "1"="1", "2"="2", "3+"="3"))

DS$Education = revalue(DS$Education, c("Graduate"="1", "Not Graduate"="2"))


DS$Self_Employed = revalue(DS$Self_Employed, c("Yes"="1", "No"="0"))

DS$Property_Area = revalue(DS$Property_Area, c("Urban"="1", "Semiurban"="2", "Rural"="3"))

DS$Loan_Status = revalue(DS$Loan_Status, c("Y"="1", "N"="0"))



test$Gender = revalue(test$Gender, c("Male"="1", "Female"="2"))

test$Married = revalue(test$Married, c("Yes"="1", "No"="0"))

test$Dependents = revalue(test$Dependents, c("0"="0", "1"="1", "2"="2", "3+"="3"))

test$Education = revalue(test$Education, c("Graduate"="1", "Not Graduate"="2"))

test$Self_Employed = revalue(test$Self_Employed, c("Yes"="1", "No"="0"))

test$Property_Area = revalue(test$Property_Area, c("Urban"="1", "Semiurban"="2", "Rural"="3"))



#Building logistic regression model using all variables

model <- glm(formula = Loan_Status ~ . ,

family = binomial,

data = DS)

summary(model)





#Building final model by using only significant variables only.

model <- glm(formula = Loan_Status ~ Credit_History + Property_Area ,

family = binomial,

data = DS)

summary(model)



#Predicting the Loan status based on the model build using train_data  

proc_pred = predict(model, type = 'response', newdata = DS)

DS$Pred = ifelse(proc_pred > 0.5, 1, 0)





#Creating "Confusion Matrix" to check how good our model is. 

tab1<- table(Predicted = DS$Pred, Actual = DS$Loan_Status)

tab1



#Checking "Misclassification Error"

1-sum(diag(tab1))/sum(tab1)



#Predicting Loan_Status in Test_Data.csv 

proc_pred = predict(model, type = 'response', newdata = test)

test$Pred = ifelse(proc_pred > 0.5, 1, 0)



#Output File

write.csv(test, "test.csv", row.names = FALSE)



