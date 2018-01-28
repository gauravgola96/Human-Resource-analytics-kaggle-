library(ggplot2)
library(readr)

HREmployeeAttritionwithDefinitions <- read.csv("C:\\Users\\Gaurav_Gola\\Desktop\\project\\HR\\HR.csv",header = TRUE)
hr_data = HREmployeeAttritionwithDefinitions

#hr_data = read_csv("C:/Users/Rajat/Documents/R/HREmployeeAttritionwithDefinitions.csv")
summary(hr_data)

summary(table(hr_data$EmployeeCount))
summary(table(hr_data$EmployeeNumber))
summary(table(hr_data$Over18))
summary(table(hr_data$StandardHours))


# leaving out- EmployeeCount, EmployeeNumber, Over18, StandardHours
# As they are same data value for all observation
hr_data1 = hr_data[,-c(9,10,22,27)]

str(hr_data1)
summary(hr_data1)

# creating factors
hr_data1$Education = as.factor(hr_data1$Education)
hr_data1$EnvironmentSatisfaction = as.factor(hr_data1$EnvironmentSatisfaction)
hr_data1$JobInvolvement = as.factor(hr_data1$JobInvolvement)
hr_data1$JobLevel = as.factor(hr_data1$JobLevel)
hr_data1$JobSatisfaction = as.factor(hr_data1$JobSatisfaction)
hr_data1$PerformanceRating = as.factor(hr_data1$PerformanceRating)
hr_data1$RelationshipSatisfaction = as.factor(hr_data1$RelationshipSatisfaction)
hr_data1$StockOptionLevel = as.factor(hr_data1$StockOptionLevel)
hr_data1$WorkLifeBalance = as.factor(hr_data1$WorkLifeBalance)


# exploratory analysis with categorical variables

barplot(table(hr_data1$Attrition), col = c("lightblue", 'darkred'), xlab = "Attrition", ylab = "Counts", main = "Overall Attrition levels")

barplot(table(hr_data1$Attrition, hr_data1$Department), beside = T, col = c("green", "blue"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Departments")

barplot(table(hr_data1$Attrition, hr_data1$Education), beside = T, col = c("darkgreen", "lightgreen"), names.arg = c("Below College", "College", "Bachelor", "Master", "Doctor"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Education levels")

barplot(table(hr_data1$Attrition, hr_data1$EducationField), beside = T, col = c("blue", "lightgreen"), names.arg = c("Human Resources", "Life Sciences", "Marketing", "Medical", "Other", "Technical Degree"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Education Fields")

barplot(table(hr_data1$Attrition, hr_data1$EnvironmentSatisfaction), beside = T, col = c("darkred", "orange"), names.arg = c("Low", "Medium", "High", "Very High"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Environment Satisfaction Levels")

barplot(table(hr_data1$Attrition, hr_data1$Gender), beside = T, col = c("yellow", "red"), names.arg = c("Female", "Male"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Gender")

barplot(table(hr_data1$Attrition, hr_data1$JobInvolvement), beside = T, col = c("lightyellow", "orange"), names.arg = c("Low", "Medium", "High", "Very High"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Job Involvement Levels")

barplot(table(hr_data1$Attrition, hr_data1$JobLevel), beside = T, col = c("lightgreen", "lightblue"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Job Levels")

barplot(table(hr_data1$Attrition, hr_data1$JobRole), beside = T, col = c("lightgreen", "lightblue"), names.arg = c("Sales Exec", "Research Scientist", "Lab Tech", "Manufac Dir", "Healthcare Rep", "Manager", "Sales Rep", "Research Dir", "Human Resources"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Job Role")

barplot(table(hr_data1$Attrition, hr_data1$JobSatisfaction), beside = T, col = c("blue", "green"), names.arg = c("Low", "Medium", "High", "Very High"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Job Satisfaction Levels")

barplot(table(hr_data1$Attrition, hr_data1$MaritalStatus), beside = T, col = c("darkgreen", "lightgreen"), names.arg = c("Divorced", "Married", "Single"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Marital Status")

barplot(table(hr_data1$Attrition, hr_data1$OverTime), beside = T, col = c("darkblue", "lightblue"), names.arg = c("No", "Yes"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Overtime status")

barplot(table(hr_data1$Attrition, hr_data1$PerformanceRating), beside = T, col = c("blue", "green"), names.arg = c("Low", "Good", "Excellent", "Outstanding"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Performance Ratings")

barplot(table(hr_data1$Attrition, hr_data1$RelationshipSatisfaction), beside = T, col = c("orange", "lightgreen"), names.arg = c("Low", "Medium", "High", "Very High"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Relationship Satisfaction Levels")

barplot(table(hr_data1$Attrition, hr_data1$StockOptionLevel), beside = T, col = c("orange", "lightblue"), names.arg = c("0", "1", "2", "3"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Stock Option Levels")

barplot(table(hr_data1$Attrition, hr_data1$WorkLifeBalance), beside = T, col = c("lightblue", "lightgreen"), names.arg = c("Bad", "Good", "Better", "Best"), ylab = "Counts", main = "Plot of Attrition Nos. Vs Work Life Balance Levels")

# statistical tests with categorical variables

chi_test = function(DV,IV,df){
  with(df,summary(table(df[,DV],df[,IV])))$p.value
}
var_class = sapply(hr_data1[,-2],class)
cat_var = names(var_class)[var_class=="factor"]

chiTest_pValue = sapply(cat_var,chi_test,DV = "Attrition",df = hr_data1)
imp_cat_var = names(chiTest_pValue)[chiTest_pValue<0.01]
imp_cat_var

############################################################

# exploratory analysis with numerical variables

boxplot(Age ~ Attrition, data = hr_data1, col = (c("red", "green")), main = "Age Vs Attrition", xlab = "Attrition", ylab = "Age")

boxplot(DailyRate ~ Attrition, data = hr_data1, col = (c("lightblue", "lightgreen")), main = "Daily Rate Vs Attrition", xlab = "Attrition", ylab = "Daily Rate")

boxplot(DistanceFromHome ~ Attrition, data = hr_data1, col = (c("orange", "red")), main = "Distance from home Vs Attrition", xlab = "Attrition", ylab = "Distance from Home")

boxplot(HourlyRate ~ Attrition, data = hr_data1, col = (c("blue", "green")), main = "Hourly Rate Vs Attrition", xlab = "Attrition", ylab = "Hourly Rate")

boxplot(MonthlyIncome ~ Attrition, data = hr_data1, col = (c("lightblue", "lightgreen")), main = "Monthly Income Vs Attrition", xlab = "Attrition", ylab = "Monthly Income")

boxplot(MonthlyRate ~ Attrition, data = hr_data1, col = (c("lightblue", "lightgreen")), main = "Monthly Rate Vs Attrition", xlab = "Attrition", ylab = "Monthly Rate")

boxplot(NumCompaniesWorked ~ Attrition, data = hr_data1, col = (c("orange", "lightblue")), main = "No. of Companies Worked Vs Attrition", xlab = "Attrition", ylab = "No. of Companies Worked")

boxplot(PercentSalaryHike ~ Attrition, data = hr_data1, col = (c("orange", "green")), main = "Percentage of Salary Hike Vs Attrition", xlab = "Attrition", ylab = "Percentage of Salary Hike")

boxplot(TotalWorkingYears ~ Attrition, data = hr_data1, col = (c("orange", "red")), main = "Total Working Years Vs Attrition", xlab = "Attrition", ylab = "Total Working Years")

boxplot(TrainingTimesLastYear ~ Attrition, data = hr_data1, col = (c("blue", "green")), main = "Training Times Last Year Vs Attrition", xlab = "Attrition", ylab = "Training Times Last Year")

boxplot(YearsAtCompany ~ Attrition, data = hr_data1, col = (c("lightblue", "lightgreen")), main = "Years At Company Vs Attrition", xlab = "Attrition", ylab = "Years At Company")

boxplot(hr_data1$YearsInCurrentRole ~ Attrition, data = hr_data1, col = (c("lightblue", "lightgreen")), main = "Years in Current Role Vs Attrition", xlab = "Attrition", ylab = "Years in Current Role")

boxplot(YearsSinceLastPromotion ~ Attrition, data = hr_data1, col = (c("orange", "lightblue")), main = "Years Since Last Promotion Vs Attrition", xlab = "Attrition", ylab = "Years Since Last Promotion")

boxplot(YearsWithCurrManager ~ Attrition, data = hr_data1, col = (c("orange", "green")), main = "Years With Current Manager Vs Attrition", xlab = "Attrition", ylab = "Years With Current Manager")

num_var = names(var_class)[var_class!="factor"]
ksTest_pValues = sapply(num_var,function(x)ks.test(hr_data1[,x][hr_data1$Attrition=="Yes"],
                                                   hr_data1[,x][hr_data1$Attrition=="No"])$p.value)

imp_num_var = names(ksTest_pValues)[ksTest_pValues<0.001]
imp_num_var


hr_data1$Attrition_1 = ifelse(hr_data1$Attrition == "Yes", 1, 0)

index = sample(nrow(hr_data1), 1390)
train_hr_data1 = hr_data1[index,]
test_hr_data1 = hr_data1[-index,]

#using logistic regression
lr_model =glm(Attrition_1 ~ ., family = binomial, data = train_hr_data1[,-2])
summary(lr_model)
pred_lr_model = predict(lr_model, test_hr_data1, type = "response")
class_lr = ifelse(pred_lr_model > 0.5 , 1, 0)

caret::confusionMatrix(class_lr, test_hr_data1$Attrition_1)

#using rpart
library(rpart)
model_rpart = rpart::rpart(Attrition_1 ~ ., data = train_hr_data1[,-2])
pred_rpart = predict(model_rpart, newdata = test_hr_data1)
class_rpart = ifelse(pred_rpart > 0.5 , 1, 0)
caret::confusionMatrix(class_rpart, test_hr_data1$Attrition_1)


#using decision tree
library(C50)
model_c50 = C50::C5.0(Attrition ~ ., data = train_hr_data1[,-31])
pred_c50 = predict(model_c50, newdata = test_hr_data1[,-31], type = "class")
caret::confusionMatrix(pred_c50, test_hr_data1$Attrition)


# ############# check giving erroreneous result #########
# with random forest
library(randomForest)
model_rf = randomForest::randomForest(Attrition ~ ., data = train_hr_data1[,-31], ntree=1000)
pred_rf = predict(model_rf, newdata = test_hr_data1[,-31], type = "class")
caret::confusionMatrix(pred_rf, test_hr_data1$Attrition)


library(randomForest)
model_rf = randomForest::randomForest(Attrition ~ train_hr_data1$Age+
                                        train_hr_data1$DailyRate+
                                        train_hr_data1$HourlyRate+
                                                  train_hr_data1$JobRole+
                                        train_hr_data1$MonthlyIncome+
                                        hr_data1$MonthlyRate+
                                                  train_hr_data1$OverTime+
                                        train_hr_data1$TotalWorkingYears, data = train_hr_data1[,-31], ntree=1000)

pred_rf = predict(model_rf, newdata = test_hr_data1[,-31], type = "class")
caret::confusionMatrix(pred_rf, test_hr_data1$Attrition)



#using naive bayes
library(e1071)
model_nb = naiveBayes(train_hr_data1[,-c(2,31)], train_hr_data1$Attrition)
pred_nb = predict(model_nb, test_hr_data1)
caret::confusionMatrix(pred_nb, test_hr_data1$Attrition)

# using svm
model.svm = svm(Attrition_1 ~ ., data = train_hr_data1[,-2], scale = T)
predict.svm = predict(model.svm, newdata = test_hr_data1[,-2])
class_svm = ifelse(predict.svm > 0.5 , 1, 0)

caret::confusionMatrix(class_svm, test_hr_data1$Attrition_1)

library(usdm)
vif(hr_data1)

library(FSelector)
library(rJava)
f = paste("Attrition~", paste(names(hr_data1[,-c(2, 32)]),collapse = "+"))
F = cfs(formula = as.formula(f), data = hr_data1[,-32])
F
# Age, MonthlyIncome, OverTime, StockOptionLevel, TotalWorkingYears, YearsWithCurrManager

################------------------Feature selection---------------------------------------###########################
library(readr)
HREmployeeAttritionwithDefinitions <- read.csv("/media/jibesh/5AD20696D2067693/Praxis/3rd_Trimester/HRA/HREmployeeAttritionwithDefinitions.csv",header = TRUE)
hr_data = HREmployeeAttritionwithDefinitions

library('Metrics')
library('randomForest')
library('ggplot2')
library('ggthemes')
library('dplyr')
set.seed(101)
dim(hr_data)

##specifying outcome variable as factor

hr_data$Attrition= as.factor(hr_data$Attrition)
hr_data$Time = NULL

model_rf<-randomForest(hr_data$Attrition ~ ., data = hr_data)
importance(model_rf)
sort(importance(model_rf),decreasing = T)

  
