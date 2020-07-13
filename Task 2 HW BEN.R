library(readr)
CompleteResponses <- read_csv("C:/Users/admin/Desktop/Task 2 R/Datasets/CompleteResponses.csv")
View(CompleteResponses)

#Understand Data Better
print.data.frame(CompleteResponses)
head(CompleteResponses)

#Check for missing values 
sum(is.na(CompleteResponses)) #<- 0 

#attributes of data 
names(CompleteResponses) #<- salary, age, elevel, car, zipcode, credit, brand 

#check structure of data 
str(CompleteResponses) #<- No levels for categorized values such as Level of Education, Car, Zip, Brand
# Because we are dealing with a classification problem, some of the attributes need to be converted to factors.

#Convert to factors 
CompleteResponses$brand<- as.factor(CompleteResponses$brand)
CompleteResponses$car<- as.factor(CompleteResponses$car)
CompleteResponses$elevel<- as.factor(CompleteResponses$elevel)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)

#check structure of data 
str(CompleteResponses) #<- now the attributes have factor levels. 

#rename education column
colnames(CompleteResponses)[colnames(CompleteResponses) == "elevel"] <- "education"
names(CompleteResponses)
str(CompleteResponses)

#making a copy
CompleteResponses_Copy <- data.frame(CompleteResponses)

#binning data
#Age
#create new attribute
AgeBin <- CompleteResponses_Copy$age
AgeBin

#binned Age into 3 sections
cut(AgeBin, 3)
AgeBin <- cut(AgeBin, 3)
AgeBin

#Salary
SalaryBin <- CompleteResponses_Copy$salary
SalaryBin

#binned Salary into 5 Sections 
cut(SalaryBin, 5)
SalaryBin <- cut(SalaryBin, 5)
SalaryBin

##adding binned date to Complete Responses Data Set 
CompleteResponses_Copy$AgeBin <- AgeBin
CompleteResponses_Copy$SalaryBin <- SalaryBin 

names(CompleteResponses_Copy) ##<- Verifying 
str(CompleteResponses_Copy) ##<- Bins are factored which is what we want 

##drop Age and Salary column 
CompleteResponses_Copy <- CompleteResponses_Copy[,-c(1,2)]
names(CompleteResponses_Copy)
#Key relation: AGE, SALARY, and BRAND
ggplot(CompleteResponses, aes(x=salary, y=age, col = brand)) + geom_point()


###################Train and Assess the Models##########

# create training and testing sets
library(caret)
set.seed(123)
inTraining <- createDataPartition(CompleteResponses_Copy$brand, p = .75, list = FALSE)
training <- CompleteResponses_Copy[inTraining,]
testing <- CompleteResponses_Copy[-inTraining,]

#10 fold Cross Validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# C5.0 model
c50Fit1 <- train(brand~., data = training, method = "C5.0", trControl = fitControl)
c50Fit1

#It would be interesting to know how the model prioritized each feature during the training 
#process. We can use the VarImp function from the caret package to obtain this information.

# use VarImp() to assess how the model prioritized each feature during training
varImp(object = c50Fit1)

plot(varImp(object = c50Fit1), main = "C5.0 - Variable Importance")

# Random Forest 
# random forest model with 5 different mtry values manually tuned
rfGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
rfFit1 <- train(brand~., data = training, method = "rf", trControl = fitControl, tuneGrid = rfGrid)
rfFit1

varImp(object = rfFit1)
plot(varImp(object = rfFit1), main = "Random Forest Variable Importance")

##Next step to perform the algorithm on the Test Set
library(readr)
SurveyIncomplete <- read_csv("C:/Users/admin/Desktop/Task 2 R/Datasets/SurveyIncomplete.csv")
View(SurveyIncomplete)

# inspect the first few rows
print.data.frame(head(SurveyIncomplete))

#Notice the similarity between the training and test sets? Both the complete and incomplete surveys 
#were conducted using the same questions and possible answers. However, we modified some attributes 
#back in the preprocessing step. These changes must also be done to our test set for the model to 
#function properly.

# convert the following attributes to factors
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$elevel <- as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)

colnames(SurveyIncomplete)[colnames(SurveyIncomplete) == 'elevel'] <- 'education'

# verify data structure of variables
str(SurveyIncomplete)

#binning data
#Age
#create new attribute
AgeBin <- SurveyIncomplete$age
AgeBin

#binned Age into 3 sections
cut(AgeBin, 3)
AgeBin <- cut(AgeBin, 3)
AgeBin

#Salary
SalaryBin <- SurveyIncomplete$salary
SalaryBin

#binned Salary into 5 Sections 
cut(SalaryBin, 5)
SalaryBin <- cut(SalaryBin, 5)
SalaryBin

##adding binned date to Complete Responses Data Set 
SurveyIncomplete$AgeBin <- AgeBin
SurveyIncomplete$SalaryBin <- SalaryBin 

names(SurveyIncomplete) ##<- Verifying 
str(SurveyIncomplete) ##<- Bins are factored which is what we want 

##drop Age and Salary column 
SurveyIncomplete <- SurveyIncomplete[,-c(1,2)]
names(SurveyIncomplete)

# predict brand preference
PredictionsBrand <- predict(c50Fit1, SurveyIncomplete)
summary(PredictionsBrand)

plot(PredictionsBrand)

#Based on our predictions, most customers will prefer a Sony computer. If we add the fully 
#answered surverys and the predictions together, Sony comes out on top as well. With this 
#information, the fictional sales team can begin to work on their product selection strategy.

barplot(table(PredictionsBrand))
