################################## HR Case Study Solution ##################################
############################################################################################

## I. Business Understanding
## II. Import and Setup
## 1. Data Sanity Checks
## 2. Derived Features-I (before merging)
## 3. Merging Datasets, EDA and Visualization
## 4. Data Cleaning
## 5. Data Preparation
## 6. Derived Features-II (after mergning)
## 7. Scaling Continuous Features
## 8. Modeling
## 9. Model Evaluation

############################################################################################
################################## I. Business Understanding ###############################
############################################################################################

## Based on the past information gathered by a company about its employees, the aim is to 
## predict whether an employee will leave the company or not (attrition) and determine what
## are the factors that cause attrition . Based on this, the company can take necessary 
## steps to reduce the rate of attrition.

######################################################################################
################################# II. Import and Setup ###############################
######################################################################################

## (OPTIONAL) Install Packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("labeling")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")

## Import Libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(dplyr)
library(labeling)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)

## Set Working Directory
#setwd("E:/Troy Docs/Learning/PGDDS/Course 3/HR Analytics Case Study/PA-I_Case_Study_HR_Analytics")

## Read the source data files
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
intime_data <- read.csv("in_time.csv", stringsAsFactors = F)
outtime_data <- read.csv("out_time.csv", stringsAsFactors = F)

###########################################################################################
################################## 1. Data Sanity Checks ##################################
###########################################################################################

## 1.1 Checking the row and column counts in each file
##################################
str(general_data) # 4,410 rows and 24 columnns
str(employee_survey_data) # 4,410 rows and 4 columnns
str(manager_survey_data) # 4,410 rows and 3 columnns
str(intime_data) # 4,410 rows and 262 columnns
str(outtime_data) # 4,410 rows and 262 columnns

## 1.2 Checking for NAs in the data. NAs have been treated later in the code (Section #4.1)
##################################
sum(is.na(employee_survey_data)) # 83 NAs A relatively low number
sum(is.na(general_data)) # 28 NAs A relatively low number
sum(is.na(intime_data)) #109080 NAs. Quite possible as there are 262 columns and 4,410 rows. Each NA represents an instance when an employee did not record their time and that is quite common to happen
sum(is.na(outtime_data)) # 109080 NAs. MAkes sense due to the above reason
sum(is.na(manager_survey_data)) #0 NAs

## 1.3 Checking for defective column names
##################################
colnames(general_data)
colnames(employee_survey_data)
colnames(manager_survey_data)
colnames(intime_data)
colnames(outtime_data)

# We observe that the first column of the intime_data and outtime_data files do not have a name. For now, we can assume that it is Employee ID as the number of rows, max and min are the same (as seen in str() in #1.1) 
## BUT we have performed an EDA below to confirm it as well (Section #3.1)
colnames(intime_data)[1]<-"EmployeeID"
colnames(outtime_data)[1]<-"EmployeeID"

## 1.4 Since there are equal number of rows in each file, it is likely that they are all unique at the same grain (employee level). However, let's confirm the primary key of each file
##################################
length(unique(tolower(general_data$EmployeeID)))    # 4410, partly confirming EmployeeID is key 
length(unique(tolower(employee_survey_data$EmployeeID))) # 4410, partly confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, partly confirming EmployeeID is key
length(unique(tolower(intime_data$EmployeeID))) # 4410, partly confirming EmployeeID is key
length(unique(tolower(outtime_data$EmployeeID))) # 4410, partly confirming EmployeeID is key


## 1.5 Now that we have equal number of rows in all the files, it is also necessary to confirm if the exact same EmployeeIDs are present in all the files. We compare all the files with the general_data dataframe
##################################
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,intime_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,outtime_data$EmployeeID) # Identical EmployeeID across these datasets

## Hence, it is confirmed that only are there same number of EmployeeIDs in all the files but they are all information of the same employees as well

## 1.6 Checking if any column is completely populated by NA and taking necessary steps
##################################

general_data_cols <- colnames(general_data)[which(sapply(general_data,function(x) sum(is.na(x))) == nrow(general_data))]
general_data_cols ## 0 Completely NA columns

employee_survey_NA_cols <- colnames(employee_survey_data)[which(sapply(employee_survey_data,function(x) sum(is.na(x))) == nrow(employee_survey_data))]
employee_survey_NA_cols ## 0 Completely NA columns

manager_survey_NA_cols <- colnames(manager_survey_data)[which(sapply(manager_survey_data,function(x) sum(is.na(x))) == nrow(manager_survey_data))]
manager_survey_NA_cols ## 0 Completely NA columns

intime_NA_cols <- colnames(intime_data)[which(sapply(intime_data,function(x) sum(is.na(x))) == nrow(intime_data))]
intime_NA_cols ## 12 Completely NA columns, signifying 12 days of 0 entries by employees. Probably holidays

outtime_NA_cols <- colnames(outtime_data)[which(sapply(outtime_data,function(x) sum(is.na(x))) == nrow(outtime_data))]
outtime_NA_cols ## 12 Completely NA columns, signifying 12 days of 0 entries by employees. Probably holidays

## 1.7 Are the exact same columns completely blank in both intime and outtime files?
##################################

intime_NA_cols == outtime_NA_cols
## Yes. It means those days must have been holidays in the office. Since these columns do not add any value to our data, let's drop them

intime_data <- subset(intime_data, select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01))
outtime_data <- subset(outtime_data, select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01))

## 1.8 Check whether the remaining NAs in these 2 files are on the same days, as it would mean that the employee took leave on that date
##################################

sum(is.na(intime_data)) #87,030
sum(is.na(outtime_data))#87,030
## Matches exactly with intime - No discrepancy. Means that the employees took leaves on those days

## 1.9 str(intime_data) and str(outtime_data) done above iindicated that all the date columns were in character format. Converting them to POSIXct format for better processing of dates
##################################

intime_data[,2:ncol(intime_data)] <- lapply(intime_data[,2:ncol(intime_data)], function(x) as_datetime(x))
outtime_data[,2:ncol(intime_data)] <- lapply(outtime_data[,2:ncol(intime_data)], function(x) as_datetime(x))

############################################################################################################
################################## 2. Derived Features-I (before merging) ##################################
############################################################################################################

## NOTE:
## In this section, we are deriving features from the individual files containing in-time and out-times
## In a later section (Derived Features - II), we will derive more features from the overall merged dataset based on some further EDA we will perform in the next section (section 3)
#############################################################################################
#############################################################################################

## 2.1 In order to utilize the in-time and out-time data effectively, creating a DataFrame that contains all the number of hours worked each day by the employees. Calling it "total_wh"(i.e. total working hours)
##################################

## Calculating the total working hours and storing it in a DataFrame for each EmployeeID
working_hours <- outtime_data[,2:257]-intime_data[,2:257]
total_wh <- cbind(intime_data$EmployeeID,working_hours)
total_wh$`intime_data$EmployeeID`
## Converting all the working hours into numeric
total_wh[,2:257] <- lapply(total_wh[,2:257], function(x) round(as.numeric(x),2))

## 2.2 Calculating the Average working hours for each employee and not considering NA in the calculation
##################################

total_wh$average <- round(rowMeans(total_wh[,2:257], na.rm = TRUE),2)

## Subset the required column of average working hours and renaming the columns intuitively
total_wh<-subset(total_wh, select = c("intime_data$EmployeeID","average"))
colnames(total_wh)<- c("EmployeeID","Average Hours")

################################################################################################################
################################## 3. Merging Datasets, EDA and Visualization ##################################
################################################################################################################

## 3.0 Merging all the different DFs into one DF called hr_analytics for further analysis
##################################

hr_analytics <- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, total_wh, by= "EmployeeID", all = F)

## 3.1 We need to check in the data if it made business sense to rename the unnamed column in intime_data and outtime_data to EmployeeID. We will perform a simple EDA to confirm if it was correct
##################################

wlife_eda <- hr_analytics %>% group_by(WorkLifeBalance) %>% summarise(avg_hours=mean(`Average Hours`))
wlife_eda

## We have confirmed that the decision was correct as the people with the most worklife balance (4) do work for the least number of hours on an average (7.58)

## 3.2 Now let us visualize the distribution of categorical variables with respect to attrition. We have listed the inferences of these plots below
##################################

bar_theme1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="top")
plot_grid(ggplot(hr_analytics, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Environment Satisfaction"),  
          ggplot(hr_analytics, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Job Satisfaction"),
          ggplot(hr_analytics, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Work Life Balance"),
          ggplot(hr_analytics, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Education Level")
          
          )
plot_grid(ggplot(hr_analytics, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1 + xlab("Business Travel"),
          ggplot(hr_analytics, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          
          align = "h")   

plot_grid(ggplot(hr_analytics, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_analytics, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,align = "h")
 
plot_grid( ggplot(hr_analytics, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_analytics, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(hr_analytics, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Number of Companies Worked"),
          ggplot(hr_analytics, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_analytics, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hr_analytics, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Job Involvement Level"),
          ggplot(hr_analytics, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Performance Rating"),
          ggplot(hr_analytics, aes(x=factor(PercentSalaryHike),fill=Attrition))+ geom_bar()+bar_theme1+xlab("Percent Salary Hike"),
          align = "h") 

## The inferences are as below - 
# Job Roles like Sales executive, Research Scientist and Laboratory Technician show more attrition, 
# when comapred with other job roles. 
# Education fields like Medical and Life Sciences show higher Attrition compared to others.
# Marital status - Single shows high Attrition among the group. 
# We find low or medium contrast in dependent variable, with respect to all the other variables.
# People with percent salary hike have higher attrition
# People with rating 3 have higher attrition

## 3.3 Next, we will visualize the distribution of continuous variables
##################################

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hr_analytics, aes(Age))+ geom_histogram(binwidth = 10)+ggtitle("Age Distribution of Employees"),
ggplot(hr_analytics, aes(x="",y=Age))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6,show.legend = F)+xlab("")+coord_flip(),align = "v",ncol=1)

plot_grid(ggplot(hr_analytics, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(hr_analytics, aes(x="",y=DistanceFromHome))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hr_analytics, aes(TotalWorkingYears))+ geom_histogram(binwidth = 20),
          ggplot(hr_analytics, aes(x="",y=TotalWorkingYears))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hr_analytics, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(hr_analytics, aes(x="",y=MonthlyIncome))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hr_analytics, aes(YearsAtCompany))+ geom_histogram(),
          ggplot(hr_analytics, aes(x="",y=YearsAtCompany))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(hr_analytics, aes(YearsSinceLastPromotion))+ geom_histogram(),
          ggplot(hr_analytics, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(hr_analytics, aes(YearsWithCurrManager))+ geom_histogram(),
          ggplot(hr_analytics, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(hr_analytics, aes(DistanceFromHome))+ geom_histogram(),
          ggplot(hr_analytics, aes(x="",y=DistanceFromHome))+ geom_boxplot(fill = "steel blue", colour = "black",width=0.6)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## 3.4 Next, we will visualize the variation of continuous variables with respect to the Dependent Variable using Box Plots
##################################

ggplot(hr_analytics, aes(x="",y=Age))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = F)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=DistanceFromHome))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=MonthlyIncome))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=PercentSalaryHike))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=TotalWorkingYears))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = F)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=YearsAtCompany))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = F)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

ggplot(hr_analytics, aes(x="",y=`Average Hours`))+ geom_boxplot(aes(fill = Attrition),width=0.5,show.legend = T)+xlab("Attrition")

######################################################################################
################################## 4. Data Cleaning ##################################
######################################################################################

## 4.1 Outlier Treatment and Capping
##################################

## 4.1.1 Monthly Income Outlier Treatment
quantile(hr_analytics$MonthlyIncome,seq(0,1,.01),na.rm = T)
# We find a huge rise in the 90%ile level (observed this in the box plot as well) - Thus, capping the income levels at 90th %ile with the value at 90th %ile
qn = quantile(hr_analytics$MonthlyIncome, 0.90, na.rm = TRUE)
hr_analytics = within(hr_analytics, {MonthlyIncome = ifelse(MonthlyIncome > qn, qn, MonthlyIncome)})

## 4.1.2 Total Working Years Outlier Correction
quantile(hr_analytics$TotalWorkingYears,seq(0,1,.01),na.rm = T)
# We find a rise in the 98th %ile level - Thus, capping the values above this at 98th %ile
qn = quantile(hr_analytics$TotalWorkingYears, 0.98, na.rm = TRUE)
hr_analytics = within(hr_analytics, {TotalWorkingYears = ifelse(TotalWorkingYears > qn, qn, TotalWorkingYears)})

## 4.1.3 Years Since Last Promotion Outlier Correction
quantile(hr_analytics$YearsSinceLastPromotion,seq(0,1,.01),na.rm = T)
## We find a steep rise in the 95%ile level - Thus, capping the values above it at 95%ile level.
qn = quantile(hr_analytics$YearsSinceLastPromotion, 0.95, na.rm = TRUE)
hr_analytics = within(hr_analytics, {YearsSinceLastPromotion = ifelse(YearsSinceLastPromotion > qn, qn, YearsSinceLastPromotion)})

## 4.1.4 Years At Company Outlier Correction
quantile(hr_analytics$YearsAtCompany,seq(0,1,.01),na.rm = T)
## We find a steep rise in the 98%ile level - Thus, capping values higher than it at 98th %ile value.
qn = quantile(hr_analytics$YearsAtCompany, 0.98, na.rm = TRUE)
hr_analytics = within(hr_analytics, {YearsAtCompany = ifelse(YearsAtCompany > qn, qn, YearsAtCompany)})

## 4.2 Missing Value (NA) Treatment and Imputation
##################################

## 4.2.1 Understanding the structure of the collated file
str(hr_analytics) #4410 obs. of 30 variables;
## 4.2.2 Checking which columns contain NA values
col_names_na <- colnames(hr_analytics)[colSums(is.na(hr_analytics)) > 0]
col_names_na
## Columns with NA values - EnvironmentSatisfaction, JobSatisfaction,WorkLifeBalance, NumCompaniesWorked, TotalWorkingYears

## 4.2.3 NA value treatment and Imputation of Environment Satisfaction
## Check the number of NAs in this column
sum(is.na(hr_analytics$EnvironmentSatisfaction)) #25 NAs in 4,410 rows. It is quite low. Hence, the NAs can be replaced with the most common values (median)
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))]<-median(hr_analytics$EnvironmentSatisfaction,na.rm = TRUE)

## 4.2.4 NA value treatment and Imputation of Job Satisfaction
## Check the number of NAs in this column
sum(is.na(hr_analytics$JobSatisfaction)) #20 NAs in 4,410 rows. It is quite low. Hence, the NAs can be replaced with the most common values (median)
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))]<-median(hr_analytics$JobSatisfaction,na.rm = TRUE)

## 4.2.5 NA value treatment and Imputation of  WorkLifeBalance(replace with median)
## Check the number of NAs in this column
sum(is.na(hr_analytics$WorkLifeBalance)) #38 NAs in 4,410 rows. It is quite low. Hence, the NAs can be replaced with the most common values (median)
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))]<-median(hr_analytics$WorkLifeBalance,na.rm = TRUE)

## 4.2.6 NA value treatment and Imputation of NumCompaniesWorked(replace with median)
## Check the number of NAs in this column
sum(is.na(hr_analytics$NumCompaniesWorked)) #19 NAs in 4,410 rows. It is quite low. Hence, the NAs can be replaced with the most common values (median)
hr_analytics$NumCompaniesWorked[which(is.na(hr_analytics$NumCompaniesWorked))]<-median(hr_analytics$NumCompaniesWorked,na.rm = TRUE)

## 4.2.7 NA value treatment and Imputation of TotalWorkingYears(replace with mean)
## Check the number of NAs in this column
sum(is.na(hr_analytics$TotalWorkingYears)) #9 NAs in 4,410 rows. It is quite low. Hence, the NAs can be replaced with the most common values (median)
hr_analytics$TotalWorkingYears[which(is.na(hr_analytics$TotalWorkingYears))]<-mean(hr_analytics$TotalWorkingYears,na.rm = TRUE)

## 4.2.8 Check for NA terms in dataset.No NA terms in the dataset are present as a final QC step
which(is.na(hr_analytics)) #0 NAs present now


#########################################################################################
################################## 5. Data Preparation ##################################
#########################################################################################

## 5.1 Create Dummy Variables for Categorical Variables
##################################

## 5.1.1 Attrition, Gender and Performance Rating have 2 levels. Convert them to 1's and 0's 
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)
hr_analytics$PerformanceRating <- ifelse(hr_analytics$PerformanceRating == 4, 1,0)

## 5.1.2 Create a dataframe of categorical features with greater than 2 levels 
hr_analytics_cat <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                    "BusinessTravel","Department","EducationField", "Education",
                                    "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                    "StockOptionLevel")]

## 5.1.3 Convert categorical attributes to factors
hr_analytics_cat <- data.frame(sapply(hr_analytics_cat, function(x) factor(x)))
str(hr_analytics_cat)

## 5.1.4 Create dummy values matrix 
dummies <- data.frame(sapply(hr_analytics_cat, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_cat))[,-1]))

## 5.1.5 Add the dummy variables to the dataframe and remove the old single column categorical variables
### Get indices of cateorical variables
cat_ind <- which(colnames(hr_analytics) %in% c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance", "BusinessTravel","Department","EducationField", "Education", "JobRole","MaritalStatus","JobInvolvement","JobLevel", "PerformanceRating", "StockOptionLevel"))
hr_analytics <- cbind(hr_analytics[,-cat_ind], dummies)

## 5.2 Over18, EmployeeCount and StandardHours have only 1 level for the entire dataset. Hence, they do not add any value to the prediction. We can drop them
##################################
hr_analytics <- hr_analytics[,-which(names(hr_analytics) %in% c("EmployeeCount", "Over18", "StandardHours"))]

###############################################################################################################
################################## 6. Derived Features - II (after mergning) ##################################
###############################################################################################################

## 6.1 Derived Column - Over time calculation. Categorically flags employees working overtime
##################################
hr_analytics$over_time <- ifelse(hr_analytics$`Average Hours` > 8, 1, 0)
# Overtime, 1 indicates yes while 0 = no

## 6.2 Derived Column - Flag signifying working for inadequate amount of time (Taking 7 hours as standard)
##################################
hr_analytics$inadq_time <- ifelse(hr_analytics$`Average Hours` < 7, 1, 0)

## 6.3 Derived Column - Number of day offs (NA values in the work hours are taken as day offs)
##################################
hr_analytics$dayoffs <- rowSums(is.na(working_hours)) 

## 6.4 Club Total Working Years into segments by experience
##################################
hr_analytics$fresher_flag <- ifelse(hr_analytics$TotalWorkingYears<=2,1,0)
hr_analytics$level_1_professional <- ifelse(hr_analytics$TotalWorkingYears<6 & hr_analytics$TotalWorkingYears>2,1,0)
hr_analytics$level_2_professional <- ifelse(hr_analytics$TotalWorkingYears<8 & hr_analytics$TotalWorkingYears>=6,1,0)
hr_analytics$experienced <- ifelse( hr_analytics$TotalWorkingYears>8,1,0)

## 6.5 Using Decision Tree to create interaction variables
##################################

## Since Logistic Regression does not take into account interaction between variables, we have used
## Decision Trees to observe interactions between variables and used this information to create
## meaningful derived variables that are based on interaction variables. This helps us make predictions
## more meaningfully

dtree <- rpart(Attrition~.,data=hr_analytics)
fancyRpartPlot(dtree)

## 6.5.1 Create dummy variable using decision tree: Average Working Hours< 8.2 and Total Working Years >=2.5( Less or Standard working hours and Experienced professionals)

hr_analytics$lwh_ep <- ifelse(hr_analytics$TotalWorkingYears>=2.5 & hr_analytics$`Average Hours`<8.2,1,0)

## 6.5.2 Create dummy variable using decision tree: Total Working Years >=8.5 and Years since Last Promotion >5.5 
hr_analytics$experienced_not_promoted <- ifelse(hr_analytics$TotalWorkingYears>=8.5 & hr_analytics$YearsSinceLastPromotion >5.5,1,0)

################################################################################
######################## 7. Scaling Continuous Features ######################## 
################################################################################

## Normalising the columns: Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Average Hours and dayoffs.
## This is done to ensure that numerically large variables do not have disproportionate impact on the final output

str(hr_analytics)
cont_ind <- which(colnames(hr_analytics) %in% c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "Average Hours", "dayoffs"))

hr_analytics_norm <- hr_analytics
for(i in cont_ind)
{
  hr_analytics_norm[,i]<-scale(x=hr_analytics[,i],center = TRUE,scale = TRUE)
}
summary(hr_analytics_norm)

#############################################################
######################## 8. Modeling ######################## 
#############################################################

## 8.1 Splitting the data between train and test
##################################

set.seed(100)
## Splitting into training and test data (80:20)
smp_size <- floor(0.8 * nrow(hr_analytics_norm))

## Set the seed to make the partition reproducible
train_ind <- sample(seq_len(nrow(hr_analytics_norm)), size = smp_size)

## Splitting the data into train and test. Also, dropping the EmployeeID column as it does not contribute to the prediction
train <- hr_analytics_norm[train_ind, c(2:ncol(hr_analytics_norm))]
test <- hr_analytics_norm[-train_ind, c(2:ncol(hr_analytics_norm))]

## 8.2 Creating a Correlation Matrix to get a visual representation of the correlation and for future reference. However, since the number of variables are very large, we will not use it to eliminate correlated variables and use a stepAIC function instead
##################################

corr_matrix <- cor(hr_analytics_norm)
corr_matrix

## 8.3 Performing Iterations 
##################################

## 8.3.1 Run an initial model with all variables to get an idea about the variables
model1<-glm(Attrition~.,data=train,family = 'binomial')
summary(model1)

## 8.3.2 Removing variables using stepAIC function as we see a lot of insignificant variables having high p-value 
steps <- stepAIC(model1, direction="both")
steps

## 8.3.3 Selecting variabes recommended by stepAIC function and running 2nd model
model2 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Hours` + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + experienced + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model2)
vif(model2)
## EducationField.xMedical has the second highest VIF but is lesser significant than EducationField.xLife.Sciences, which has the highest VIF. Hence, removing EducationField.xMedical


## 8.4 Running multiple iterations of the model and removing insignificant and multicolinear variables
##################################

## After each iteration, we will check the summary for p-values and the VIF for multicollinearity. We will remove insiginificant variables that are introducing multicolinearity
model3 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + `Average Hours` + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + experienced + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model3)
vif(model3)

## Removing Average Hours as it has highest VIF and lowest significance among VIF > 4 variables

model4 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + experienced + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model4)
vif(model4)

## Removing YearsAtCompany as it has the leat significance among features having VIF > 4
## We also observe that the VIFs > 4 all lie in a very narrow range of 4-5. Hence, we will focus on the significance of these high VIF variables to decide which one to drop

model5 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + experienced + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model5)
vif(model5)

## Removing experienced variable as it has second highest VIF and least significance among variables with VIF > 4

model6 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model6)
vif(model6)

## Removing YearsSinceLastPromotion as it has highest VIF and lowest significance among VIF > 4 variables

model7 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + JobInvolvement.x3 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model7)
vif(model7)

## Removing JobInvolvement.x3 as it has the least significance among variables 
model8 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model8)
vif(model8)

## All high VIF variables have high significance. Hence, we are going to remove a few variables with extremely low significance first. Removing EducationField.xMedical as it has a p-value of 0.99

model9 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model9)
vif(model9)

## Removing JobRole.xResearch.Scientist due to extremely high p-value of 0.99

model10 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                JobLevel.x5 + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                level_1_professional + level_2_professional + 
                lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model10)
vif(model10)

## Removing StockOptionLevel.x2 due to extremely high p-value of ~0.98

model11 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + JobLevel.x4 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model11)
vif(model11)

## Removing JobLevel.x4 due to extremely high p-value of ~0.97

model12 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model12)
vif(model12)

## Removing gender as it has the highest p-value

model13 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 lwh_ep + experienced_not_promoted, family = "binomial", data = train)
summary(model13)
vif(model13)

## Removing lwh_ep as it has the highest VIF and relatively higher p-value among VIF > 4 variables

model14 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model14)
vif(model14)

## Removing BusinessTravel.xTravel_Rarely as it has high VIF and is the least significant among VIF > 4 variables

model15 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model15)
vif(model15)

## Removing Department.xSales as it has the highest VIF. Removing it even though it is significant, but not more significant than Department.xResearch...Development, which is the other variable with high VIF. Removing any of the other insignificant variables is not reducing its VIF, hence we have no option but to remove this

model16 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x3 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model16)
vif(model16)

## Removing Education.x3 as it has a high p-value and a moderately high VIF
model17 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model17)
vif(model17)

## Removing PercentSalaryHike due to an extremely high p-value of 0.99

model18 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x4 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model18)
vif(model18)

## Removing Education.x4 due to a high p-value of 0.96

model19 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model19)
vif(model19)

## Removing DistanceFromHome as it has the highest p-value among insignificant variables

model20 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + Education.x5 + 
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x3 + 
                 JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model20)
vif(model20)

## Removing JobLevel.x3 as it has the highest p-value among insignificant variables

model21 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model21)
vif(model21)

## Removing JobRole.xLaboratory.Technician as it has the highest p-value among insignificant variables

model22 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + inadq_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model22)
vif(model22)

## Removing inadq_time as it has the highest p-value among insignificant variables

model23 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 StockOptionLevel.x3 + over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model23)
vif(model23)

## Removing StockOptionLevel.x3 as it has the highest p-value among insignificant variables

model24 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model24)
vif(model24)

## Removing JobRole.xSales.Executive as it has the highest p-value among insignificant variables

model25 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model25)
vif(model25)

## Removing MonthlyIncome as it has the highest p-value among insignificant variables

model26 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + JobLevel.x5 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model26)
vif(model26)

## Removing JobLevel.x5 as it has the highest p-value among insignificant variables

model27 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x2 + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model27)
vif(model27)

## Removing Education.x2 as it has the highest p-value among insignificant variables

model28 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x4 + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + 
                 level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model28)
vif(model28)

## Removing JobInvolvement.x2 as it has the highest p-value among insignificant variables

model29 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + Education.x5 +
                 JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x4 + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model29)
vif(model29)

## Removing Education.x5 as it has the highest p-value among the moderately significant variables

model30 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobInvolvement.x4 + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model30)
vif(model30)

## Removing JobInvolvement.x4 as it has the highest p-value among the moderately significant variables

model31 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model31)
vif(model31)

## Removing JobRole.xHuman.Resources as it has the highest p-value among the moderately significant variables

model32 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Representative + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model32)
vif(model32)

## Removing JobRole.xSales.Representative as it has the highest p-value among the moderately significant variables

model33 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model33)
vif(model33)

## Removing JobRole.xManager as it has the highest p-value among the moderately significant variables

model34 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_1_professional + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model34)
vif(model34)

## Removing level_1_professional as it has the highest p-value among the significant variables

model35 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model35)
vif(model35)

## Removing MaritalStatus.xMarried as it has the highest p-value among the significant variables

model36 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model36)
vif(model36)

## Removing EducationField.xTechnical.Degree as it has the highest p-value among the significant variables

model37 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 EducationField.xOther + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model37)
vif(model37)

## Removing EducationField.xOther as it has the highest p-value among the significant variables

model38 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + JobLevel.x2 + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model38)
vif(model38)

## Removing JobLevel.x2 as it has the highest p-value among the significant variables

model39 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model39)
vif(model39)

## Removing JobRole.xResearch.Director as it has the highest p-value among the significant variables

model40 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + EducationField.xMarketing + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model40)
vif(model40)

## Removing Department.xResearch...Development as it has the highest p-value among the significant variables

model41 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + EducationField.xMarketing + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model41)
vif(model41)

## Removing EducationField.xMarketing as it has the highest p-value among the significant variables 

model42 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model42)
vif(model42)

## Removing StockOptionLevel.x1 as it has the highest p-value among the significant variables

model43 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model43)
vif(model43)

## Removing TrainingTimesLastYear as it has the highest p-value among the significant variables

model44 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + level_2_professional + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model44)
vif(model44)

## Removing level_2_professional as it has the highest p-value among the significant variables

model45 <- glm(formula = Attrition ~ Age + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model45)
vif(model45)

## Removing Age as it has the highest p-value among the significant variables

model46 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model46)
vif(model46)

## Removing JobSatisfaction.x2 as it has relatively higher p-value among remaining variables

model47 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model47)
vif(model47)

## Removing JobSatisfaction.x3  as it has a high p-value

model48 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model48)
vif(model48)

## Removing WorkLifeBalance.x3 even though it has a low p-value because it has a high VIF value among remaining variables

model49 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model49)
vif(model49)

## Removing WorkLifeBalance.x4 as it has a high p-value

model50 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model50)
vif(model50)

## Removing WorkLifeBalance.x2 as it has a high p-value

model51 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + over_time + fresher_flag + 
                 experienced_not_promoted, family = "binomial", data = train)
summary(model51)
vif(model51)

## 8.5 Final Model with 18 significant features
##################################

final_model<- model51

#########################################################################################
################################## 9. Model Evaluation ##################################
#########################################################################################

## 9.1 Running the model on Test data to evaluate its performance. Checking the summary of test predictions to get an idea of the range of probabilities returned
##################################

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)
test$prob <- test_pred
#View(test)

## Adding the actual churn in the test data to help in evaluation of model performance
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

## 9.2 Obtain the optimal probability cut-off to classify 0's and 1's
##################################

## In order to do so, we will calculate the Sensitivity and Specificity for each 
##possible cut-off (varying it by 0.1) and pick the cut-off that gives us Sensitivity = 
##Specificity as we are looking to balance these 2 properties within the model output

## The below function returns the accuracy, sensitivity and specificity for a given cutoff value
performance_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

## Since the values of our predictions range from 0.0008905 to 0.9403332, we will create a range of cutoff values between them with a variation of 0.1. In order to have one range each for the 3 values the function has to return, we will initialize a matrix of 100x3

summary(test_pred)
s = seq(.01,.94,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = performance_fn(s[i])
} 

## Plotting the Sensitivity and Specificity to visualize the optimal cut-off
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.60,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## We select the point of intersection of accuracy, sensitivity and specificity to be the cut-off used to classify 0's and 1's. This will ensure that the cut-off is optimal to maintain the best balance of the 3 metrics
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

## 9.3 Using the obtained cut-off to evaluate model performance
##################################

## Hence, we select a value of 0.160303 for the model and assign "Yes" and "No" accordingly
test_cutoff_attrition <- factor(ifelse(test_pred >= 0.160303, "Yes", "No"))

## 9.3.1 Creating a confusion matrix to evaluate performance
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

## 9.3.2 Obtaining the Accuracy, Sensitivity and Specificity and taking a look at the values
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc ## 0.7256236 or ~73%
sens ## 0.7318841 or ~73%
spec ## 0.7244624 or ~72%

## The model's performance seems to be quite good, hence we will go ahead with this model

## 9.3.3 Obtaining the KS-Statistic Value and decile where it occurs to evaluate model performance

test_cutoff_attrition <- ifelse(test_cutoff_attrition == "Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes",1,0)
pred_object_test <- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)
## We obtained a max KS Value of 0.4563464, whoch is above 0.4. Hence, it is a good model and is able to differentiate the true and false values effectively

## 9.3.4 Gain and Lift Charts

lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile
## We will use attrition_decile to plot the gain and lift charts in Excel and use it in the PPT.
## Upon visual inspection of the values itself, it appears to be a good model as there is a gain of 77% in the 4th decile itself, which means that 77% of the 1's are captured in the top 40% of the data itself. This means this is a good model as it is able to rank order well and provide good gains