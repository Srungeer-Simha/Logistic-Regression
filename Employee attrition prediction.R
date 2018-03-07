############################################# Loading libraries ###########################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(car)
library(MASS)
library(caret)
library(ROCR)
library(reshape2)


############################################### Loading data ##############################################

employee <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general <- read.csv("general_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
manager <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

View(employee)
View(general)
View(in_time)
View(manager)
View(out_time)


############################################## Business objective #########################################

# Help reduce attrition rates
  ## Identify key variables to focus on to curb attrition
  ## Identify the most important variable in causing attrition


######################################## Data cleaning and preparation ####################################

#---------------------------------------------- Data preparation -----------------------------------------#

## Collating the data frames

length(unique(employee$EmployeeID)) == nrow(employee) # employeeID is primary key
length(unique(general$EmployeeID)) == nrow(general) # employeeID is primary key
length(unique(manager$EmployeeID)) == nrow(manager) # employeeID is primary key

setdiff(employee$EmployeeID, general$EmployeeID) # Identical employee ID across datasets
setdiff(employee$EmployeeID, manager$EmployeeID) # Identical employee ID across datasets

HR <- merge(x = employee, y = manager, by = "EmployeeID", all = F)
HR <- merge(x = HR, y = general, by = "EmployeeID", all = F)


## Calculating average working time of each employee

# renaming column 1 of in_time and out_time to Employee ID

colnames(in_time)[1] <- c("EmployeeID")
colnames(out_time)[1] <- c("EmployeeID")


# converting both data frames to POSIXct format

in_time[ , 2:ncol(in_time)] <- sapply(in_time[ , 2:ncol(in_time)], 
                                      function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"), 
                                      simplify = FALSE)
str(in_time) # data is now in date format

out_time[ , 2:ncol(out_time)] <- sapply(out_time[ , 2:ncol(out_time)], 
                                      function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"), 
                                      simplify = FALSE)
str(out_time) # data is now in date format


# calculating mean working time for each employee on each day

working_hours <- data.frame(EmployeeID = in_time$EmployeeID, 
                            out_time[ ,2:ncol(in_time)] - in_time[ ,2:ncol(in_time)])
str(working_hours) #working time has been calculated in hours for each day

# converting difftime back into numeric and then calculating mean
working_hours[ ,2:ncol(working_hours)] <- sapply(working_hours[ ,2:ncol(working_hours)], as.numeric)
working_hours$mean_hours <- rowMeans(working_hours[ ,-1], na.rm = T)


# append mean working time of each employee in 2015 to HR data frame

HR <- merge(x = HR, y = working_hours[ , names(working_hours) %in% c("EmployeeID", "mean_hours")], 
            by = "EmployeeID", all = F)

str(HR) #master file

#--------------------------------------------- Data cleaning ---------------------------------------------#

## Checking for unnecessary rows and columns

# headers and footers

head(HR, 5) # no unnecessary headers
tail(HR, 5) # no unnecessary footers

# Duplicated rows

sum(duplicated(HR)) # no duplicate rows


## Checking for NAs

sapply(HR, function(x) sum(is.na(x))) #There are NAs in several areas

# Upon inspection these NAs seem to be random gaps in data and have no hidden meaning
# let's remove them

HR <- na.omit(HR)


########################################## Exploratory data analysis ######################################

# Defining functions for categorical and continous variables

categorical <- function(var, varname){
  
  plot1 <- ggplot(HR, aes(x = factor(var), fill = factor(Attrition))) + geom_bar() + 
           geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) + 
           theme_light() + xlab(varname) + ylab("No of employees")
  
  plot2 <- ggplot(HR, aes(x = factor(var), y = (..count..)/sum(..count..), 
                          fill = factor(Attrition))) + geom_bar(position = "fill") + 
           theme_light() + xlab(varname) + ylab("% employees")
  
  grid.arrange(plot1, plot2, nrow = 2)
}

continous <- function(var, bin, varname){
  
  plot1 <- ggplot(HR, aes(x = var, fill = factor(Attrition))) + geom_histogram(binwidth = bin) + 
           theme_light() + ylab("No of employees") + xlab(varname)
  
  plot2 <- ggplot(HR, aes(x = var, fill = factor(Attrition))) + xlab(varname) +
           geom_histogram(position = "fill", binwidth = bin) + theme_light() + ylab("% employees")
  
  plot3 <- ggplot(HR, aes(x = factor(0), y = var)) + geom_boxplot() + 
           theme_light() + xlab(NULL) + scale_x_discrete(breaks = NULL) + ylab(varname)
  
  grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,1,3), c(2,2,2,3)))
}

# 1. % Attrition rate

ggplot(HR, aes(x = factor(Attrition), y = (..count..)/sum(..count..))) + geom_bar() + 
      geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1)) + 
      theme_light() + scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
      ylab("% total") + xlab("Attrition")

# 16% of employees have attritioned in 2015

# 2. Distribution by environment satisfaction

categorical(HR$EnvironmentSatisfaction, "Environment Satisfaction")

# majority of empoyees are in the higher satisfaction range
# employees with "low" satisfaction level have higher attrition rate than other levels

# 3. Distribution by job satisfaction

categorical(HR$JobSatisfaction, "Job Satisfaction")

# similar trend seen here as in environment satisfaction
# people with "low" job satisfaction are more likely to quit

# 4. Distribution by Work life balance

categorical(HR$WorkLifeBalance, "Work Life balance")

# most empployees indicate "better" work life balance
# similar observation to satisfaction levels, low work life balance more likely to quit

# 5. Distribution by Job involvement

categorical(HR$JobInvolvement, "Job Involvement")

# most employees are rated as highly involved in their job, as expected those with low involvement
# are more likely to quit or get fired, but differnce is not so great in this case

# 6. Distribution by performance rating

categorical(HR$PerformanceRating, "performance rating")

# Nobody has received a low performance rating and there is negligible differnce in attrition rate

# 7. Distribution with age

continous(HR$Age, 5, "Age")

# majority of the staff are in middle ages 30-45 years old
# attrition is highest among the youngsters ~ 50% and decreases with age 

# 8. Distribution with frequency of business travel

categorical(HR$BusinessTravel, "Frequency of business travel")

# majority of staff travel rarely and people who travel frequently have highest attrition rate
# perhaps travelling frequently is not desirable?

# 9. Distribution by department

categorical(HR$Department, "Department")

# most staff are in research and development
# the relatively tiny HR department seems to have a high attrition rate

by_department <- HR %>%
                 group_by(Department) %>%
                 summarise(jobsatisfaction = mean(JobSatisfaction), WLB = mean(WorkLifeBalance), 
                          environmentsaisfaction = mean(EnvironmentSatisfaction)) 

plot1 <- ggplot(by_department, aes(x = factor(Department), y = jobsatisfaction)) + 
         geom_col(fill = "blue") + theme_light() + xlab("Department") +
         scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5))

plot2 <- ggplot(by_department, aes(x = factor(Department), y = WLB)) + 
         geom_col(fill = "red") + theme_light() + xlab("Department") +
         scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5))
         
plot3 <- ggplot(by_department, aes(x = factor(Department), y = environmentsaisfaction)) + 
         geom_col(fill = "green") + theme_light() + xlab("Department") + 
         scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5))
         
grid.arrange(plot1, plot2, plot3, nrow = 3)

# all three departments show uniform values of employee satisfaction metrics
# this not the cause of high attrition in HR department

# 10. Distribution by distance from home

continous(HR$DistanceFromHome, 5, "distance from home")

# most people live close to the office, travel distance doesnt seem to affect attrition rates

# 11. Distribution of education

categorical(HR$Education, "Education")

# most staff has a bachelors degree, attrition rate is similar across all education levels

# 12. Distribution by field of education

categorical(HR$EducationField, "Education field")

# most staff are educated in life sciences and medical
# relatively small number of staff with an education of HR have very high attrition rate

HR %>%
filter(Department == "Human Resources") %>%
ggplot(aes(x = EducationField)) + geom_bar() + theme_light() + labs(title = "education of HR staff")

# this could be correlated to the high attrition of  HR staff in general

# 13. Distribution by Gender

categorical(HR$Gender, "Gender")

# Most staff is male but there is no difference in attrition rate

# 14. Distribution by Job level

categorical(HR$JobLevel, "Job Level")

# most staff are in the lower job levels, as expected
# attrition rates isnt very different by job level

# 15. Distribution by job role

plot1 <- ggplot(HR, aes(x = factor(JobRole), fill = factor(Attrition))) + geom_bar() + 
         geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) + 
         theme_light() + xlab("Job Role") + ylab("No of employees") + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(HR, aes(x = factor(JobRole), y = (..count..)/sum(..count..), 
                        fill = factor(Attrition))) + geom_bar(position = "fill")+ 
         theme_light() + xlab("Job Role") + ylab("% employees") + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot1, plot2, nrow = 2)

# Higher attrition rate can be seen in research and sales related roles

# 16. Distribution by marital status

categorical(HR$MaritalStatus, "Marital Status")

# Most staff is married, markedly higher attrition rate among single staff

ggplot(HR, aes(x = Age)) + geom_histogram(binwidth = 5) + facet_grid(~ MaritalStatus) +
      theme_light() + ylab("no of employees")

# this is partly correlated to higher attrition rate among younger staff as they tend to be unmarried
# but plenty of older staff seem to be single as well

# 17. Distribution by monthly income

continous(HR$MonthlyIncome, 10000, "Monthly income")

# salaries show expected distribution with majority people in lower salaries, 
# no real trend seen with attrition

# 18. Distribution with Number of companies worked

continous(HR$NumCompaniesWorked, 1, "Number of companies worked")

# majority staff are less experienced, more experienced staff >5 years have a higher attrition rate

# 19. Distribution by percent salary hike

continous(HR$PercentSalaryHike, 3, "percent salary hike")

# salary hikes show an expected trend, no of people decreasing with hike received
# people with higher hikes have a higher attrition rate

# 20. Distribution by Stock option level

categorical(HR$StockOptionLevel, "Stock option level")

# Most staff have limited stock options and attition rate is similar amongs them

# 21. Distribution by total working years

continous(HR$TotalWorkingYears, 5, "Total working years")

# Most staff is of low experience as seen before
# people with very high experience ~ 40 years have high attrition rate, this is probably due to retirement

# 22. Distribution with no of trainings last year

categorical(HR$TrainingTimesLastYear, "No of trainings last year")

# Majority people have attended a few trainings in the year
# doesnt seem to be a big indicator of attrition

# 23. Distribution by years at company

continous(HR$YearsAtCompany, 5, "Number of years worked at the company")

# No new insights made here, most people are young staff, there are some senior folk

# 24. Distribution with years since last promotion

continous(HR$YearsSinceLastPromotion, 3, "Years since last promotion")

# a lot of people received promotions recently
# surprisingly attrition rate is similar acorss the board

# 25. Distribution based on years with current manager

continous(HR$YearsWithCurrManager, 5, "Years with current manager")

# Attrition rate decreasing with number of years spent under the same manager

# 26. Distribution with mean hours spent in the office

continous(HR$mean_hours, 2, "Mean hours at the office")

# People who spend longer hours in the office have a clearly higher attrition rate


## Generating correlation matrix

cormat <- cor(HR[ , c(2:7, 11, 16, 19:20, 22, 24:30)])

# plotting the correlation matrix

melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
      xlab(NULL) + ylab(NULL)

# Percent salary hike and performance rating are very highly correlated
# Age and total working years are well correlated, to be expected
# years at company, years since promotion, years with same manager and total working years are well correlated


## Key findings:

# 1. measures of employement satisfaction are good indicators of attrition
# 2. HR department has high attrition rate
# 3. Young people and single people have higher attrition rate
# 4. variables related to experience are correlated and are indicators of attrition (due to retirement)
# 5. People who work long hours are much more likely to quit
# 6. Staff that travel a lot also have high chance of quitting

################################## Data preparation for model building ################################

## Converting categorical varibales into factors

str(HR)

# Observations:
# 1. Environment satisfaction, job satisfaction, work life balance, job involvement, 
#    performance rating need to be made into factors
# 2. Attrition, business travel, department, education, education field, gender need to be made factors
# 3. Job level, job role, marital status, over 18 and stock option level need to be converted

data.frame(cbind(names(HR), 1:ncol(HR))) # tabulating column names and column numbers

HR_fact <- data.frame(sapply(HR[ ,c(2:6,8:10,12:13,15:18,24)], factor))

str(HR_fact) 

# There are someof variables which either have just one level or are not required for analysis, 
# these can be dropped

#Employee ID    - Id of an employee
#Employee Count - The value is 1 for the entire range
#Over 18        - All the employees are over 18
#Standard Hours - The value is same for all the employees (8 hours)


## Scaling of continous variables

HR_scaled <- data.frame(sapply(HR[ , c(7,11,19,20,22,25:30)], scale)) 

str(HR_scaled) # all continous variables are now scaled

## Dummy variable creation

dummies <- data.frame(sapply(HR_fact, function(x) data.frame(model.matrix(~x, data = HR_fact))[ , -1]))


## Combining dummies with scaled variables

attrition <- cbind(HR_scaled, dummies) #final data frame for model building


############################################## Model Building #############################################

## creating testing and training datasets

set.seed(100)
trainindices = sample(1:nrow(attrition), 0.7*nrow(attrition))
train = attrition[trainindices, ]
test = attrition[-trainindices, ] 


## Model Building

model1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model1)

# Null deviance = 2625 , residual deviance = 2002, AIC = 2114.4
# Very large number of insignificant variables

## Applying StepAIC

stepAIC(model1, direction = "both")

# Several variables have been eliminated

model2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1, 
              family = "binomial", data = train)

summary(model2)

# Null deviance = 2625 , residual deviance = 2012, AIC = 2081.8
# A lot more variables are now significant
# no major increase in deviance, reduction in AIC seen

data.frame(vif(model2))

# High multicollinearity not seen
# lets remove educational field technical degree, highest p-value

model3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1, 
              family = "binomial", data = train)

summary(model3)

data.frame(vif(model3))

# Null deviance = 2625 , residual deviance = 2014, AIC = 2082.4
# Lets remove education.x3, highest p-value

model4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x4 + Education.x5 + EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1, 
              family = "binomial", data = train)

summary(model4)

data.frame(vif(model4))

# Null deviance = 2625 , residual deviance = 2017, AIC = 2083.1
# other education parameters have become insignificant, remove education.x4

model5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x5 + EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1, 
              family = "binomial", data = train)

summary(model5)

data.frame(vif(model5))

# Null deviance = 2625 , residual deviance = 2018, AIC = 2082.1
# continue with removal of education.x5

model6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1, 
              family = "binomial", data = train)

summary(model6)

data.frame(vif(model6))

# Null deviance = 2625 , residual deviance = 2020, AIC = 2082.1
# Stock option level1 has highest p-value lets, remove

model7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model7)

data.frame(vif(model7))

# Null deviance = 2625 , residual deviance = 2023, AIC = 2083.1
# job level x2 has highest p-value, lets remove

model8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model8)

data.frame(vif(model8))

# Null deviance = 2625 , residual deviance = 2029, AIC = 2085.1
# education field other has highest p-value, lets remove

model9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model9)

data.frame(vif(model9))

# Null deviance = 2625 , residual deviance = 2033, AIC = 2087
# percent salary hike has highest p-value, lets remove

model10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model10)

data.frame(vif(model10))

# Null deviance = 2625 , residual deviance = 2036, AIC = 2088
# job role research director has highest p-value, lets remove

model11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.x5 + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model11)

data.frame(vif(model11))

# Null deviance = 2625 , residual deviance = 2040, AIC = 2090
# job level x5 has highest p-value, lets remove

model12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3 + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model12)

data.frame(vif(model12))

# Null deviance = 2625 , residual deviance = 2045, AIC = 2093
# job involvement x3 has highest p-value, lets remove

model13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model13)

data.frame(vif(model13))

# Null deviance = 2625 , residual deviance = 2050, AIC = 2096
# business travel rarely has high p-value and VIF, lets remove

model14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently+ 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model14)

data.frame(vif(model14))

# Null deviance = 2625 , residual deviance = 2058, AIC = 2101
# job satisafaction x2 has highest p value, lets remove

model15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently+ JobRole.xManufacturing.Director +
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model15)

data.frame(vif(model15))

# Null deviance = 2625 , residual deviance = 2064, AIC = 2106.7
# job satisafaction x3 has highest p value, lets remove

model16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently+ JobRole.xManufacturing.Director +
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model16)

data.frame(vif(model16))

# Null deviance = 2625 , residual deviance = 2070, AIC = 2110.7
# job role manufacturing director has highest p value and high VIF value, lets remove

model17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently+ 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model17)

data.frame(vif(model17))

# Null deviance = 2625 , residual deviance = 2079, AIC = 2117.4
# training times last year has highest p value and high VIF value, lets remove

model18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + mean_hours + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently+ 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)

summary(model18)

data.frame(vif(model18))

# Null deviance = 2625 , residual deviance = 2089, AIC = 2125.4
# All variable are now at *** level significance, but the department variables have high VIF
# Model 18 is our final model

# the significant variables remaining in the final model corroborate with findings from EDA
  # Age, Single marital status, employee satisfaction, HR department, long working hours etc.


############################################ Model Evaluation ############################################

## Predicting probabilites of attrition using test data

test$test_prob <- predict(model18, type = "response", newdata = test[ , !names(test) %in% c("Attrition")])


## Estimating cut off value

# converting 1 and 0 of attrition into Yes and No 

test_actual_attrition <- factor(ifelse(test$Attrition == 1,"Yes","No"))

# defining a function to tabulate sensitivity, specificity and accuracy for a range of cutoffs

perform_fn <- function(cutoff) {
  predicted_attrition <- factor(ifelse(test$test_prob >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(cutoff, sens, spec, acc))) 
  colnames(out) <- c("cutoff", "sensitivity", "specificity", "accuracy")
  return(out)
}

# testing cut off from 0.01 to 0.8

s = seq(.01,.80,length=100) #example range for s
OUT = matrix(0,100, 4)
colnames(OUT) <- c("cutoff", "sensitivity", "specificity", "accuracy")

for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 

# finding cut off value with optimum accuracy, sensitivity and specificity

cutoff <- s[which(abs(OUT[,2]-OUT[,3]) < 0.02)]
cutoff

# Plotting trend of sensitivity, specificity and accuracy

plot(s, OUT[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,0.1),seq(0,1,0.1),cex.lab=1.5)
axis(2,seq(0,1,0.1),seq(0,1,0.1),cex.lab=1.5)
lines(s,OUT[,3],col="darkgreen",lwd=2)
lines(s,OUT[,4],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# optimum cutoff value is 0.17


## Creating a confusion matrix 

test$test_attrition <- factor(ifelse(test$test_prob >= 0.17, "Yes", "No"))

confmatrix <- confusionMatrix(test$test_attrition, test_actual_attrition, positive = "Yes")
confmatrix

# with model 18 and cutoff of 0.17, a model of reasonable accuracy has been achieved
# Accuracy = 74%
# Sensitivity = 76%
# Specificity = 74%


## Checking KS Statistic

test_attrition <- ifelse(test$test_attrition == "Yes",1,0)

pred_object_test<- prediction(test_attrition, test$Attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
                (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) 

# KS statistic is 50%, this is a good model


## Checking Gain and Lift charts

# defining a function to create gain and lift table

lift <- function(labels, predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels, predicted_prob))
  helper[ ,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% 
              group_by(bucket)  %>%
              summarise_at(vars(labels), funs(total = n(), totalresp=sum(., na.rm = TRUE))) %>%
              mutate(Cumresp = cumsum(totalresp),
                     Gain = Cumresp/sum(totalresp)*100,
                     Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test$Attrition, test$test_prob, groups = 10)

# plotting gain and lift charts

plot1 <- ggplot(Attrition_decile, aes(x = bucket, y = Gain)) + geom_line() + geom_point() + 
         geom_text(aes(label = round(Gain, 1)), nudge_y = 5) + theme_light() + 
         scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,100,10))

plot2 <- ggplot(Attrition_decile, aes(x = bucket, y = Cumlift)) + geom_line() + geom_point() + 
         geom_text(aes(label = round(Cumlift, 2)), nudge_y = 0.2) + geom_hline(yintercept = 1) + 
         theme_light() + scale_x_continuous(breaks = seq(1,10,1))
      
grid.arrange(plot1, plot2, nrow = 2)

# model18 shows significantly better predictive ability than a random model


############################################## Conclusion ################################################

# model18 with a cutoff of 0.17 predicts employee attrition with reasonable accuracy and 
# discriminatory power. 

# The model contains only highly significant variables with little to no multicollinearity

# Key variables indicative of employee attrition
  # 1. Employee age - young staff (18 to 25) have a high chance of attrition
  # 2. Senior staff are also prone to attrition, as expected due to voluntary or mandatory retirement
  # 3. Staff who spend a lot of hours ( > 8) at work are likely to quit
  # 4. Employees who rate "low" on employee satisfaction surveys are likely to quit
  # 5. Single staff are more likely to quit
  # 6. the HR department has also shows a relatively high attrition rate
  # 7. Staff that travel frequently are also likely to quit

# The single most important variable is age, with young employees having an attrition rate of 50%
