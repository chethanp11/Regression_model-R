#### SIMPLE LINEAR REGRESSION #################
#### SIMPLE LINEAR REGRESSION #################
#### SIMPLE LINEAR REGRESSION #################


## Importing the dataset #############
## Importing the dataset #############
setwd("./data/")
getwd()
dir()


ls()
# Alt+Shift+K gives cheat sheet
# Slelect function and hit F1 for help - shortcut
dataset = read.csv('Salary_Data.csv')
print(dataset)
head(dataset)
summary(dataset)
library(psych)
describe(dataset)
describeBy(dataset) # useful when we want to group by
attr(dataset,"names")
attr(dataset, "dim")
rownames(dataset)
colnames(dataset)



## Splitting the dataset into the Training set and Test set #############
## Splitting the dataset into the Training set and Test set #############
# install.packages('caTools')
library(caTools) # sample.split is from this caTools package
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set
test_set



## Feature Scaling (Not required) ##############
## Feature Scaling (Not required) ##############
training_set2 = scale(training_set)
describe(training_set2)
test_set2 = scale(test_set)
test_set2



## Fitting Simple Linear Regression to the Training set ##############
## Fitting Simple Linear Regression to the Training set ##############
library(stats)
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
summary(regressor)



## Predicting the Test set results ############
## Predicting the Test set results ############
y_pred = predict(regressor, newdata = test_set)
summary(y_pred)



## Visualising the Training set results ############
## Visualising the Training set results ############
# install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary) ,
             color = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)) , 
                color = "green" ) +
  ggtitle("Salary vs Experience (Training Set)") +
  xlab("Years of Experience") +
  ylab("Salary")

  
  
## Visualising the Test set results ############
## Visualising the Test set results ############
ggplot() +
  geom_point(aes(x = test_set$YearsExperience , y = test_set$Salary) ,
            color = "red") +
  geom_line(aes(x = training_set$YearsExperience , y = predict(regressor , newdata = training_set)) , 
            color = "blue") +
  ggtitle("Salary vs Experience (Training Set)") +
  xlab("Years of Experience") +
  ylab("Salary")
  

## =================================================== ##
  
  
#### MULTIPLE LINEAR REGRESSION EXAMPLE 1 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 1 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 1 #################

## Importing the dataset #############
## Importing the dataset #############
dataset = read.csv('50_Startups.csv')
summary(dataset)
describe(dataset)
describeBy(dataset, dataset$State)


# Encoding categorical data #############
# Encoding categorical data #############
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
dataset$State
					   
					   
## Splitting the dataset into the Training set and Test set #############
## Splitting the dataset into the Training set and Test set #############
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



## Feature Scaling (Not required) ##############
## Feature Scaling (Not required) ##############
training_set2 = scale(training_set)
test_set2 = scale(test_set)


# Correlation analysis  #############
# Correlation analysis  #############
cor(training_set[c(1:3,5)])


# Fitting Multiple Linear Regression to the Training set #############
# Fitting Multiple Linear Regression to the Training set #############

regressor = lm(formula = Profit ~ ., data = training_set)
summary(regressor)
confint(regressor)
#Except R.D. Spend other variables are not significant

model1 = lm(formula = Profit ~ Administration, data = training_set) # Admin charges has no impact on profit
summary(model1)
confint(model1)

regressor = lm(formula = Profit ~ R.D.Spend, data = training_set)
summary(regressor)
confint(regressor)
		   
			   
# Predicting the Test set results #############
# Predicting the Test set results #############

y_pred = predict(regressor, newdata = test_set)

plot(formula = Profit ~ .,
               data = training_set)

plot(training_set$Profit ~ subset(training_set, colnames(colnames) != "Profit"))
abline(regressor, col=2)

## =================================================== ##

#### MULTIPLE LINEAR REGRESSION EXAMPLE 2 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 2 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 2 #################

# Load packages #############
# Load packages #############
library(psych)
library(stats)
library(caTools)
library(ggplot2)

# Read data into a dataframe called FS (Faculty Salary)  #############
# Read data into a dataframe called FS (Faculty Salary)  #############
FS <- read.table("Stats1.13.Lab.06.txt", header = T)
View(FS) # If you want to view the data
#edit(FS)
summary(FS) # Summary statistics
summary(FS$dept)
describe(FS)
describeBy(FS$dept)



# Correlation analysis  #############
# Correlation analysis  #############
cor(FS[1:4])


# Encoding categorical data #############
# Encoding categorical data #############
# Now let's conduct regression analyses that include a categorical predictor
# We need to use dummy codes to represent the nominal variable (dept) as numeric 
# In R there are several ways to do this, the following is just one example, using the function C (for contrasts)

dept.code = C(FS$dept, treatment) #this is capital C for contrast to convert categorical variable to numeric and create dummy variables
dept.code


# Fitting Multiple Linear Regression to the Training set #############
# Fitting Multiple Linear Regression to the Training set #############

# Model0 demonstrates that age and years are, for the most part redundnant, so we will only use years in the next set of models

model0 <- lm(FS$salary ~ FS$age) # age variable is redundent based on p-value
summary(model0)
confint(model1)

model1 <- lm(FS$salary ~ FS$years)
summary(model1)
confint(model1)

model0 <- lm(FS$salary ~ FS$years + FS$age) # age variable is redundent based on p-value
summary(model0)
confint(model1)

model2 = lm(FS$salary ~ FS$pubs)
summary(model2)
confint(model2) 

model3 = lm(FS$salary ~ FS$years + FS$pubs)
summary(model3)
confint(model3) 


model4 = lm(FS$salary ~ FS$years + FS$pubs + (dept.code))
summary(model4)
confint(model4)

# Model Comparirision ################
# Model Comparirision ################
anova(model1, model3)
anova(model2, model3)
anova(model3, model4) # Compre Model4 to Model3 to determine if including dept improves the predictions of the model


# Model Analysis ################
# Model Analysis ################
# Let's examine the salary difference between History and Sociology
# To quickly view the means, use the tapply function
tapply(FS$salary, FS$dept, mean)

# The actual means are not that different, so why are the means predicted by the model so different?
# There must be differences across departments in years and/or pubs
# Let's look at years
tapply(FS$years, FS$dept, mean)

# Let's look at pubs
tapply(FS$pubs, FS$dept, mean)

# The actual salary for Sociology is not that different from the other departments BUT they have more years on the job and more publications, on average, than the other departments, so their PREDICTED salary, based on an AVERAGE number of years and publications is lower, which is a more accuracte refelction of the discrepancies across departments.

## =================================================== ##

#### MULTIPLE LINEAR REGRESSION EXAMPLE 3 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 3 #################
#### MULTIPLE LINEAR REGRESSION EXAMPLE 3 #################

hw = read.table("./in/Stats1.13.HW.06.txt", header=T)
head(hw)
summary(hw$years)
summary(hw$profession)
hw$profession = as.factor(hw$profession)
class(hw$profession)
class(hw$ID)
library(psych)
tab = describe(hw)
tab
class(tab)
tab = describeBy(hw, hw$profession)
class(tab)
attributes(hw)
summary(hw)
hw[1,2]
library(stats)
x= table(hw$profession)
x
rc = dim(hw)
rc
dim(hw)[2]

ncol(hw)
ls()
dir()


model1 = lm(hw$salary ~ hw$years)
summary(model1)
plot(hw$salary ~ hw$years)
abline(model1, col=2)
confint(model1)

dummy.prof = C(hw$profession, treatment)
model2 = lm(hw$salary ~ hw$years+hw$courses)
summary(model2)
confint(model2)
model3 = lm(hw$salary ~ hw$years+dummy.prof)
model4 = lm(hw$salary ~ hw$courses+dummy.prof)
model5 = lm(hw$salary ~ hw$years+hw$courses+dummy.prof)
model5
summary(model5)
confint(model5)

x = tapply(hw$salary, hw$profession, mean)
x
x[1]-x[3]
?tapply

x =anova(model2, model5)

anova(model3, model5)
anova(model4, model5)
