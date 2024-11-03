# Assignment 3 - R code
# Jyotsna Nagaprasad Ambekar JXA210018
# Pooja Akkaladevi PXA210024
# Karthik Ram - KCR210004

# Question 5.3
# reading file
data_set  = read.csv('Table_2.6.csv')

# ols estimation 
ols = lm(wage ~ school, data = data_set); #ols_asset

# summary of ols estimate
summary(ols)

# running anova
anova(ols)


# Question 5.9
# reading file
data_set  = read.csv('Table_5.5.csv')

# ols estimation 
ols = lm(SALARY ~ SPENDING, data = data_set); ols
library(ggplot2)
# summary of ols estimate
summary(ols)
anova_results = anova(ols); anova_results
g <- ggplot(data_set, aes(SPENDING, SALARY))
g + geom_point()
g + geom_point() + geom_smooth(method=lm)  

  # confidence intervals
ols$coefficients[[2]] + (qt(0.975,49) * 0.3117)
ols$coefficients[[2]] - (qt(0.975,49) * 0.3117)  

#Q6.14

### Clear memory
rm(list=ls())

### Set working directory
setwd("C:/Users/pooja/OneDrive/UTD_Spring 2022/BUAN 6312.001/Assignments/Assignment-3")

### Install packages
#install.packages('data.table')
library('data.table')

### Read data file
sample_data = read.csv("Table_6.8.csv")

#Running the ols regression model
ols = lm(l_voverl ~ l_wage,data=sample_data)
summary(ols)

#Q7.1
### Clear memory
rm(list=ls())

### Set working directory
setwd("C:/Users/pooja/OneDrive/UTD_Spring 2022/BUAN 6312.001/Assignments/Assignment-3")

### Install packages
#install.packages('data.table')
library('data.table')

### Read data file
sample_data = read.csv("Table_7.5.csv")

#Running the ols regression model

#ols summary of first equation
ols1 = lm(Y ~ X2,data = sample_data)
summary(ols1)
#ols summary of second equation
ols2 = lm(Y ~ X3,data = sample_data)
summary(ols2)
#ols summary of third equation
ols3 = lm(Y ~ X2 + X3,data = sample_data)
summary(ols3)


#Q7.18
library(readr)
Table_7.8 <- read.csv('Table_7.8.csv')

mlr = lm(Y ~ X2 + X3 + X4 + X5, data = Table_7.8)
summary(mlr)




#Q7.23

Table_2.6 <- read.csv('Table_2.6.csv')

Table_2.6$ln_wage = exp(Table_2.6$wage)
Table_2.6$ln_education = exp(Table_2.6$school)
Table_2.6$ln_education_2 = (exp(Table_2.6$school))^2

mlr_2.6 = lm(ln_wage ~ ln_education + ln_education_2, data = Table_2.6)
summary(mlr_2.6)







