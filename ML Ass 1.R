# ML Assignment 1

setwd("C:/Users/samgh/Desktop/Masters of Statistics and Operations Research/Year 2/Sem 1/Machine Learning/Assignments/Assignment 1")

######
# Load and clean columns of Dataset
######

heart <- read.csv('heart.csv')
head(heart)

names(heart)[1] <- 'age'
names(heart)[ncol(heart)] <- 'disease'

######
# Factorise qualitative integer variables
######

str(heart)
# Sex = 1 is male 0 is female
# CP = Chest Pain
# FBS = fasting blood sugar > 120mg/dl
# RESTECG = resting ecg result?? shouldn't it be numerical?
# EXANG = exercise induced angina
# SLOPE = the slope of the peak exercise ST segment
# CA = number of major vessels coloured by fluorosopy
# THAL = defects?? investigate further
library(tidyverse)
unique(heart$thal)
heart <- heart %>% mutate(sex = factor(sex,
                                       labels = c('Male','Female'),
                                       levels = c(0, 1)),
                          cp = factor(cp,
                                      labels = c(0,1,2,3),
                                      levels = c(0,1,2,3),
                                      ordered = TRUE),
                          fbs = factor(fbs,
                                       labels = c('Yes','No'),
                                       levels = c(1,0)),
                          exang = factor(exang,
                                         labels = c('Yes','No'),
                                         levels = c(1,0)),
                          slope = factor(slope,
                                         labels = c(0,1,2),
                                         levels = c(0,1,2),
                                         ordered = TRUE),
                          ca = factor(ca,
                                      labels = c(0,1,2,3,4),
                                      levels = c(0,1,2,3,4),
                                      ordered = TRUE),
                          thal = factor(thal, 
                                        labels = c(0,1,2,3),
                                        levels = c(0,1,2,3),
                                        ordered = TRUE))

str(heart)
summary(heart) # no NAs present in orginal dataset

######
# Investigate each variable for their distributions and univariate outliers
######

# Age
hist(heart$age) # looks like a normal dist
boxplot(heart$age) # no outliers

# Sex
barplot(table(heart$sex)) # significantly more female 

# Chest Pain
barplot(table(heart$cp)) # mostly 0 no cp

# Resting Blood Pressure
hist(heart$trestbps) # positive skew normal dist present
boxplot(heart$trestbps) # 5 outliers

# Serum Cholestoral
hist(heart$chol) # positive skew normal distribution
boxplot(heart$chol) # 5 outliers

# Fasting Blood Sugar > 120mg/dl
barplot(table(heart$fbs)) # ~85% mostly no

# Resting ECG
barplot(table(heart$restecg)) # 2 is rare, 0 or 1 account overwhelmingly most instances

# Maximum Heart Rate Achieved
hist(heart$thalach) # negatively skewed normal distribution
boxplot(heart$thalach) # 1 outlier

# Exercise Induced Angina
barplot(table(heart$exang)) # ~65% no

# ST Depression induced by Exercise Relative to Rest
hist(heart$oldpeak) # Chi-distribution?
boxplot((heart$oldpeak)) # 4 outliers

# The Slope of Peak Exercise ST Segment
barplot(table(heart$slope)) # 0 is rare, 1 or 2 take up most instances

# Number of Major Vessels Colours by Fluorosopy
barplot(table(heart$ca)) # decreases in a quadratic fashion

# Thal - whatever the hell that is...
barplot(table(heart$thal))
# metadata tells me nothing in conjunction with this data

# Target - Disease
barplot(table(heart$disease)) # even-ish distribution of yes and no

### Extra curricular thingys
# 1) label all plots/axis and use fancy colours
# 2) for continuous variables, place number of outliers on boxplots, plot distribution on histograms
# 3) for factor variables, place percentage above each bar

######
# Further Investigation
######


