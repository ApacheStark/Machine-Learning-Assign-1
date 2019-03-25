# Load Packages #
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(mlr)
library(outliers)

#----------------------------------------------------------------------------------------------------------#

# Read Data #

setwd("C:\\Users\\marga\\OneDrive\\Documents\\Uni\\Machine Learning")

heart <- read.csv("heart.csv")


#----------------------------------------------------------------------------------------------------------#

# Understand the Data#

head(heart)
tail(heart)
dim(heart)
str(heart)
names(heart)
class(heart)

#Findings: 14 attributes for 303 observations

#Attributes:

# > 1. age 
# > 2. sex 
# > 3. chest pain type (4 values) 
# > 4. resting blood pressure 
# > 5. serum cholestoral in mg/dl 
# > 6. fasting blood sugar > 120 mg/dl
# > 7. resting electrocardiographic results (values 0,1,2)
# > 8. maximum heart rate achieved 
# > 9. exercise induced angina 
# > 10. oldpeak = ST depression induced by exercise relative to rest 
# > 11. the slope of the peak exercise ST segment 
# > 12. number of major vessels (0-3) colored by flourosopy 
# > 13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect


#Ideas: 
# - Change Column Names
# - Change Sex to M and F (factor)
# - Change chest pain type to ordered factors (names? mild to severe?)
# - Change "thal" numerical values to anmed factors normal, fixed defect, reversable defect

#----------------------------------------------------------------------------------------------------------#

# Tidy the Data #
 
colnames(heart) <- c('Age', 
                     'Sex', 
                     'Chest Pain', 
                     'Rest. Blood Pressure', 
                     'Cholestoral (mg/dl)', 
                     'Fast. Blood Sugar (>120mg/dl)',
                     'Resting ECG',
                     'Max Heart Rate',
                     'Ex. Induced Angina',
                     'Old Peak',
                     'Slope',
                     'No. of Blood Vessels',
                     'Thalessemia',
                     'Target')
head(heart)                     
str(heart)



heart$Sex <- factor(heart$Sex,
                    levels = c(1,0), 
                    labels = c("Male", "Female"))

heart$`Chest Pain` <- factor(heart$`Chest Pain` ,
                    levels = c(0,1,2,3), 
                    labels = c("Typical Angina", "Atypical Angina", "Non-Anginal", "Asymptomatic"))

heart$`Fast. Blood Sugar (>120mg/dl)` <- factor(heart$`Fast. Blood Sugar (>120mg/dl)` ,
                             levels = c(0,1), 
                             labels = c("FALSE", "TRUE"))

heart$`Resting ECG` <- factor(heart$`Resting ECG` ,
                                                levels = c(0,1,2), 
                                                labels = c("Normal", "ST-T Abnormal", "Hypertrophy"))

heart$`Ex. Induced Angina` <- factor(heart$`Ex. Induced Angina` ,
                                    levels = c(0,1), 
                                    labels = c("No", "Yes"))

heart$Slope <- factor(heart$Slope ,
                                     levels = c(0,1,2), 
                                     labels = c("Upsloping", "Flat", "Downsloping"))

heart$`Thalessemia` <- factor(heart$`Thalessemia` ,
                                     levels = c(1,2,3), 
                                     labels = c("Normal", "Fixed Defect", "Reversable Defect"))

heart$Target <- factor(heart$Target,
                       levels = c(0,1),
                       labels = c("No", "Yes"))

str(heart)
head(heart)

summary(heart)

#----------------------------------------------------------------------------------------------------------#

# Scan the Data #

# Missing Values #

which(is.na(heart)) 
colSums(is.na(heart))
rowSums(is.na(heart))

#Only a couple of NAs in the Thalessemia column. Can replace with mode value of Thalessemia. 

summary(heart$`Thalessemia`)

#Mode value is "Fixed Defect".

heart$`Thalessemia`[is.na(heart$`Thalessemia`)] <- "Fixed Defect" #Look at relationships between thal and other variables to find best mode for NAs

which(is.na(heart)) 
summary(heart$`Thalessemia`)

#Check for special values
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x)
}

sapply(heart, is.special)

#None found.

# Outliers #

str(heart)

z.scores <- heart$`Rest. Blood Pressure` %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

#2 Outliers for Resting Blood Pressure

z.scores <- heart$`Cholestoral (mg/dl)` %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

#4 Outliers for Resting Blood Pressure

z.scores <- heart$`Max Heart Rate` %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

#1 Outlier for Max Heart Rate

z.scores <- heart$`Old Peak` %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

#2 Outliers for Old Peak

#Univariate Box Plots of Numeric Variables
boxplot(heart$`Rest. Blood Pressure`, main = "Resting Blood Pressure")
boxplot(heart$`Rest. Blood Pressure`, main = "Resting Blood Pressure")
boxplot(heart$`Cholestoral (mg/dl)`, main = "Cholestoral")
boxplot(heart$`Old Peak`, main = "Old Peak")

#Histograms
hist(heart$`Age`, main = "Sample Distribution of Age")
hist(heart$`No. of Blood Vessels`, main = "No. of Blood Vessels Coloured by Flourosopy")
