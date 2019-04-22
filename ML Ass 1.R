# ML Assignment 1

setwd(
  "C:/Users/samgh/Desktop/Masters of Statistics and Operations Research/Year 2/Sem 1/Machine Learning/Assignments/Assignment 1"
)

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
unique(heart$disease)
heart <- heart %>% mutate(
  sex = factor(
    sex,
    labels = c('Male', 'Female'),
    levels = c(0, 1)
  ),
  cp = factor(
    cp,
    labels = c(0, 1, 2, 3),
    levels = c(0, 1, 2, 3),
    ordered = TRUE
  ),
  fbs = factor(fbs,
               labels = c('Yes', 'No'),
               levels = c(1, 0)),
  exang = factor(exang,
                 labels = c('Yes', 'No'),
                 levels = c(1, 0)),
  slope = factor(
    slope,
    labels = c(0, 1, 2),
    levels = c(0, 1, 2),
    ordered = TRUE
  ),
  ca = factor(
    ca,
    labels = c(0, 1, 2, 3, 4),
    levels = c(0, 1, 2, 3, 4),
    ordered = TRUE
  ),
  thal = factor(
    thal,
    labels = c(0, 1, 2, 3),
    levels = c(0, 1, 2, 3),
    ordered = TRUE
  ),
  disease = factor(
    disease,
    labels = c('Yes', 'No'),
    levels = c(1, 0)
  )
)

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
# Further Investigation with Graphs
######

library(mlr)

summarizeColumns(heart)

library(ggplot2)

str(heart)
summary(heart)

plot(heart$age, heart$trestbps)

# Age against Chol in regard to Disease
ggplot(data = heart,
       mapping = aes(
         x = age,
         y = chol,
         shape = disease,
         col = disease
       )) +
  geom_point(size = 3) # nothing significant

# Age against Resting
ggplot(data = heart,
       mapping = aes(
         x = age,
         y = trestbps,
         shape = disease,
         col = disease
       )) +
  geom_point(size = 3) # nothing

# Disease by Sex
sex_tbl <-
  heart %>% select(sex, disease) %>%  group_by(sex, disease) %>%  summarise(count = n())

x <- ggplot(data = sex_tbl,
       mapping = aes(x = sex,
                     y = count,
                     fill = disease)) +
  geom_bar(stat = 'identity', position = 'dodge')
# males smaller sample, but more likely in terms of proporition
x 
ggplot2::ggsave(filename = 'Disease by Sex.png',
                plot = x,
                path = getwd(),
                dpi = 320,
                width = 20,
                height = 12,
                units = 'cm')


# Chest Pain in regard to Sex and Target
cp_m_tbl <-
  heart %>%  select(sex, cp, disease) %>%  filter(sex == 'Male') %>%  group_by(cp, disease) %>% summarise(count = n())
cp_f_tbl <-
  heart %>%  select(sex, cp, disease) %>%  filter(sex == 'Female') %>%  group_by(cp, disease) %>% summarise(count = n())

# Male
x2 <- ggplot(data = cp_m_tbl,
       mapping = aes(x = cp, 
                     y = count, 
                     fill = disease)) +
  geom_bar(stat = 'identity', 
           position = 'dodge') +
  labs(title = 'Chest Pain against Disease (Males only)') +
  xlab('Chest Pain Rank') + 
  ylab('Number of Cases')
# No decreases as Chest Pain increases
x2
ggplot2::ggsave(filename = 'Chest Pain against Disease (Males only).png',
                plot = x2,
                path = getwd(),
                dpi = 320,
                width = 20,
                height = 12,
                units = 'cm')


# Female
x3 <- ggplot(data = cp_f_tbl,
       mapping = aes(x = cp, 
                     y = count, 
                     fill = disease)) +
  geom_bar(stat = 'identity', 
           position = 'dodge') +
  labs(title = 'Chest Pain against Disease (Females only)') +
  xlab('Chest Pain Rank') + 
  ylab('Number of Cases')
# No decrease as Chest Pain increase 
x3
ggplot2::ggsave(filename = 'Chest Pain against Disease (Females only).png',
                plot = x3,
                path = getwd(),
                dpi = 320,
                width = 20,
                height = 12,
                units = 'cm')

# Age, Sex, Chol, Disease
x4 <- ggplot(data = heart,
             mapping = aes(x = age, y = chol, shape = sex, col = disease)) +
  geom_point(size = 4)
x4

# Resting Heart Rate, Sex, Chol, Disease
x5 <- ggplot(data = heart,
             mapping = aes(y = trestbps, x = age, shape = sex, col = disease)) +
  geom_point(size = 4)
x5


###---------------------------------###
# Meg's Factor Code
###---------------------------------###

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
