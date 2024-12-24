rm(list=ls(all=T))
setwd("C:/Users/navkr/OneDrive/Documents/Rfiles")
library(DescTools)
library(car)
library(dplyr)
library(tidyr)

CaliDS <- read.csv("1553768847-housing.csv",header=TRUE)
attach(CaliDS)
length(CaliDS) #Number of columns, 10
names(CaliDS) #List variables

#Check for missing values in the dataset
sum(is.na(housing_median_age)) #0
sum(is.na(total_rooms)) #0
sum(is.na(total_bedrooms)) #207
sum(is.na(new.total_bedrooms)) #0
sum(is.na(population)) #0
sum(is.na(households)) #0
sum(is.na(median_income)) #0
sum(is.na(ocean_proximity)) #0
sum(is.na(median_house_value)) #0

#Cleaning Data
hist(CaliDS$total_bedrooms,col="lightgreen")
##Right skewed, so we will replace NA's with median
CaliDS[!complete.cases(CaliDS), ] 
median(CaliDS$total_bedrooms)
medianBedrooms <- median(CaliDS$total_bedrooms, na.rm = TRUE)
print(medianBedrooms) #435
CaliDS[is.na(CaliDS$total_bedrooms), "total_bedrooms"] <- medianBedrooms
print(CaliDS)

#Summary of variables in dataset
summary(housing_median_age)
summary(total_rooms)
summary(new.total_bedrooms)
summary(population)
summary(households)
summary(median_income)
summary(ocean_proximity)
summary(median_house_value)
boxplot(median_house_value,col="blue",ylab="Median House Values")
abline(h=mean(median_house_value),lwd=2,col="yellow")

model1 <- lm(median_house_value ~ longitude + latitude + housing_median_age + total_rooms + 
             total_bedrooms + population + households + median_income + 
             ocean_proximity)
summary(model1)
#Multiple R-squared: 0.6465
#Adjusted R-squared: 0.6463
#F-Statistic: 3112 on 12 and 20420 DF
#p-value: < 2.2e-16

#Model Equations
###################################################
#INLAND REGION EQUATION
#median_house_value = -2.270e+06 + -2.681e+04longitude + -2.548e+04latitude + 1.073e+03housing_median_age + -6.193e+00total_rooms + 
#1.006e+02total_bedrooms + -3.797e+01population + 4.962e+01households + 3.926e+04median_income + 
#-3.928e+04ocean_proximity

#ISLAND REGION EQUATION
#median_house_value = -2.270e+06 + -2.681e+04longitude + -2.548e+04latitude + 1.073e+03housing_median_age + -6.193e+00total_rooms + 
#1.006e+02total_bedrooms + -3.797e+01population + 4.962e+01households + 3.926e+04median_income + 
#1.529e+05ocean_proximity

#NEAR BAY REGION EQUATION
#median_house_value = -2.270e+06 + -2.681e+04longitude + -2.548e+04latitude + 1.073e+03housing_median_age + -6.193e+00total_rooms + 
#1.006e+02total_bedrooms + -3.797e+01population + 4.962e+01households + 3.926e+04median_income + 
#-3.954e+03ocean_proximity

#NEAR OCEAN REGION EQUATION
#median_house_value = -2.270e+06 + -2.681e+04longitude + -2.548e+04latitude + 1.073e+03housing_median_age + -6.193e+00total_rooms + 
#1.006e+02total_bedrooms + -3.797e+01population + 4.962e+01households + 3.926e+04median_income + 
#4.278e+03ocean_proximity

###################################################

confint(model1)

#Model Diagnostics
library(nortest)
ad.test(model1$residuals) #Large dataset means more variance
par(mfrow=c(2,2))
plot(model1)
vif(model1)
plot(model1$fitted.values, rstandard(model1),xlab="Fitted values",
     ylab="Standardized residuals of model 1")
mmps(model1)
detach(CaliDS)

