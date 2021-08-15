#Log Book 2
#predictions and evaluation
# ~
#set the working directory
setwd("/Users/basilisgounarismpampaletsos/Desktop/PROJECTS/27:11 logbook2")

options(scipen = 9)
#load all the libraries which we will use
library(readxl)
library(psych)
library(ggplot2)
library(caTools)
library(statsr)
library(dplyr)
library(BAS)
library(car)
library(tidyr)
library(purrr)
library(gridExtra)
library(forcats)
library(corrplot)
library(magrittr)
library(caret)
library(Hmisc)
library(tidyverse)

#read the data file
train_set <- read_excel("ames_train.xlsx")
test_set <- read_excel("ames_test.xlsx")

#summary of the data
#run summary for the first time to see basic things and summary statistics
#also we can see and find some data issues 
summary(data) 


######################
#DATA QUALITY ISSUES
######################


#Fix the data in train_set 
#deal with the outlier and data issues
train_set$SalePrice[train_set$SalePrice>700000] <- NA #fix sale price outliers and maximum
train_set$Overall.Qual[train_set$Overall.Qual<0] <- NA #values must be between 0-10
train_set$Lot.Area[train_set$Lot.Area>30000] <- NA #fix very big Lot Area sqft
train_set$Pool.Area[train_set$Pool.Area>100] <- NA #fix Pool Area limits
train_set$Year.Built[train_set$Year.Built>2020] <- NA #max date must be >= 2020
train_set$Year.Built[train_set$Year.Built<1920] <- NA #fix build year for 100 years maximum
train_set$Neighborhood[train_set$Neighborhood == "mes"] <- NA #fix some data issues
train_set$Garage.Area[train_set$Garage.Area>1100] <-NA #check for 1200

#convert varibales into factor
train_set$Bedroom.AbvGr <-as.factor(train_set$Bedroom.AbvGr)
train_set$House.Style <-as.factor(train_set$House.Style)
train_set$MS.Zoning <-as.factor(train_set$MS.Zoning)
train_set$Foundation <-as.factor(train_set$Foundation)
train_set$Neighborhood <-as.factor(train_set$Neighborhood)
train_set$Sale.Type <-as.factor(train_set$Sale.Type)
train_set$MS.SubClass <-as.factor(train_set$MS.SubClass)
train_set$Overall.Qual <-as.factor(train_set$Overall.Qual)
train_set$Kitchen.AbvGr <-as.factor(train_set$Kitchen.AbvGr)
train_set$Overall.Cond <-as.factor(train_set$Overall.Cond)
train_set$Heating.QC <-as.factor(train_set$Heating.QC)


#fix the data in test_set
test_set$SalePrice[test_set$SalePrice>700000] <- NA #fix sale price outliers and maximum
test_set$Overall.Qual[test_set$Overall.Qual<0] <- NA #values must be between 0-10
test_set$Lot.Area[test_set$Lot.Area>30000] <- NA #fix very big Lot Area sqft
test_set$Pool.Area[test_set$Pool.Area>100] <- NA #fix Pool Area limits
test_set$Year.Built[test_set$Year.Built>2020] <- NA #max date must be >= 2020
test_set$Year.Built[test_set$Year.Built<1920] <- NA #fix build year for 100 years maximum
test_set$Neighborhood[test_set$Neighborhood == "mes"] <- NA #fix some data issues
test_set$Garage.Area[test_set$Garage.Area>1100] <-NA #check for 1200

#convert varibales into factor
test_set$Bedroom.AbvGr <-as.factor(test_set$Bedroom.AbvGr)
test_set$House.Style <-as.factor(test_set$House.Style)
test_set$MS.Zoning <-as.factor(test_set$MS.Zoning)
test_set$Foundation <-as.factor(test_set$Foundation)
test_set$Neighborhood <-as.factor(test_set$Neighborhood)
test_set$Sale.Type <-as.factor(test_set$Sale.Type)
test_set$MS.SubClass <-as.factor(test_set$MS.SubClass)
test_set$Overall.Qual <-as.factor(test_set$Overall.Qual)
test_set$Kitchen.AbvGr <-as.factor(test_set$Kitchen.AbvGr)
test_set$Overall.Cond <-as.factor(test_set$Overall.Cond)
test_set$Heating.QC <-as.factor(test_set$Heating.QC)


######################
#DATA UNDERSTANDING
######################
summary(train_set)
describeBy(train_set$SalePrice, train_set$Overall.Cond)
describeBy(train_set$SalePrice, train_set$Neighborhood)
describeBy(train_set$SalePrice, train_set$SalePrice)
aggregate(SalePrice ~ Neighborhood, data = data, FUN = mean)


##################
#ASSOCIATIONS
##################
#making basic correlations
#making some linear regressions
#all these to understand my hyphothesis with 5 independent variables
cor.test(x= train_set$SalePrice, y = train_set$Lot.Area)
cor.test(x= train_set$SalePrice, y = train_set$Year.Built)
cor.test(x= train_set$SalePrice, y = train_set$Bedroom.AbvGr)


cor(train_set$SalePrice, train_set$Year.Built, method = "spearman", use = "complete.obs")
cor(train_set$SalePrice, train_set$Lot.Area, method = "spearman", use = "complete.obs")
cor(train_set$SalePrice, train_set$Bedroom.AbvGr, method = "spearman", use = "complete.obs")

#Linear Reagression
#Understanding the significance between variables
Price_Area <- lm(SalePrice ~ Lot.Area, data = train_set)
summary(Price_Area)

Price_Neigh <- lm(SalePrice ~ Neighborhood, data = train_set)
summary(Price_Neigh)

Price_Cond <- lm(SalePrice ~ Overall.Cond, data = train_set)
summary(Price_Cond)

Price_Year <- lm(SalePrice ~ Year.Built, data = train_set)
summary(Price_Year)

Price_Bedrm <- lm(SalePrice ~ Bedroom.AbvGr, data = train_set)
summary(Price_Bedrm)


######################
#DATA VISUALISATIONS
#####################

#barchart
#it shows the number of houses in each neighborhood
#there are a lot of NA because there are a wrong name in neighborhoods
#and i put it NA
ggplot(data, aes(Neighborhood)) +
  geom_bar(colour="black", mapping = aes(fill = Neighborhood))

#scatterplot
#it shows Sale price per lot area
ggplot(data = data, aes(x = SalePrice, y = Lot.Area)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Sale price by Lot area", x = "sale price ($)", y = "lot area(sqft)")

#histogram 
#it shows the average price by Overall Condition
ggplot(data = data) + 
  geom_bar(mapping = aes(x=as.factor(Overall.Cond), y = SalePrice), stat="summary", fun.y = "mean") + 
  labs(title = "average house price by overall condition", x="Overall Condition", y="Average sale price($)")

##############
#REGRESSION
##############

#building the basic model
#using the multiple linear regression model
#with 5 "best" independent variables to see how it is fit
model1 = lm(formula = SalePrice ~ Year.Built + Lot.Area + Bedroom.AbvGr + Neighborhood + Overall.Cond,
            data = train_set)
summary(model1)

#adding more independent variables to evaluate the model
model2 = lm(formula = SalePrice ~ Year.Built + Lot.Area + Bedroom.AbvGr + Neighborhood + House.Style + 
              Overall.Cond + Garage.Area + MS.SubClass,
            data = train_set)
summary(model2)

#adding more independent variables
model3 = lm(formula = SalePrice ~ Year.Built + Lot.Area + Bedroom.AbvGr + Neighborhood + House.Style + 
              Overall.Cond + Overall.Qual + Full.Bath + Foundation + Garage.Area + MS.SubClass,
            data = train_set)
summary(model3)

#adding more independent variables
model4 = lm(formula = SalePrice ~ Year.Built + Lot.Area + Bedroom.AbvGr + Neighborhood + House.Style + 
              Overall.Cond + Overall.Qual + Full.Bath + Foundation + Garage.Area + MS.SubClass  + Utilities + 
              Sale.Type,
            data = train_set)
summary(model4)


##############
#PREDICTION
#############

final_model = lm(formula = SalePrice ~ Year.Built + Lot.Area + Bedroom.AbvGr + House.Style +
                 MS.Zoning + MS.SubClass + Overall.Cond + Overall.Qual + 
                 Heating.QC + Full.Bath + Foundation + Utilities + Neighborhood + Garage.Area +
                 Sale.Type + Kitchen.AbvGr,
               data = train_set)
summary(final_model)

#make prediction using the test_set
predictor <- predict(final_model, newdata = test_set)
predictor

#evaluate the accuracy
#RMSE and r squared calculation
postResample(predictor, test_set$SalePrice)

#####################
#CHECK ASSUMPTIONS
#####################

#assumption 1
#the mean of residuals is zero
mean(final_model$residuals)

#assumption 2
#we check 2 assumpitions with this plot
#1.Homoscedasticity of residuals or equal variance
#2.Normality of residuals
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(final_model)

#assumption 3
#multicollinearity
vif(final_model)

#assumption 4
#influential cases
influence.measures(final_model)

#assumption 5
#independent residuals
durbinWatsonTest(final_model)


