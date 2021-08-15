#log book 1
#program in R about Houses Dataset analysis
#what i going to do in the code
# ~

#set the working directory
setwd("/Users/basilisgounarismpampaletsos/Desktop/AUTUMN LESSONS/7177-statistics for business/log book 1")

#load the packages
library(readxl) #package for reading excel files
library(psych)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(kableExtra)
library(plotly)
library(DT)
library(viridis)
library(streamgraph)
library(hrbrthemes)
library(viridisLite)
library(dplyr)
library(scales)

#read in the ames_train data
data <- read_excel("ames_train.xlsx")

#summary of the data
#run summary for the first time to see basic things and summary statistics
#also we can see and find some data issues 
summary(data) 

#deal with the outlier and data issues
data$SalePrice[data$SalePrice>700000] <- NA #fix sale price outliers and maximum
data$Overall.Qual[data$Overall.Qual<0] <- NA #values must be between 0-10
data$Lot.Area[data$Lot.Area>30000] <- NA #fix very big Lot Area sqft
data$Pool.Area[data$Pool.Area>100] <- NA #fix Pool Area limits
data$Year.Built[data$Year.Built>2020] <- NA #max date must be >= 2020
data$Year.Built[data$Year.Built<1920] <- NA #fix build year for 100 years maximum
data$Neighborhood[data$Neighborhood == "mes"] <- NA #fix some data issues


#set variables types
data$Overall.Qual <- as.factor(data$Overall.Qual)
data$Overall.Cond <- as.factor(data$Overall.Cond)
data$MS.SubClass <- as.factor(data$MS.SubClass)
data$Utilities <- as.factor(data$Utilities)
data$House.Style <- as.factor(data$House.Style)
data$Neighborhood <- as.factor(data$Neighborhood)
data$MS.Zoning <- as.factor(data$MS.Zoning)
data$Foundation <- as.factor(data$Foundation)
data$Heating.QC <- as.factor(data$Heating.QC)
data$Full.Bath <- as.factor(data$Full.Bath)
data$Garage.Type <- as.factor(data$Garage.Type)
data$Sale.Type <- as.factor(data$Sale.Type)

####
#beginning of summary statistics

#create some table to understand character variables
#create a table in caracter vector
table(data$MS.SubClass)
table(data$MS.Zoning)
table(data$Utilities)
table(data$Neighborhood)
table(data$Overall.Cond)
table(data$Overall.Qual)
table(data$House.Style)
table(data$Sale.Type)

#i create a new dataset with summary statistics 
#i check the prices in every neighborhood
#i want to find the cheapest and the most expensive neighborhood
x <- describeBy(data$SalePrice, list(data$Neighborhood))
x2 <- do.call("rbind", x)

summary(data$Overall.Qual)


#summarise 1 variable to other one(OR with two others)
aggregate(SalePrice ~ Overall.Qual, data = data, FUN = mean)
aggregate(SalePrice ~ Heating.QC + Bedroom.AbvGr, data=data, FUN = mean)
aggregate(SalePrice ~ House.Style, data = data, FUN = mean)
aggregate(SalePrice ~ Year.Built, data = data, FUN = mean)
aggregate(SalePrice ~ MS.Zoning, data = data, FUN = mean)
aggregate(SalePrice ~ Neighborhood, data = data, FUN = mean)
aggregate(SalePrice ~ Lot.Area + Garage.Type, data=data, FUN = mean)
aggregate(SalePrice ~ Foundation + Bedroom.AbvGr, data=data, FUN = mean)
#describe 1 variable with some other

describeBy(data$SalePrice, data$Overall.Qual)
describeBy(data$SalePrice, data$Neighborhood)



#i am doing some simple plot to understand the data and choose some variables
#test and visualise summary statistics
ggplot(data, aes(Overall.Cond)) +
  geom_bar(colour="black", mapping = aes(fill = Overall.Cond))

#test with chart
chart <- ggplot(data = data)
chart + geom_bar(aes(Overall.Qual))

#test with scatterplot and mean
bchart <- ggplot(data, aes(Overall.Qual, SalePrice))
bchart + geom_boxplot() + labs(x = "house condition", y = "house price (£)")

#test, simple point the mean values
ggplot(data = data) + 
  geom_point(mapping = aes(x = Year.Built, SalePrice), 
             stat = "summary", fun.y = "mean")

#test simple scatterplot
qplot(SalePrice, Lot.Area,
      data = data,
      colour = Garage.Type,
      xlab = "Living area sq ft", ylab = "sale price",
      main = "sale price and living area")

#boxplot test
qplot(Overall.Qual, Lot.Area, data = data, geom = c("boxplot"), 
      fill = Overall.Qual)

#test, failure
#i am trying to fix but i dont deal with it
ggplot(data = data, mapping = aes(x = SalePrice, y = Year.Built, color = Foundation)) +
  geom_line() +
  facet_grid(rows = vars(Heating.QC))

#piechart shows the number of a variable
# Ms.Subclass
#i am trying this piechart with a lot of variables to understand the data
df <- as.data.frame(table(data$MS.SubClass))
colnames(df) <- c("Ms.SubClass", "freq")
pie <- ggplot(data, aes(x = "", fill = factor(MS.SubClass))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="House Style", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)

#good barchart but i make it better
ggplot(data = data) +
  geom_bar(mapping = aes(MS.SubClass, fill = Overall.Qual)) 

#test - fail
ggplot(data = data, mapping = aes(x = SalePrice, y = Year.Built, color = Foundation)) +
  geom_line() +
  facet_grid(rows = vars(Heating.QC))

#good but i fail to deal with it
#just test it
ggplot(data = data, mapping = aes(x = Lot.Area, y = SalePrice, color = Full.Bath)) + 
  geom_line() + 
  facet_grid(rows = vars(Utilities)) +
  geom_point(alpha=.1) +
  scale_size(range = c(.1, 5), name="Lot Area")


#very good graph
#just for test
#i dont use it 
#devided in small pieces
#good information through this plot
ggplot(data = data, aes(x = SalePrice, y = Lot.Area, color = Neighborhood)) + 
  geom_line() +
  facet_wrap(vars(Overall.Cond)) +
  labs(title = "Observed house's price through time", x = "price", y = "lot area") +
  theme_bw()

#test plot
ggplot(data = data) +
  geom_bar(mapping = aes(MS.SubClass, fill = Overall.Qual)) 

#########




#My final visualisations
#i select them because i think they are very good and they have a lot of informations.

#No.1
#scatter plot shows Sale Price per Lot.Area
ggplot(data = data, aes(x = Lot.Area, y = SalePrice)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Sale price by Lot area", x = "sale price ($)", y = "lot area(sqft)")

#No.2
#coloured scatterplot shows the heating quality of the houses dependent on the year.built and the sale price
ggplot(data = data, mapping = aes(x = Year.Built, y = SalePrice)) +
  geom_point(alpha = 0.5, aes(color = Heating.QC)) +
  labs(title = "The Heating Quality of the houses compared to price and year built", x = "Year Built", y = "Sale Price")

#No.3
#DotPlot it shows the dencity of the house price per quality
ggplot(data = data, mapping = aes(x = Overall.Qual, y = SalePrice)) + 
  geom_point(col = "tomato2", size = 2) + 
  geom_segment(aes(x = Overall.Qual, xend = Overall.Qual, y = mean(SalePrice), yend = mean(SalePrice)), linetype = "dashed", size = 0.1) +   
  labs(title="Dot Plot", 
       subtitle="Sale Price vs Overall Quality", 
       caption="We can compare different value too") +  
  coord_flip() #i change the values in x and y axis because it is better for my analysis

#No.4
#Bar chart
#it shows the general zoning classification of the house in every neighborhood
g <- ggplot(data, aes(Neighborhood))
g + geom_bar(aes(fill= Overall.Cond), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1)) +
  labs(title="The house's condition in every Neighborhood", 
       subtitle="It counts the houses in every neighborhood per overall condition", 
       caption="We can create a lot of different bar chart")

#No.5
#the same bar chart but now it shows the number of house styles per neighborhood
g <- ggplot(data, aes(House.Style))
g + geom_bar(aes(fill= Garage.Type), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1)) +
  labs(title="Garage Type in House Style Bar Chart", 
       subtitle="Counts the garage type in every house style", 
       caption="We can create a lot of different bar chart")

#No.6
#scatterplot measures the how many bedrooms has a house and from what material is made this house
#all these in axis price and lot area 
ggplot(data = data, mapping = aes(x = Lot.Area, y = SalePrice, size = Bedroom.AbvGr, color = Foundation)) +
  geom_point(alpha=.3) + #here we can adjust the shading
  scale_size(range = c(.5, 6), name="Bedrooms") + #in this line we can adjust the size of the bubble
  labs(title = "Foundation and No.Bedrooms Above Ground", subtitle = "In price and lot area scale", x = "Lot Area", y = "Sale Price")

#No.7
#histogram
#it show the average price price by overall condition of the house
#fill with bathrooms number 
ggplot(data = data) + 
  geom_bar(mapping = aes(x=as.factor(Overall.Cond), y = SalePrice, fill = Full.Bath), stat="summary", fun.y = "mean") + 
  labs(title = "Average House Price by Overall Condition ", 
       subtitle = "Fill with the number of baths", 
       x="Overall Condition", y="Average sale price($)")

#No.8
#histogram 
#it shows the average price of dwelling type
ggplot(data = data) + 
  geom_bar(mapping = aes(x=as.factor(Overall.Cond), y = SalePrice), stat="summary", fun.y = "mean") + 
  labs(title = "average house price by type of dwelling", x="Type of Dwelling", y="Average sale price($)")