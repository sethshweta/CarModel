library(DataExplorer)
library(pastecs)
library(readxl)
library(ggplot2)
library(caTools)
library(tidyverse)
CarModel<-read_xlsx("C:/Users/User/Desktop/Outlook/MRA Analytics.xlsx",sheet=1)
str(CarModel)
dim(CarModel)
summary(CarModel)
stat.desc(CarModel)
head(CarModel,10)
#from graph between age and price we can say that with life of car it's prices decrease and brand new car has higher price when compared to old car
ggplot(CarModel,aes(Age,Price))+geom_point()
ggplot(CarModel,aes(Age,Price))+geom_smooth()

#from graph b/w age and kilometer we can say that with age kilometer also increases i.e. more age more km car has covered. 
ggplot(CarModel,aes(Age,KM))+geom_point()
ggplot(CarModel,aes(Age,KM))+geom_smooth()

#from graph b/w price and Km we can say that with price kilometer also increases.
ggplot(CarModel,aes(KM,Price))+geom_point()
ggplot(CarModel,aes(KM,Price))+geom_smooth()

#from graph b/w cc and Hp we can say that there is no relation as such .
ggplot(data=CarModel,aes(cc,HP))+geom_point()
ggplot(data=CarModel,aes(cc,HP))+geom_smooth()

# NORMALITY CHECK
boxplot(CarModel$Price)
shapiro.test(CarModel$Price)

mrmodel <- lm(Price ~ CarModel$Age+
                CarModel$KM+
                CarModel$HP+
                CarModel$cc+
                CarModel$Doors+
                CarModel$Quarterly_Tax+
                CarModel$Weight, data = CarModel)
summary(mrmodel)
summary(mrmodel)$coefficient
confint(mrmodel)
sigma(mrmodel)/mean(CarModel$Price)
#TEST AND TRAINING DATA
# divide data on the basis of ID no into two parts
set.seed(100)
?set.seed()
#randomly distributed data where 70 percent is true remaining is false
split1<-sample.split(CarModel$Id, SplitRatio = 0.70)
split1
#training dataset
data2train<-subset(CarModel, split1 == TRUE)
data2test<-subset(CarModel,split1==FALSE)
# Checking Dimensions for data2train
dim(data2train)   #70%
# Checking Dimensions for data2test
dim(data2test)    #30%

#MULTIPLE REGRESSION
regmod <- lm(Price ~ CarModel$Age+
                CarModel$KM+
                CarModel$HP+
                CarModel$cc+
                CarModel$Doors+
                CarModel$Quarterly_Tax+
                CarModel$Weight+
                CarModel$Model, data = data2train)
summary(regmod)

#H0
#this model is explaining 81 percent variation
#if value is more than p value then no effect and if less then (0.05 ) then affects

#Now trying to remove weight,quarterly Tax,doors from regression model and predicting accuracy and it remains same

regmo<-lm(data2train$Price~
          data2train$Age+
          data2train$KM+
          data2train$HP+
          data2train$cc,data=data2train)
summary(regmo)
#H0
#this model is explaining 81 percent variation
#if value is more than p value then no effect and if less then (0.05 ) then affects

# now there is problem using hit trial method
library(leaps)
regmod<-regsubsets(data2train$Price~
                        data2train$Age+
                        data2train$KM+
                        data2train$HP+
                        data2train$Doors+
                        data2train$Quarterly_Tax+
                        data2train$Weight+
                      data2train$cc,data=data2train,
                         nbest = 3)
regmod
summary(regmod)
#darker the color more significant value
plot(regmod,scale="r2")
#we wil adj r2
plot(regmod,scale="adjr2")
plot(regmod,scale="bic")

# AUTOCORRELATION
library(lmtest)
dwtest(regmodel)


