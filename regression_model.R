#loading data 
Life_data_Exp<-read.csv("Life Expectancy Data.csv")
#translating data as a data frame
Life_data_Exp <- as.data.frame(Life_data_Exp)
#checking the tye of data
class(Life_data_Exp)
#checking the type of variables
lapply(Life_data_Exp,class)
#storing all columns that are numeric to a variable
Num_Life <- unlist(lapply(Life_data_Exp,class)) == "numeric"
Num_Life
#checking correlation between the variables
cor(Life_data_Exp[,Num_Life])
#loading tidyverse library
library(tidyverse)
#omitting NA
Life_data_Exp <- Life_data_Exp <- Life_data_Exp %>% 
  drop_na()
#checking dataframe
str(Life_data_Exp)
summary(Life_data_Exp)
#range of Life.expectancy
range(Life_data_Exp$Life.expectancy)
#correlation between life expectancy and adult mortality
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Adult.Mortality)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$infant.deaths)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Alcohol)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Schooling)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Population)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Hepatitis.B)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Polio)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$Diphtheria)
cor(Life_data_Exp$Life.expectancy,Life_data_Exp$percentage.expenditure)
#plotting correlation 
plot(Life_data_Exp[,Num_Life])
plot(Life_data_Exp$Life.expectancy,Life_data_Exp$Adult.Mortality)
plot(Life_data_Exp$Life.expectancy, Life_data_Exp$Schooling)
plot(Life_data_Exp$Life.expectancy, Life_data_Exp$infant.deaths)
plot(Life_data_Exp$Life.expectancy, Life_data_Exp$Income.composition.of.resources)
plot(Life_data_Exp$Life.expectancy, Life_data_Exp$Alcohol)
plot(Life_data_Exp$Life.expectancy, Life_data_Exp$percentage.expenditure)
#regression of Life expectancy on Income composition of resources
fit_life <- lm(Life.expectancy ~ Income.composition.of.resources , data=Life_data_Exp)
fit_life
#getting summary statistics
life_sum<-summary(fit_life)
life_sum$coefficients
pvalue<- life_sum$coefficients[,4]
#comparing p values 
pvalue<=0.05
# R^2 of fit_life
fitr2<-life_sum$r.squared
fitr2
#calculating AIC of fit_life
fitaic <- AIC(fit_life)
fitaic
#regress life expectancy on Schooling
fit2 <- lm(Life.expectancy ~ Schooling,data=Life_data_Exp) 
sum2 <- summary(fit2)
sum2
fit2r2 <- sum2$r.squared
fit2aic <- AIC(fit2)
fit2aic
#testing coefficients
sum2$coefficients[,4] <= 0.05
# getting coefficient details
sum2$coefficients
#compare the models using R^2
r2 <- c(fitr2,fit2r2)
r2
round(r2,2)
names(r2) <- c("Income.composition.of.resources","Schooling")
round(r2,3)
#compare the models using AIC
aic <- c(fitaic,fit2aic)
names(aic) <- names(r2)
round(aic,3)
#regress life expectency on country
fit3 <- lm(Life.expectancy~Country,data=Life_data_Exp) 
sum3 <- summary(fit3) # Get summary statistics
sum3
fit3r2 <- sum3$r.squared
fit3aic <- AIC(fit3)
#Model with intercept
lm(Life.expectancy ~ Income.composition.of.resources, data=Life_data_Exp)
#model without intercept
lm(Life.expectancy ~ 0 +Income.composition.of.resources , data=Life_data_Exp)
#multiple regression with Income composition of resources and schooling
fit4 <- lm(Life.expectancy~Income.composition.of.resources+Schooling,data= Life_data_Exp )
sum4 <- summary(fit4)
sum4
#Create a variable yy that includes the first 10 values of Life expectancy
yy <- Life_data_Exp[,colnames(Life_data_Exp)=="Life.expectancy"]
yy <- yy[1:10]
#matrix to draw random values
xx <- matrix(runif(90),ncol=9)
ftemp <- list()
#looping all regressions
for (i in 1:9){
  ftemp[[i]] <- lm(yy ~xx[,1:i])
}
#getting r squared from all models
r2temp <- unlist(lapply(ftemp,function(Life_data_Exp){summary(Life_data_Exp)$r.squared}))
plot(1:9,r2temp,xlab="Number of random inputs",ylab="R-squared")
sapply(ftemp,AIC)
#calculating errors
yy - ftemp[[9]]$fitted.values
#fitting with a constant
ftemp[[10]] <- lm(yy~1)
sapply(ftemp,AIC)
#checking r squares
unlist(lapply(ftemp,function(x){summary(x)$r.squared}))
#full model on life expectancy
fit5 <- lm(Life.expectancy~.,data=Life_data_Exp)
summary(fit5)
#model with only an intercept
fitmin <- lm(Life.expectancy~1,data=Life_data_Exp)
#defining the full model
fit6 <- step(fitmin,direction="both",scope=formula(fit5))
#the final model
summary(fit6)
#producing forward regression
fit7 <- step(fitmin,direction="forward",scope=formula(fit5))
summary(fit7)
fit8 <- step(fit5,direction="backward",scope=formula(fit5))
#final backward model
summary(fit8)
AIC(fitmin)
#comparing models
aic <- c(AIC(fit_life),AIC(fit2),AIC(fit5),AIC(fit6))
names(aic) <- c(formula(fit_life),formula(fit2),"Full model","Stepwise")
round(aic,4)
plot(fit6)
#getting all 4 plots
par(mfrow=c(2,2)) 
plot(fit6)
par(mfrow=c(1,1))
resid <- fit6$residuals
fitted <- fit6$fitted.values
par(mfrow=c(2,2))
plot(fitted,resid) # plot fitted vs. residuals
plot(Life_data_Exp$Income.composition.of.resources,resid) #  plot income composition of resources vs. residuals
plot(Life_data_Exp$Schooling,resid) # plot schooling vs. residuals
hist(resid,100)
par(mfrow=c(1,1))
#getting sample to define a test set
idx <- sort(sample(1:nrow(Life_data_Exp),100))
xTest <- Life_data_Exp[idx,]
xLife_exp <- Life_data_Exp[-idx,]
#building a simple regression
fitLife_exp <- lm(Life.expectancy ~ Income.composition.of.resources + Schooling, data=xLife_exp)
fitLife_exp
#generating predictions for test set
predict(fitLife_exp,newdata=xTest)
predict(fitLife_exp)
fitLife_exp$fitted.values
