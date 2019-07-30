library(XML)
library(dplyr)
library(rvest)
library(tidyr)
library(reshape)
library(car)
library(leaps)
library(HH)


data = read.csv("/Users/daraladjevardian/Downloads/Complete_SEDS_update.csv")
data = filter(data, Year == 2016 & Data)

variables = c("ESTCB", "FFTCB", "SOTCB", "GETCB", "NGTCB", "WDTCB", "WYTCB","WSTCB", "GDPRX")
#Electrivity, Fossil Fuel, Solar Energy, Geothermal Energy, Natural Gas, Wind Energy, Waste, Wood
#Real GDP
#Consumption in Billions BTU
#GDP (Real Gross Domestic Product) in Millios chained (2009) dollars

abbrev = state.abb

finaldata = filter( data, StateCode %in% abbrev)
finaldata = filter( finaldata, MSN %in% variables)
finaldata = select(finaldata, MSN, StateCode, Data)

ConsumptionData = reshape(finaldata, idvar="StateCode", timevar="MSN", direction="wide")
names(ConsumptionData) = c("State", "Electricity", "Fossil Fuel", "GDP","Geothermal",
                           "Natural Gas", "Solar", "Wood", "Waste", "Wind")
ConsumptionData = ConsumptionData[,c(0:3, 5:10, 4)] 

ProjectData = read.csv("RegressionData.csv")[,c(2:11 )]
ProjectData[is.na(ProjectData)] = 0 

#Numeric and Graphical Summaries for Important Variables
#Verification of Model Assumptions
summary(ProjectData)
bosplot(ProjectData)


boxplot(ProjectData$Electricity, xlab = "Electricity", ylab = "Billion btu")
boxplot(ProjectData$Fossil.Fuel, xlab = "Fossil Fuel", ylab = "Billion btu")
boxplot(ProjectData$Geothermal, xlab = "Geothermal", ylab = "Billion btu")
boxplot(ProjectData$Natural.Gas, xlab = "Natural Gas", ylab = "Billion btu")
boxplot(ProjectData$Solar, xlab = "Solar", ylab = "Billion btu")
boxplot(ProjectData$Wood, xlab = "Wood", ylab = "Billion btu")
boxplot(ProjectData$Waste, xlab = "Waste", ylab = "Billion btu")
boxplot(ProjectData$Wind, xlab = "Wind", ylab = "Billion btu")
boxplot(ProjectData$GDP, xlab = "Real GDP", ylab = "Millions chained (2009) $")
#Each plot is skewed to the right with 3-4 outliers

head(ProjectData %>% arrange(desc(GDP)))
#Outliers are Texas, New York, and California
NoOutliers = (ProjectData %>% arrange(desc(GDP)))[c(-1,-2,-3),]
boxplot(NoOutliers$Electricity, xlab = "Electricity", ylab = "Billion btu")
#Indpendence can be assumed (State data is measured independently from one another)


plot( GDP ~., data = ProjectData[,-1])
plot( GDP ~., data = NoOutliers[,-1])


ProjectDatalog = sapply( ProjectData[,-1], function(x) log(x))
ProjectDatalog[is.infinite(ProjectDatalog)] = 0
plot( GDP ~., data = ProjectDatalog)
#Log Plots, except for wind, show better linearity!


#Press Enter to see progression of scatter plots
#Some strong, some weak, positive linear associations between GDP and predictor variables
outlierfit = lm( GDP ~., data = NoOutliers[,-1])
summary(outlierfit)

logfit = lm( GDP ~., data = data.frame(ProjectDatalog))
summary(logfit)
#Electricity, Solar, Waste, and Wind are statistically significant here

fit = lm( GDP ~., data = ProjectData[,-1])
summary(fit)
#From this linear regression without any further analysis, we see that Electricity, Fossil Fuels, Natural Gas, Solar consumptions are statistically significant
#While Geothermal, Wood, Waste, and Wind consumption do not seem to be statistically significant

anova(fit)



#Multiple Linear Regression

#Linearity Assumption: Scatter Plots, except for wind, satisfy the straight enough condition

plot(fit$fitted.values, fit$residuals,ylab="residuals", xlab="fitted y", main="Residual plot")
abline(0,0)


#log fit follows equal variance condition better, random scatter along horizontal band at zero
plot(logfit$fitted.values, logfit$residuals,ylab="residuals", xlab="fitted y", main="Residual plot")
abline(0,0)

qqnorm(fit$residuals)
qqline(fit$residuals)


#Normality Assumption
qqnorm(logfit$residuals)
qqline(logfit$residuals)
#So can we stick to using the log plot because these better fit the regression conditions?

anova(logfit)


#Indicator Variables and Interaction Terms
#None because categorical

#MultiCollinearity

vif(fit)
#vif > 5, multicollinearity with Electricity, Fossil Fuels, Geothermal, Natural Gas, Solar

vif(logfit)
#vif > 5, multicollinearity with Electricity, Fossil Fuels, Natural Gas

#What to do about these variables? Combine or remove

#Variable Selection

#All Subsets Regression
fitsubs = regsubsets(GDP ~., data = ProjectData[,-1])

logfitsubs  = regsubsets(GDP ~., data = data.frame(ProjectDatalog))


cbind(summary(fitsubs)$which, summary(fitsubs)$adjr2, summary(fitsubs)$bic, summary(fitsubs)$cp)


summaryHH(fitsubs)

summaryHH(logfitsubs)


#Adjusted R squared: largest value at model with Electricity, Fossil Fuels, Natural Gas, Solar, Wood
#Same with smallest CP
#8 regressors, p = 8
#Same with smallest BIC


#Backward Elimination
#Do we need to continue using log
backsel = step(fit, direction="backward")
summary(backsel)


#Forward Selection
null = lm(ProjectData$GDP ~ 1)
forwardsel = step(null, scope=list(lower=null, upper=fit), direction="forward")
#Not sure why this is not working

#Stepwise Selection
stepsel = step(null, scope=list(lower=null, upper=fit),direction="both")
#Also not working

#Nonetheless, from this seleciton procedure, it seems we maintain the following Model
# GDP ~ Electricity + Fossil Fuels + Natural Gas + Solar + Wood

#But the previous selection procedure, for the log fit, it is GDP ~ Electricity + Wind + Natural Gas + Solar + Waste
#which to pick and how?

summary(fit)
#Electricity, Natural Gas, and Solar have small p-values and are in all three models, so let's keep those for sure
#The question now is between wood, waste, wind, and fossil fuels
#Perhaps look at linearity, best to take out wind?





#Regression Diagnostics

newfit = lm( GDP ~ Electricity + Solar + Fossil.Fuel + Natural.Gas + Wood, data = ProjectData[,-1])
summary(newfit)

plot(GDP ~ Electricity + Solar + Fossil.Fuel + Natural.Gas + Wood, data = ProjectData[,-1])
#Strong Positive linear, wood is the exception
plot(newfit$fitted.values, newfit$residuals,ylab="residuals", xlab="fitted y", main="Residual plot")
abline(0,0)

qqnorm(newfit$residuals)
qqline(newfit$residuals)

#These are fine, log is better again

di = rstandard(newfit) # standardized 
ri = rstudent(newfit) # studentized residuals
plot(newfit$fitted.values, ri, main="Studentized residuals versus fitted values")
abline(0,0)
qqnorm(ri)
qqline(ri)
#The extremes are off and deviate from the line (remove outliers?)
#heavy-tailed distirbution
#Outliers make sense: Texas, CAlifornia, and New York are abnormally wealthier than other states

plot(newfit$fitted.values, di, main="Studentized residuals versus fitted values")
abline(0,0)
qqnorm(di)
qqline(di)


avPlots(newfit)
#No random scatter, all variables belong in mode;

hi = hatvalues(newfit)
ProjectData[which(hi > 3*mean(hi)),]
#High leverage formula for small-medium dataset
#Outliers are actually Texas, Louisiana, and California

plot(hi, type="h", main="Plot of Leverage")
abline(3*mean(hi),0, lty=2)

#Are these outliers influential?

x=dfbetas(newfit)

plot(abs(dfbetas(fit)[,1]), type="h")
abline(1,0)
#observation 5 and 9

plot(abs(dfbetas(fit)[,2]), type="h")
abline(1,0)
#Observation 5 and 34

plot(abs(dfbetas(fit)[,3]), type="h")
abline(1,0)
#Observation 5 and 34

plot(abs(dfbetas(fit)[,4]), type="h")
abline(1,0)
#Observation 4,5, and 34

plot(abs(dfbetas(fit)[,5]), type="h")
abline(1,0)
#Observation 9 and 34

plot(abs(dfbetas(fit)[,6]), type="h")
abline(1,0)
#Observation 4,5, and 34

plot(abs(dfbetas(fit)[,7]), type="h")
abline(1,0)
#Observation 5 and 34

plot(abs(dfbetas(fit)[,8]), type="h")
abline(1,0)
#Observation 9 and 33

ProjectData[c(4,5,9,33,34),]
#Of the observations with high leverage, California is influential
#Ask the professor then if it would be necessary to remove California from the model

dffits(newfit)
plot(abs(dffits(newfit)), type="h")
abline(1, 0, lty=2)
ProjectData[which(abs(dffits(newfit))>1),]
#California, Florida, LA, New YOrk
#in previous test, Florida and New YOrk were claimed influential


plot(cooks.distance(newfit), type="h")
abline(1, 0, lty=2)
#California and New York

summary(influence.measures(newfit))
#AZ, CA, FL, GA, LA, NY, TX

#Plot of all the regression diagnostics
#Plot of Residuals vs. Fitted
#Normal QQ Plot of Standardized Residuals
#a Scale-Location plot of sqrt(absolute value of standardizedresiduals) versus fitted values
#a plot of standardized residuals against leverages
plot(newfit)

outlierTest(newfit)
#California and New York

#QQ Plot for Studentized Residuals
qqPlot(newfit, main="QQ Plot")
#I think we should therefore remove New York and California

influencePlot(newfit, main="Influence Plot")

noInfluencers = ProjectData[c(-5,-34),]
SubFit = lm( GDP ~ Electricity + Solar + Fossil.Fuel + Natural.Gas + Wood, data = noInfluencers[,-1])


summary(SubFit)
#In this new fit without California and New York, solar consumption is no longer statistically significant

#ANOVA Tests

#One Way
#Are the means of predictors equal?
boxplot(noInfluencers[,-1])
#Fossil Fuel, Natural Gas, and GDP means differ a lot. Electricity a little. Rest are pretty eqyal

#Compare difference of means to variations in groups
#All assumptions satisfied as shown before
#Boxplots are skewed in the same direction
#How to make them more symmetric? AFter removing New York and California -> Ask Teacher

model = dplyr::select(noInfluencers, Electricity,  Fossil.Fuel, Natural.Gas, Wood, GDP) #Since SOlar is no longer signfiicant
#Ask professor about this
res.anova = aov(GDP ~ Electricity + Fossil.Fuel + Natural.Gas + Wood, data = model)
summary(res.anova)

#Model Assumptions
plot(res.anova$fitted.values, rstandard(res.anova))
abline(0,0)
qqnorm(rstandard(res.anova))
qqline(rstandard(res.anova))
#Satisfied

#Bonferroni Test
pairwise.t.test(GDP, Electricity, data = model, p.adj="bonf")
#Actually ANOVA is not really relatable since there are no categorical variables or groups within a category

TukeyHSD(res.anova)
#NO Factors, so yeah I don't think we have to do this. Ask professor alternative option

#No Factors
#So majority of our project is about variable selection, regression diagnostics, outliers and influencers
#This is what we should focus on




