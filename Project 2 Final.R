rm(list=ls())

#Set Working Directory
setwd("C:/Users/Lenovo/Documents/LM/EdWisor/Projects/Project 2")

getwd()


#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')


install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

############Load Data######################################

Data_Day = read.csv("day.csv", header = T )


#Exploratory Data Analysis

class(Data_Day)
dim(Data_Day)
head(Data_Day)
names(Data_Day)
str(Data_Day)
summary(Data_Day)


#From the above observations

#Droping few columns
Data_Day = subset(Data_Day, select = -c(instant, dteday, casual, registered))

dim(Data_Day)
names(Data_Day)


#separate numeric and categorical variables

numeric_var = c('temp', 'atemp', 'hum', 'windspeed', 'cnt')

categorical_var = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')



######################## Missing Value analysis #############################

summary(is.na(Data_Day))
sum(is.na(Data_Day))

#there is no missing values 


###############Outlier Analysis ##################################

df = Data_Day
Data_Day = df

# BoxPlots - Distribution and Outlier Check


library(ggplot2)

for (i in 1:length(numeric_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numeric_var[i]), x = "cnt"), data = subset(Data_Day))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_var[i],x="count")+
           ggtitle(paste("Box plot of count for",numeric_var[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5, ncol=2)


# outliers found in windspeed and humidity variables.

 
#replacing outliers with NA

for(i in numeric_var){
  print(i)
  outlier = Data_Day[,i][Data_Day[,i] %in% boxplot.stats(Data_Day[,i])$out]
  print(length(outlier))
  Data_Day[,i][Data_Day[,i] %in% outlier] = NA
}

sum(is.na(Data_Day))


#Impute NA values with KNN

library(DMwR)
library(rpart)

Data_Day = knnImputation(Data_Day, k = 5)

sum(is.na(Data_Day))


#################### Data Understanding ###################


# Time to plot some graphs, so let's install few libraries

library(ggplot2)
library(scales)
library(psych)
library(gplots)


# Barplot with x axis as season and y axis as count

ggplot(Data_Day, aes(x = Data_Day$season, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Number of bikes rented with respect to season", x = "Seasons", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

#It is found that season 3, has the highest count of bikes and season 1 has lowest count of bikes


# Barplot with x axis as year and y axis as count

ggplot(Data_Day, aes(x = Data_Day$yr, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "red")+
  labs(title = "Number of bikes rented with respect to year", x = "yr", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

# It is found that Year 1 has the highest count while year 0 has lowest count.


# Barplot with x axis as weekday and y axis as count

ggplot(Data_Day, aes(x = Data_Day$weekday, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "navyblue")+
  labs(title = "Number of bikes rented with respect to days", x = "Days of the week", y = "count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))

#It is found that on day 5 there is highest count and on day 0 its lowest count of bikes rented


#Count with respect to temperature and humidity together

ggplot(Data_Day,aes(temp,cnt)) + 
  geom_point(aes(color=hum),alpha=0.5) +
  labs(title = "Bikes count vs temperature and humidity", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

#it is found that when normalized temperature is between 0.5 to 0.75 and humidity is between 0.50 to 0.75, count is high.


# Count with respect to windspeed and weather together

ggplot(Data_Day, aes(x = windspeed, y = cnt))+
  geom_point(aes(color= weathersit ), alpha=0.5) +
  labs(title = "Bikes count vs windspeed and weather", x = "Windspeed", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

# It is found that count is at peak, when windspeed is from 0.1 to 0.3 and weather is from 1.0 to 1.5.


# Count with respect to temperature and season together

ggplot(Data_Day, aes(x = temp, y = cnt))+
  geom_point(aes(color=season),alpha=0.5) +
  labs(title = "Bikes count vs temperature and season", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

# it is found that count is maximum when temperature is 0.50 to 0.75 & season 3 to season 4.



################Feature Selection ###########################################

df2 = Data_Day
Data_Day = df2

#Correlation Analysis and Anova test is done identify if variables can be reduced or notis perfo

# Correlation Analysis for numeric variable

library(corrgram)

corrgram(Data_Day[,numeric_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis between numeric variables")

#it is found that temperature and atemp are highly correlated with each other.


# Anova Test for categorical variables

for(i in categorical_var){
  print(i)
  Anova_test_result = summary(aov(formula = cnt~Data_Day[,i],Data_Day))
  print(Anova_test_result)
}

#it is found that holiday, weekday and workingday has p value > 0.05. null hypothesis accepted


# Dimension redusction , removing variables that ar not required

Data_Day = subset(Data_Day, select=-c(atemp,holiday,weekday,workingday))


############Feature Scaling ######################################


numeric_var = c("temp","hum","windspeed","cnt")
catergorical_var = c("season", "yr", "mnth", "weathersit")


# Skewness test

library(propagate)

for(i in numeric_var){
  print(i)
  skew = skewness(Data_Day[,i])
  print(skew)
}

#dataset is approximately symmetric. values are found ranging between -0.5 to +0.5.


# Identify range and check min max of the variables to check noramility

for(i in numeric_var){
  print(summary(Data_Day[,i]))
}

#dat is found as normalized, scaling not required


# visualizing normality check 

hist(Data_Day$temp, col="Navyblue", xlab="Temperature", ylab="Frequency",
     main="Temperature Distribution")

hist(Data_Day$hum, col="Blue", xlab="Humidity", ylab="Frequency",
     main="Humidity Distribution")

hist(Data_Day$windspeed,col="Dark green",xlab="Windspeed",ylab="Frequency",
     main="Windspeed Distribution")

# the distribution is approximately symmetric



####################MODELING ##########


library(DataCombine)
rmExcept("Data_Day")


df3 = Data_Day
Data_Day = df3

#Develop error metrics

#R Square 

Rsquare = function(y,y1){
  cor(y,y1)^2
}

#MAPE

MAPE = function(y,y1){
  mean(abs((y-y1)/y))*100
}



########Dummy creation #############

categorical_var = c("season","yr","mnth","weathersit")

library(dummies)

Data_Day = dummy.data.frame(Data_Day, categorical_var)

#Save Data for KFold CV
KFData = Data_Day


#divide data

set.seed(123)
train_index = sample(1:nrow(Data_Day),0.8*nrow(Data_Day))
train= Data_Day[train_index,]
test= Data_Day[-train_index,]


###############check multicollinearity ###########################

numeric_var = c("temp","hum","windspeed", "cnt")

numeric_var2 = Data_Day[,numeric_var]

library(usdm)

vifcor(numeric_var2, th = 0.7)

#No collinearity problem.


#############DECISION TREE ####################

library(rpart)

DTModel = rpart(cnt~., train, method = "anova" , minsplit=5)


# Predictions

DTTest = predict(DTModel, test[-25])

summary(DTModel)

#MAPE

DTMape_Test = MAPE(test[,25], DTTest)
DTMape_Test   #26.4225


#RSquare

DT_RSquare = Rsquare(test[,25], DTTest)
DT_RSquare  #0.7612102


#############RANDOM FOREST####################

library(randomForest)
set.seed(123)

RFModel = randomForest(cnt~., train, ntree = 500, importance = TRUE)

# Predictions

RFTest = predict(RFModel, test[-25])


# MAPE

RFMape_Test = MAPE(test[,25], RFTest)
RFMape_Test  #  19.32104

#RSquare

RF_RSquare = Rsquare(test[,25], RFTest)
RF_RSquare   # 0.8685008

#################LINEAR REGRESSION####################

LRModel = lm(cnt~., train)

summary(LRModel)


# Predictions on test

LRTest = predict(LRModel, test[-25])


#MAPE

LRMape_Test = MAPE(test[,25], LRTest)
LRMape_Test #  21.56792


#RSquare

LR_RSquare = Rsquare(test[,25], LRTest)
LR_RSquare  #  0.8191175


###########################Model Selection & Evaluation ##########################

print("MAPE Statistics")
print(DTMape_Test)
print(RFMape_Test)
print(LRMape_Test)

print("Accuracy")
print(100 - DTMape_Test)
print(100 - RFMape_Test)
print(100 - LRMape_Test)


print("R Square Statistics")
print(DT_RSquare)
print(RF_RSquare)
print(LR_RSquare)


#######################Cross Validation ###########################

#Load Data
library(caret)

KFData 

#divide data

set.seed(123)
train_index2 = sample(1:nrow(KFData),0.8*nrow(KFData))
train_KF = KFData[train_index,]
test_KF = KFData[-train_index,]


#Random Forest Cross Validation

RF_KF = train(cnt~.,
              data = train_KF,
              method = "rf",
              tuneGrid = expand.grid(mtry = c(2,3,4)),
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE,))


print(RF_KF)


knitr::kable(head(RF_KF$results), digits = 3)

print(RF_KF$bestTune)



RFpreds = predict(RF_KF, test_KF[-25])

RFpreds_MAPE = MAPE(test_KF[,25], RFpreds)
RFpreds_MAPE

RFPreds_RSquare = Rsquare(test[,25], RFpreds)
RFPreds_RSquare





#Decision Tree Cross Validation


DT_KF = train(cnt~.,
                 data = train_KF,
                 method = "gbm",
                 tuneGrid = expand.grid(n.trees = 200, 
                                        interaction.depth = c(1,2,3), 
                                        shrinkage = 0.1,
                                        n.minobsinnode = 10 ),
                 trControl = trainControl(method = "cv",
                                          number = 5,
                                          verboseIter = FALSE))
              
print(DT_KF)


knitr::kable(head(DT_KF$results), digits = 3)

print(DT_KF$bestTune)


DTpreds = predict(DT_KF, test_KF[-25])

DTpreds_MAPE = MAPE(test_KF[,25], DTpreds)
DTpreds_MAPE

DTPreds_RSquare = Rsquare(test[,25], DTpreds)
DTPreds_RSquare





#Linear Regression CV


LR_KF = train(cnt~.,
              data = train_KF,
              method = "lm",
              tuneGrid = expand.grid(intercept = TRUE),
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       verboseIter = FALSE))

print(LR_KF)

knitr::kable(head(LR_KF$results), digits = 3)

print(LR_KF$bestTune)

LRpreds = predict(LR_KF, test_KF[-25])

LRpreds_MAPE = MAPE(test_KF[,25], LRpreds)
LRpreds_MAPE

LRPreds_RSquare = Rsquare(test[,25], LRpreds)
LRPreds_RSquare





