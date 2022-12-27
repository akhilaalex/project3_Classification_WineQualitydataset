#Installingckages("cluster",repos = "http://cran.us.r-project.org")

install.packages("factoextra",repos = "http://cran.us.r-project.org")
install.pa
#Loading libraries
library(ggplot2)
library(factoextra)
library(dplyr)
library(purrr)
library(tidyverse)
library(caret)
library(corrplot)
library(cluster)
library(stargazer)
library(tidyverse)
library(ISLR2)
library(stargazer)
library(leaps)

#Upload Data
winedata <- read.csv("C:\\Users\\akhil\\OneDrive\\Desktop\\conestogac\\multivariate_statistics\\project 2\\winequalityN.csv", stringsAsFactors = FALSE, header = TRUE)
summary(winedata)

#Change the values name
winedata$type <- ifelse(winedata$type == 'white', 1,0)
summary(winedata)
names(winedata)

#Replacing missing values with mean value

redwine <- winedata %>% select(fixedacidity,volatileacidity,citricacid,residualsugar,chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,sulphates,alcohol,quality) %>%
                         filter(winedata$type == 1)                                            # Duplicate data frame
redwine$fixedacidity[is.na(redwine$fixedacidity)] <- mean(redwine$fixedacidity, na.rm = TRUE)  # Replace NA in one column
redwine$volatileacidity[is.na(redwine$volatileacidity)] <- mean(redwine$volatileacidity, na.rm = TRUE)  # Replace NA in one column
redwine$citricacid[is.na(redwine$citricacid)] <- mean(redwine$citricacid, na.rm = TRUE)  # Replace NA in one column
redwine$residualsugar[is.na(redwine$residualsugar)] <- mean(redwine$residualsugar, na.rm = TRUE)  # Replace NA in one column
redwine$chlorides[is.na(redwine$chlorides)] <- mean(redwine$chlorides, na.rm = TRUE)  # Replace NA in one column
redwine$sulphates[is.na(redwine$sulphates)] <- mean(redwine$sulphates, na.rm = TRUE)  # Replace NA in one column
redwine$pH[is.na(redwine$pH)] <- mean(redwine$pH, na.rm = TRUE)  # Replace NA in one column
summary(redwine)

#Normalize
redwine$fixedacidity_normalized = scale(redwine$fixedacidity)
redwine$volatileacidity_normalized = scale(redwine$volatileacidity)
redwine$citricacid_normalized = scale(redwine$citricacid)
redwine$residualsugar_normalized = scale(redwine$residualsugar)
redwine$chlorides_normalized = scale(redwine$chlorides)
redwine$freesulfurdioxide_normalized = scale(redwine$freesulfurdioxide)
redwine$totalsulfurdioxide_normalized = scale(redwine$totalsulfurdioxide)
redwine$density_normalized = scale(redwine$density)
redwine$pH_normalized = scale(redwine$pH)
redwine$sulphates_normalized = scale(redwine$sulphates)
redwine$alcohol_normalized = scale(redwine$alcohol)
summary(redwine)

redwine$fixedacidity_normalized
winedata = redwine[c(12:23)]
summary(winedata)

#adjust plot margins
windows()
plot.new() 
# Relation
corrplot(cor(winedata))


#Plot the frequency of quality
ggplot(winedata,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

# Group into categories

winedata$quality <- factor(ifelse(winedata$quality >= 3 & winedata$quality < 6, 0, 1))
#redwine$quality <- factor(titanic$Survived, labels = c("Deceased", "Survived"))
summary(winedata)
names(winedata)

head(winedata)


#Subset selection

subsetmodel <- regsubsets(quality ~ ., data = winedata)
summary(subsetmodel)

#adjust plot margins
windows()
plot.new() 

#create scatterplot
plot(subsetmodel, scale = "adjr2") 


#Divide the data into train and test

sam <- sample(2, nrow(winedata), replace=TRUE, prob=c(0.8, 0.2))
trainData <- winedata[sam==1,]
trainData$quality <- factor(trainData$quality)
testData <- winedata[sam==2,]
testData$quality <- factor(testData$quality)


#:::::::::Knn

k_grid <- expand.grid(k = seq(1, 15))
knn_redwine_model <- train(
  as.factor(quality) ~ volatileacidity_normalized+residualsugar_normalized+freesulfurdioxide_normalized+totalsulfurdioxide_normalized+density_normalized+pH_normalized+sulphates_normalized+alcohol_normalized,
  data = trainData,
  method = "knn",
  preProcess = c("center","scale"),
  tuneGrid = k_grid,
  trControl = trainControl(method = "cv", number = 10)
)
knn_redwine_model


#Prediction
#Prediction using Knn
knn_prediction <- predict(knn_redwine_model, testData)
knn_prediction

testData$quality = as.factor(testData$quality)
testData$quality

#Checking the accuracy of the model


confusionMatrix(knn_prediction,as.factor(testData$quality))



##:::::Logistic model

# redwine_model <- glm(as.factor(quality) ~ . , data = trainData, family = "binomial")
# coef(redwine_model)
# exp(coef(redwine_model))
# summary(redwine_model)

logistic_redwine_model <- train(
  as.factor(quality) ~ .,
  data = trainData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
logistic_redwine_model

#PREDICTION
logistic_prediction <- predict(logistic_redwine_model, testData)
logistic_prediction
testData$quality
#CONFUSION MATRIX
confusionMatrix(logistic_prediction, testData$quality)

#LOGISTIC REGRESSION AFTER SUBSET SELECTION AND NORMALIZATION


logistic_redwine_model_1 <- train(
  as.factor(quality) ~ volatileacidity_normalized+residualsugar_normalized+freesulfurdioxide_normalized+totalsulfurdioxide_normalized+density_normalized+pH_normalized+sulphates_normalized+alcohol_normalized,
  data = trainData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
logistic_redwine_model_1

#PREDICTION
logistic_prediction_1 <- predict(logistic_redwine_model_1, testData)
logistic_prediction_1
testData$quality
#CONFUSION MATRIX
confusionMatrix(logistic_prediction_1, testData$quality)

