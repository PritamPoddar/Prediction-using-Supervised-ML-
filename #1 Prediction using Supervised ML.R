## Q) Predict the percentage of an student based on the no. of study hours?
## Hours(2.5, 5.1, 3.2, 8.5, 3.5, 1.5, 9.2, 5.5, 8.3, 2.7, 7.7, 5.9, 4.5, 3.3, 1.1, 8.9, 2.5, 1.9, 6.1, 7.4, 2.7, 4.8, 3.8, 6.9, 7.8)
## Scores(21, 47,27, 75, 30, 20, 88, 60, 81, 25, 85, 62, 41, 42, 17, 95, 30, 24, 67, 69, 30, 54, 35, 76, 86)
## Q) What will be predicted score if a student studies for 9.25 hrs/ day? 

library(MASS)
library(caTools)
getwd()

### --- Fetching Data
MyData <- read.csv(file.choose())
View(MyData)
summary(MyData)

### --- Plot
plot(MyData$Hours,MyData$Scores, xlab ="Study Hours", ylab ="Score of Students",
     main ="Study Hours vs Score of Students", col="Red", pch=20 )
cor(MyData$Hours,MyData$Scores)

### --- Linear Model
LmModel <- lm(Scores ~ Hours, data = MyData)
LmModel

### --- Fitted Line
abline(LmModel, lwd=2, col = 'blue')
summary(LmModel)

### --- Predict
Predict_1 <- predict(LmModel)
Predict_1

### --- Residuals
Residuals_1 <- residuals(LmModel)
Residuals_1

### --- Plot B/W Predict & Residuals 
plot(Predict_1, Residuals_1, abline(0,0) )

### --- Predict Vs Actual
plot(MyData$Scores,xlab ="Index", ylab ="Scores", main ="Studies Hours per Day",col= "Red", type = "l", lwd = 2)

lines(Predict_1, col="blue",type="l", lwd=2)

### --- Predicted Score For a Particular Time
Predict_Hour <- data.frame(Hours=9.25)
Predict_Hours_9.25 <- predict(LmModel, Predict_Hour)
print(Predict_Hours_9.25)

