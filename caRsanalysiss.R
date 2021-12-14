data1<-cars
data1
attach(data1)
summary(data1)

#the minimum speed in the dataset cars is 4mph while the maximum is of 25 mph. 
#Half of the observations lie above and half of the observations lie below the speed of 15 mph
#average speed of the cars dataset is found to be 15.4 mph
#i.e., on an average the observations have the speed of 15.4 mph

#the minimum distance in the dataset cars is 2 ft while the maximum is 120 ft.
#half of the observations lie above and half of the observations lie below the distance of 36 ft
#average distance covered is 42.98 ft

cor(data1)

#speed and distance are found to be correlated with correlation coefficient of 0.8068
#which indicates a pretty strong relationship between the two

lm.fit1=lm(speed~dist,data=data1)
summary(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)

# fitting a linear model between speed and distance 
#(speed as the response variable while distance as the predictor variable)
#from the coefficients table we have the evidence of the relationship between the response and the predictor
#also, the multiple R-squared (0.6511) explains the 65% of the variance in the training dataset
#while the adjusted R-squared indicates that the fit will be able to explain 64.38% of the variance
# in the test data (while predicting)

# the residual vs the fitted plot of the linear fit shows a trend (reverse parabola)
# indicating the presence of non linear relationship between the response and the indicator

# the Normal Q-Q plot indicate that the residuals are normally distributed among all the observations of the dataset

#the Scale-Location plot shows a horizontal red solid line which indicates homoscadacity
#(the values have equal variance)

#the residuals vs leverage plot indicates that the 49th observation will affect the linear regression fit if removed from the model

lm.fit2=lm(speed~poly(dist,2),data=data1)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

#fitting a non linear model between speed and distance
# (OF DEGREE 2)
#the coefficients table of the polynomial fit of degree 2 clearly gives evidence of the 
#presence of relationship between distance and (distance)^2
# multiple R-squared explains 71% of the variations in the data (which is much better than the linear fit model with only 65% of the variances explained)
# Adjusted R-square increased for the polynomial model, but the increase is not much significant
#which indicates that (distance)^2 doesn't explain much of the variances except the ones already 
# done by the (distance) component 

#the residuals vs fitted plot of the regression fit shows no particular trend and hence provides us
# with the evidence that the training data contains non linear relationship between the response and the predictor

#the Normal Q-Q plot of the regression fit shows that the residuals are normally fit across all the data points of the data

#the Scale-Location plot of the regression fit shows homoscadacity in the dataset

#the Residual vs Leverage plot now shows that the regression fit with and without the 49th observation 
#will not have a huge impact as it is on the border of Cook's distance

lm.fit3=lm(speed~poly(dist,3),data=data1)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

# Polynomial regression for speed and distance (degree 3)
# the coefficient table of the regression model shows that the component (distance)^3 doesn't have much correlation with the response
# unlike distance and (distance)^2 components
# multiple R now explains 72% of the variations in the training dataset 
#but the Adjusted R- square shows insignificant increase, which may be due to overfitting of the training data 

#the residual vs fitted plot shows equally distributed points and absence of any trend which justifies the polynomial regression

#the Normal Q-Q plot shows that the residuals are normally distributed for the dataset

#the scale-location plot shows homoscadacity

#the residual vs leverage plot for cubic regression is similar to that of the quadratic regression
# as again 49th observation has an impact on the regression fit line

lm.predict1=predict(lm.fit1,data.frame(speed=c(5,10,15)), interval="confidence")
lm.predict1
plot(predict(lm.fit1),residuals(lm.fit1))

# confidence intervals for linear model

lm.predict2=predict(lm.fit2,data.frame(speed=c(5,10,15)), interval="confidence")
lm.predict2
plot(predict(lm.fit2),residuals(lm.fit2))

# confidence intervals for quadratic regression model

lm.predict3=predict(lm.fit3,data.frame(speed=c(5,10,15)), interval="confidence")
lm.predict3
plot(predict(lm.fit1),residuals(lm.fit1))

#confidence intervals for cubic regression model

library(boot)

#calculating the test error for the linear model

glm.fit=glm(speed~dist,data=data1)
cv.err=cv.glm(data1,glm.fit)
cv.err
cv.err$delta

#calculating test error for the three regression fits together

cv.error=rep(0,3)
for (i in 1:3){
  glm.fit=glm(speed~poly(dist,i), data=data1)
  cv.error[i]=cv.glm(data1,glm.fit)$delta[1]
}
cv.error

#the test error (by LOOCV method) for the quadratic fit is the least among the three, indicating it best represents the training data
#as well as capable of making predictions accurately

set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(speed~poly(dist,i),data=data1)
  cv.error.10[i]=cv.glm(data1,glm.fit,K=10)$delta[1]
}
round(cv.error.10)

#the test error (by K-level CV method, here, k=10) also shows that the quadratic regression model is the best fit to describe the model


#*******INTERPRETATION*******
#the quadratic regression model best describes the training data accurately and is capable 
#of making predictions with test error of approx. 9 (both by LOOCV and K-fold CV method)
#the model explains the variations well without overfitting the data