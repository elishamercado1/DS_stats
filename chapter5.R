# Libraries needed

library(tidyverse) # For data manipulation 
library(car)       # For finding multicollinearity in a regression model 
library(leaps)     # For model selection 

# Importing datasets 

energy_data <- readr::read_csv("./Data/energy_data.csv")

airquality_data <- readr::read_csv("./Data/airquality_data.csv")


# Regression assumptions --------------------------------------------------

# Change the panel layout to 2 x 2
# Look at all 4 plots at once 

par(mfrow = c(2, 2))

# Use plot() function to create diagnostic plots

simple_model <- lm(formula=ozone ~ wind, data=airquality_data)

plot(simple_model)

# Residuals vs fitted: check linearity assumption
# Normal Q-Q: check normality of residuals assumption
# Scale location: check homoscedasticity of residuals
# Residuals vs leverage: identify any influential value in dataset

# Check linearity
# Create the first diagnostic plot 
plot(simple_model,1)
# residuals are non linear - should use non-linear transformations of explanatory variable in model eg logX, srtX etc

# Check normality of residuals
# Create the second diagnostic plot 
plot(simple_model,2)
# Can assume normality, but endpoints are deviated from straight line, suggesting 
# a heavy-tailed distribution (distribution is longer and tails are fatter so there
# may be outliers)

# check homoscedasticity
# Create the third diagnostic plot 
plot(simple_model,3)
# Heteroscedastic - red line not horizontal and residuals for a funnel shape
# larger end toward fitted values
# USING A LOG OR SQRT TRANSFORMATION ON RESPONSE VARIABLE REDUCES HETEROSCEDASTICITY

# Influential values
# Outliers: Observations whose standardised residuals are greater than 3 in 
# absolute value are considered as potential outliers
# Leverage points: A measure of how extreme an observation is on the explanatory variable.
# High leverage point is when leverage > 2(p+1)/n, where p = no. explanatory variables, 
# n = total no. observations.
# Influential point: A data point is influential if it unduly influences any
# part of a regression analysis, such as the predicted responses or the estimated slope coefficients. 

# Create the 5th diagnostic plot 
plot(simple_model,5)

# One extreme point with standardised residual above 3 -> suggests 117 is potential outlier
# A few high leverage points since some are higher than 4/153 = 0.026
# None outside the grey dashed line --> no influential point

# Create the 4th diagostic plot
# Which gives cook's distance
plot(simple_model,4)
# By default, the top 3 most extreme values are labelled on the Cook’s distance plot.

# Exercise
# 1. Fit a multiple linear regression on airquality_data where the response variable 
# is ozone and all explanatory variables are added.

full_model <- lm(formula=ozone ~ ., data=airquality_data)
summary(full_model)
plot(full_model)

# 2. Check assumptions

# Check linearity
# Create the first diagnostic plot 
plot(full_model,1)
# Residuals are non-linear - this assumption does not hold

# Check normality of residuals
# Create the second diagnostic plot 
plot(full_model,2)
# Mostly holds, but deviates at end (may be outliers)

# check homoscedasticity
# Create the third diagnostic plot 
plot(full_model,3)
# Red line not flat, 'dips' in the middle -> heteroscedastic

# Create the 5th diagnostic plot 
plot(full_model,5)
# 62 and 117 are possible outliers
# 3 high leverage points > 12/153 = 0.08
# No influential points outside of grey dashed line

# Create the 4th diagnostic plot to get Cook's distance
plot(full_model,4)
# 30, 62 and 117 are most extreme values


# Model transformation ----------------------------------------------------

# Create a scatter plot
# Specify the x and y axes
# Specify the default geom_point()
# Specify the titles, x and y labels
# Add the regression line
# Centrally align the title text

ggplot2::ggplot(airquality_data) +
  aes(x=wind, y=ozone) +
  geom_point() +
  labs(x="Wind (miles per hours)", 
       y = "Ozone concentration (per billions)") +
  ggtitle("Ozone Concentration and Wind Speed from May till September") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Not really linear, and as above does not meet all assumptions for linear regression
# Try a stabilising transformation (log(X))

# Apply algorithm transformation on the response variable
# wind is the explanatory variable
log_model <- lm(formula=log(ozone) ~ wind, data=airquality_data)

summary(log_model)

# Exercise
# Apply the logarithmic transformation on the multiple linear regression containing 
# all explanatory variables from airquality_data.

full_log <- lm(formula=log(ozone) ~ ., data = airquality_data)
summary(full_log)
par(mfrow = c(2, 2))
plot(full_log)


# Model Comparisons -------------------------------------------------------

# Certain metrics like R-squared, RMSE, RSE and mean absolute error (MAE) are
# sensitive to the inclusion of additional variables. So, we need a more robust 
# metric to guide model choice such as:
# Adjusted R-squared: which adjusts the R2 for having too many variables in the model.
# AIC: It stands for Akaike’s Information Criteria. The basic idea of AIC is to 
# penalize the inclusion of additional variables to a model. It adds a penalty 
# that increases the error when including additional terms. The lower the AIC, 
# the better the model.
# It stands for Bayesian Information Criteria. It is a variant of AIC with a stronger 
# penalty for including additional variables to the model.


# Now - compare two MLR models:
# Multiple linear regression
# the response variable is ozone 
# explanatory variables are temp and solar_r

model_one <- lm(formula=ozone ~ temp + solar_r, data=airquality_data)

# Multiple linear regression
# the response variable is ozone 
# all explanatory variable 

model_two <- lm(formula=ozone ~ ., data=airquality_data)

# Use 'glance' from broom package to compare models
# computes the R2, adjusted R2, sigma (RSE), AIC, BIC 
# of model one and two

broom::glance(model_one)
broom::glance(model_two)

# NOTE: ADDING EXPLANATORY VARIABLES GENERALLY DECREASES AIC AND BIC SO IT IS
# BETTER TO USE DELTA AIC OR DELTA BIC. 
# Always use multiple metrics to justify final model choice

model_a <- lm(formula=ozone ~ solar_r + month, data=airquality_data)
model_b <- lm(formula=ozone ~ ., data=airquality_data)
broom::glance(model_a)
broom::glance(model_b)


# Multicollinearity -------------------------------------------------------

# Collinearity = two + variables correlate with each other
# Multicollinearity: 3+ variables even if no particular pair have high correlation
# Colinearity can be detected using a correlation matrix, but multicollinearity needs
# variance inflation factor (VIF). 1 = no multicollinearity >5 problem, >10 big problem

# Use the variance inflation factor from the car package
# multiple_exercise is the multiple linear regression model 
# which contains all explanatory variables 
# that was created in the first section exercise
car::vif(full_model)


# Model selection ---------------------------------------------------------
# Stepwise
# Backward
full_model <- lm(formula=ozone~., data = airquality_data)

backward_selection <- step(full_model, direction = "backward")

# ISSUE: NOT EVERY POSSIBLE MODEL IS CHECKED SO THEY MAY MISS THE BEST MODEL
# If you have a large number of explanatory variables this is often necessary
# because to check every poss model would be time consuming, however, as long as
# dataset isn't too large you can use the leaps package to check all...

# Best subset linear model using leaps::regsubsets()
# method = "forward", "backward", "seqrep" (both)
# nvmax = max number of explanatory variables
# Gives best model of all sizes eg. 1-variable, 2-variable etc.

# Use the regsubsets() function to get the best model
# Specify the method to be seqrep (sequential replacement)
# Set nvmax to be 5 since we have 5 explanatory variables

best_model <- leaps::regsubsets(ozone~., data = airquality_data, 
                                nvmax = 5, method = "seqrep")

# summary() reports the best set of variables for each model size

summarise_model <- summary(best_model)

summarise_model

# Find which model has the maximum Adjusted R2
# and minimum residual sum of square RSS
# and minimum BIC

data.frame(Adj_R2 = which.max(summarise_model$adjr2),
           RSS = which.min(summarise_model$rss),
           BIC = which.min(summarise_model$bic))

# Exercise
# 1. Fit the best subset linear model using regsubsets() function and set the 
# method to be backward and nvmax=3.

best_model_exercise <- leaps::regsubsets(ozone~., data = airquality_data, 
                                nvmax = 3, method = "backward")

summarise_exercise <- summary(best_model_exercise)
summarise_exercise

# Find the best model using the metrics such as Adjusted R2 , RSS (residual sum of squares) and BIC.   
data.frame(Adj_R2 = which.max(summarise_exercise$adjr2),
           RSS = which.min(summarise_exercise$rss),
           BIC = which.min(summarise_exercise$bic))
