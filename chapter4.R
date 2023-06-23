# Libraries needed

library(tidyverse) # For data manipulation

# Importing datasets

energy_data <- readr::read_csv("./Data/energy_data.csv")

airquality_data <- readr::read_csv("./Data/airquality_data.csv")


# Formula and formulae in R -----------------------------------------------

# A formula for variables x and a saved as object c
# The tilde ~ generally characterises formulae (of the class formula) written in R. 
# It means “capture the meaning of the code here without immediately evaluating it”. 
c <- x ~ a

# class of c?
class(c)

# Pose question of relationship between consumption, tenure, region and property age
# as a formula

consumption ~ tenure + region + property_age


# ANOVA -------------------------------------------------------------------

# We will use one-way ANOVA on energy_data, where we want to understand whether 
# gas consumption significantly varies across known values of property_type. 

# Remove the NA in the property_type 

property_type_data <- energy_data %>%
  filter(property_type != "NA") %>% 
  select(property_type, consumption) 


# We want to see the counts of each group
property_type_data %>% group_by(property_type) %>% count()

#  The assumptions for one-way ANOVA is as follows:

# Random samples
# Independent samples
# The data of each factor level are normally distributed.
# The standard deviations of the variable under consideration are the same for all the groups

# Already know are random and independent samples - need to check the other 2 assumptions
# 1. Normality assumption
# Normal probability (Q-Q) plot of property type variable by each group
# stat_qq produces Q-Q plot

qqplot_property_type <- ggplot2::ggplot(property_type_data) +
  aes(sample = consumption,
      colour = factor(property_type)) +
  stat_qq() 

# Display the graph

qqplot_property_type

# We can also add the slope lines to see if the data points follow a straight line.
# stat_line() produces the slope

qqplot_property_type + stat_qq_line()

# the majority of observations for each group falls on a straight line suggesting a normal distribution.

# 2. Equal std. deviations across different groups

# Grouped tibble by property_type
groupby_property_type <- dplyr::group_by(property_type_data,
                                         property_type)

# Calculating the standard deviation of gas consumption 
# by group dataset

standard_consumption <- dplyr::summarise(groupby_property_type,
                                         sd_consump=sd(consumption,na.rm = TRUE))

#Display the results

standard_consumption 

# How to quantify if std. devs are sufficiently similar? Divide max sd with min
# If result < 2, it is safe to assume constant std dev across groups

# Divide the maximum with a minimum standard deviation

max(standard_consumption$sd_consump)/min(standard_consumption$sd_consump)

# Conditions are met, now, we construct the hypothesis as follows:

# H0: The mean gas consumption is equal across different property types
# 
# Ha: At least one mean is different

# Running one-way anova test by using aov() function
# Consumption is the response/dependent variable
# so it will be before the ~
# property_type is the explanatory variable

one_way_anova <- aov(consumption~property_type, data=property_type_data) 

# Summary of the analysis
summary(one_way_anova)

# Next, we determine the critical values

# Decide significance level
alpha <- 0.05

# Use qf() to find the quantile function 
# of F distribution used for ANOVA


# Since F-test is one tailed we only specify 
# 1-alph and donot divide by 2 
# Then specify the first degrees of freedom in table
# i.e. degrees of freedom of groups (between-group variability)
# Then specify the second degrees of freedom in table 
# i.e. degrees of freedom of errors (within-group variability)

qf(p= 1-alpha, df1= 6, df2 =  1069)

# Crit value approach: F=15.44 is greater than critical values (2.10) -> enough evidence to reject null hypothesis
# p-value approach: p<0.05 --> there is a significant difference between consumption based on property type


# Exercise ANOVA ----------------------------------------------------------

# Use one-way ANOVA on energy_data, to understand whether gas consumption 
# significantly varies across different floor area sizes.

floor_area_subset <- energy_data |> 
  select(consumption, floor_area) 

# check normality assumption

qqplot_floor_area <- ggplot2::ggplot(floor_area_subset) +
  aes(sample = consumption,
      colour = factor(floor_area)) +
  stat_qq() 

# Display the graph

qqplot_floor_area

# We can also add the slope lines to see if the data points follow a straight line.
# stat_line() produces the slope

qqplot_floor_area + stat_qq_line()

# check std. dev equality assumption

# Grouped tibble by property_type
groupby_floor_area <- dplyr::group_by(floor_area_subset,
                                         floor_area)

# Calculating the standard deviation of gas consumption 
# by group dataset

standard_floor_consumption <- dplyr::summarise(groupby_floor_area,
                                         sd_consump=sd(consumption,na.rm = TRUE))

#Display the results

standard_floor_consumption 

# Divide the maximum with a minimum standard deviation

max(standard_floor_consumption$sd_consump)/min(standard_floor_consumption$sd_consump)
# 1.227 - less than 2, so assumption is ok

# H0: The mean gas consumption is equal across different floor area sizes
# 
# Ha: At least one mean is different

# Running one-way anova test by using aov() function
# Consumption is the response/dependent variable
# so it will be before the ~
# property_type is the explanatory variable

one_way_anova_floor <- aov(consumption~floor_area, data=floor_area_subset) 

# Summary of the analysis
summary(one_way_anova_floor)

# Next, we determine the critical values

# Decide significance level
alpha <- 0.05

# Use qf() to find the quantile function 
# of F distribution used for ANOVA


# Since F-test is one tailed we only specify 
# 1-alph and donot divide by 2 
# Then specify the first degrees of freedom in table
# i.e. degrees of freedom of groups (between-group variability)
# Then specify the second degrees of freedom in table 
# i.e. degrees of freedom of errors (within-group variability)

qf(p= 1-alpha, df1= 6, df2 =  1069)

# p = 1.74e-05 << 0.05 so we reject the null hypothesis.
# Gas consumption does vary significantly across different floor area sizes


# Pairwise t-test ---------------------------------------------------------

# Constructing multiple hypotheses can cause problems since testing more means we 
# will be more likely to reject the null hypothesis incorrectly. Therefore we need
# to use Bonferroni correction to adjuct inflating p-values. #

# List other methods that we can use
p.adjust.methods

# Conduct pairwise t-test
# Gives matrix of bonferroni adjusted t-tests for pairs 
pairwise.t.test(x=property_type_data$consumption, 
                g=property_type_data$property_type, 
                p.adjust.method="bonferroni")

# Since this procedure is a bit harder to conduct and interpret we will use the 
# Tukey’s HSD (honest significant difference) test.

# The Tukey Multiple-Comparison Method, also known as Tukey’s HSD 
# (honest significant difference) test compares the means of every group to the 
# means of every other group and obtains the confidence interval for each.
# If the confidence interval for a pairwise comparison does not include 0 then we reject the null hypothesis.

# Tukey's test
tukey <- TukeyHSD(one_way_anova)


# Plotting the graph makes it easier to interpret results
# par sets or adjusts plotting parameters
# mar set margin sizes mar(bottom,left,top,right)
# las indicates the orientation of the tick mark labels
# 0 is parallel to axis, 1 is horizontal, perpendicular is 2,
# vertical is 3

par(mar=c(5,13,3,3))
plot(tukey, col = "brown",las=1)

# Exercise
# Use Tukey’s HSD test to identify which group means of floor_area are different in energy_data.

# Tukey's test
tukey_floor <- TukeyHSD(one_way_anova_floor)


# Plotting the graph makes it easier to interpret results
# par sets or adjusts plotting parameters
# mar set margin sizes mar(bottom,left,top,right)
# las indicates the orientation of the tick mark labels
# 0 is parallel to axis, 1 is horizontal, perpendicular is 2,
# vertical is 3

par(mar=c(5,13,3,3))
plot(tukey_floor, col = "brown",las=1)

# Only 2 groups are significantly statistically different: 51 to 100 and 50 or less
# and over 250 and 50 or less


# Linear regression -------------------------------------------------------

# Specify the x and y axes
# Specify the default geom_point()
# Specify the titles, x and y labels

ggplot2::ggplot(data = airquality_data) +
  aes(x = wind, y = ozone) +
  geom_point() +
  labs(x="Wind (miles per hour)", 
       y ="Ozone parts (per billion)") +
  ggtitle("Wind and Ozone from May till September") +  
  theme(plot.title = element_text(hjust = 0.5)) 

# Building linear regression in R
# First specify your formula then the dataset

simple_model <- lm(formula=ozone ~ wind, data=airquality_data)

# Get a summary of the model
summary(simple_model)

# reminder: if R-squared = -0.282 this means that the linear model explains 28.2%
# of the variance

# Create a variable where the wind speeds are 7 mph and 8mph

wind_speed <- c(7,8)


# Compute ozone concentration when wind speed is 7 mph and 8mph

85.2-4.32*wind_speed

# Exercise
# Fit a linear regression model on the airquality_data where the response 
# variable is ozone concentration and the explanatory variable is the temperature.

tempozone_model <- lm(formula = ozone~temp, data = airquality_data)

summary(tempozone_model)

# p < 2.2e-16 so temperature is associated with changes in ozone.
# Roughly 37% of the variance in ozone is explained by the changes temperature
# The ozone concentration is -101.6 parts per billion when the temperature is 0 which doesn't make physical sense
# One unit change in temperature is associated with a 1.85 increase in ozone parts per billion

# Create some new data

new_wind <- data.frame(wind = c(23, 26, 30))

# Use the predict function on new values
# And also get confidence intervals for these
predict(simple_model, newdata = new_wind, interval = "confidence")

new_temp <- data.frame(temp = c(48, 52, 55))
predict(tempozone_model, newdata = new_temp, interval = "confidence")
# All values negative which is unphysical


# Multiple linear regression ----------------------------------------------

# First specify your formula then the dataset
multiple_model <- lm(formula=ozone ~ wind + temp, data=airquality_data)

# Get a summary of the model
summary(multiple_model)

# Exercise
# Fit a multiple linear regression model with explanatory variables wind, temp and solar_r.
multiple_model_solar <- lm(formula=ozone ~ wind + temp +solar_r, data=airquality_data)
summary(multiple_model_solar)

# Interaction effects

# Often need to centre explanatory variables. Here's a rough guide of when to do this:

# 1. Interpreting the intercept: If the intercept term needs to be interpreted, 
# and any explanatory variable does not have a meaningful 0 value (such as weight 
# or height) then the explanatory should be centred.

# 2. Interactions: If you are testing an interaction between a continuous 
# variable and another variable (continuous or categorical) the continuous 
# variables should be centred to avoid multicollinearity issues, which could 
# affect model convergence and/or inflate the standard errors.

# 3. Polynomial terms: If you are transforming a variable (e.g. X2), the 
# transformed variable may be highly correlated with the untransformed variable (X). 
# For the same reason as the interaction term, centre the untransformed variable (X) 
# after the transformation.

# ONLY CENTRE THE CONTINUOUS EXPLANATORY VARIABLES, NOT CATEGORICAL OR RESPONSE VARIABLES
# Centre the explanatory variables
# by using scale() function

# center= True means centering variables and not scaling

centre_temp <- scale(airquality_data$temp,center = TRUE, scale = FALSE)
centre_wind <- scale(airquality_data$wind,center = TRUE, scale = FALSE)

# Add an interaction term between centred wind and centred temp

interaction_model <- lm(formula=ozone ~ centre_wind + centre_temp +
                          centre_wind:centre_temp, data=airquality_data)


# Or only use * between two variables - a*b is equivalent to a + b + a:b
interaction_model <- lm(formula=ozone ~ centre_wind*centre_temp,
                        data=airquality_data)

# Get a summary of the model
summary(interaction_model)

# Exercise
# Create a multiple regression model of the response variable ozone and the 
# explanatory variables temp and solar_r, and include interaction between both explanatory variables.

centre_solar <- scale(airquality_data$solar_r,center = TRUE, scale = FALSE)

interaction_solar <- lm(formula=ozone ~ centre_temp*centre_solar,
                        data=airquality_data)
summary(interaction_solar)
