library(tidyverse)
library(BSDA) # Needed for Z-test
library(janitor)

energy_data <- readr::read_csv("./Data/fake_energy_data.csv")

airquality_data <- airquality |> 
  clean_names()

# There are seven steps to follow when you are using any statistical test:
#   
# 1. State the null hypothesis H0 and alternative hypothesis Ha
# 2. Decide the significance level, α
# 3. Compute the observed test statistics
# 4. Determine the critical value
# 5. Determine the p-value
# 6. Determine the confidence interval
# 7. Interpret your results


# Statistical tests -------------------------------------------------------

# Z-test = statistical test to determine whether two population means are different 
# when the variances are known and the sample size is large

# Here we are filtering the airquality_data dataset
# Then pulling out the values, pull takes the value
# Out from a tibble into a single vector rather than 
# a column in a tibble


temperature_may <- airquality_data %>% 
  filter(month == 5) %>% 
  pull(temp)

temperature_september <- airquality_data %>% 
  filter(month == 9) %>% 
  pull(temp)

# Step 1: Construct a two-tailed test where the null hypothesis and alternative hypothesis are shown below:
#   
# H0: Mean temperature in May is the same as the mean temperature in September
# 
# Ha: Mean temperature in May different from the mean temperature in September

# Step 2: Choose your significance level α=0.05 (typically it is 5% or 1%)

# Assigning significance level to be 0.05 

alpha <- 0.05

# Step 3: Run two-sided z-test test by running one function (in package BSDA) as shown below:

# First we specify the two groups we want to compare which is temperature in May and June
# Then we specify that we want to conduct a two sided z-test
# Then we specify the population standard deviations of 
# both groups 
# Then we specify mu which is 0 since under null hypothesis 
# our mu is 0 
# and standard deviation is 1 
# The significance level is assumed 5%
# hence the confidence interval is 95%


z_test <- BSDA::z.test(x=temperature_may,
                       y=temperature_september,
                       alternative = "two.sided", 
                       sigma.x = sd(temperature_may),
                       sigma.y=sd(temperature_september),
                       mu=0, conf.level = 0.95) 

# Display the results 

z_test

# Step 4: Critical value approach: Determine the critical value to compare with the observed test statistics.

# Find the critical value using quantile function of 
# normal distribution 
# Because it is two sided we need to divide 
# significance level by 2

z_critical <-qnorm(p=1−alpha/2)

# Display the critical value 

z_critical 

# A value of observed z statistics outside +/- this value means we can reject the
# null hypothesis at the 5% significance level. Otherwise, we cannot reject the 
# null hypothesis (non-rejection region)

# Constructing the Non-rejection region
# where any value above -1.96 and any value below +1.96
# is considered to be in the non-rejection region

non_rejection_region <- c(− z_critical, z_critical) 

non_rejection_region

# since z = -5.79 is outside the non-rejection region - we reject the null hypothesis

# Step 5: P-value approach

# Display the p-value 

z_test$p.value

# much less than 0.05 which suggests very strong evidence against the null hypothesis --> reject


# Exercise 1 --------------------------------------------------------------

# Q. Is there a statistical difference between average wind speed in June and September?


wind_june <- airquality_data %>% 
  filter(month == 6) %>% 
  pull(wind)

wind_september <- airquality_data %>% 
  filter(month == 9) %>% 
  pull(wind)

# Null hypothesis: there is no statistical difference
# Alt hypothesis: there is a difference


z_test_wind <- BSDA::z.test(x=wind_june,
                       y=wind_september,
                       alternative = "two.sided", 
                       sigma.x = sd(wind_june),
                       sigma.y=sd(wind_september),
                       mu=0, conf.level = 0.95) 

z_test_wind

# Critical value approach

z_critical_wind <-qnorm(p=1−alpha/2)

# Display the critical value 

z_critical_wind 

non_rejection_region <- c(− z_critical_wind, z_critical_wind) 

non_rejection_region

# z= 0.092761 is inside the non-rejection region so we cannot reject the null hypothesis

# p-value approach: p=0.9261, we cannot reject null hypothesis as we have no evidence that 
# the wind speed in June is statistically different to the wind speed in September


# T-Test ------------------------------------------------------------------

# Here we are filtering the airquality_data dataset
# Then pulling out the values, pull takes the value
# Out from a tibble into a single vector rather a column 
# in a tibble

temperature_june <- airquality_data %>% 
  filter(month == 6) %>% 
  pull(temp)

temperature_september <- airquality_data %>% 
  filter(month == 9) %>% 
  pull(temp)

# First, construct the hypothesis as follows:
#   
# H0: Mean temperature in June is the same as the mean temperature in September
# 
# Ha: Mean temperature in June is different from the mean temperature in September

# Run t-test
# By default a two sample independent t-test is assumed

t.test(temperature_june, temperature_september)

# Finding the critical value 
# Use quantile function for t distribution (qt)
# For two-sided test our probability vector p is 1- alpha/2 
# where alpha is significance level
# It is a two-sided test  that is why we divide it by 2
# Then we specify the degrees of freedom (df)


t_critical <- qt(p=1−alpha/2, df = 55.043)

# Constructing the Non-rejection region
non_rejection_region <- c(− t_critical, t_critical) 


non_rejection_region


# Exercise 2 --------------------------------------------------------------

# Q. Is the energy consumption in owner-occupied and privately rented statistically different?

owner_consumption <- energy_data %>% 
  filter(tenure == "Owner-occupied") %>% 
  pull(consumption)

rented_consumption <- energy_data %>% 
  filter(tenure == "Privately rented") %>% 
  pull(consumption)

t.test(owner_consumption, rented_consumption)

# Critical value approach:
t_critical <- qt(p=1−alpha/2, df = 564.93)
non_rejection_region <- c(− t_critical, t_critical) 

non_rejection_region

# t = -0.55269 is within the non-rejection region so we cannot reject the null hypothesis

# p-value approach: p = 0.5807 so we cannot reject the null hypothesis 
# (the consumption of the two groups is not statistically different)


# Chi-square independence test --------------------------------------------

# The Chi-square (χ2) independence test is an inferential method to determine 
# whether two variables are independent/unrelated to each other. 

# The null hypothesis is that the distribution of one variable is independent 
# of the distribution of another variable, and therefore not affected by its presence.

# Q. Are the number of bedrooms in a house and the number of adults in a house independent?

# 1. Derive a contingency table of 1 categorical variable vs the other: adults vs bedrooms

# Create in the format of the contingency table 
# using the table() function

contingency_table <- table(energy_data$bedrooms,
                           energy_data$adults)

# To Display the data

contingency_table

# Then we construct the hypothesis:
#   
# H0: No association between the number of bedrooms and adults living in the house
# 
# Ha: There is an association between the number of bedrooms and adults living in the house

# Perform the chisq with no corrections

chisq.test(contingency_table, correct = FALSE)

# Produces warning message. Why?
# The contingency table has some pretty small values in it. If we remove the 
# groups that are giving us problems, then we can see that our chi-squared test 
# works without any warnings.


# Chi-square test after making corrections --------------------------------

# First, we remove the groups that are giving us problems

filtered_energy <- energy_data %>% 
  filter(bedrooms %in% c("4","5","6"))%>% 
  filter(adults %in% c("3","4"))

# Display the results
filtered_energy

# Then, create in the format of the contingency table 
# using the table() function.

filtered_cntgncy_table <- table(filtered_energy$bedrooms,
                                filtered_energy$adults)

filtered_cntgncy_table

# Perform the chisq with no corrections

chisq.test(filtered_cntgncy_table, correct = FALSE)

# p = 0.4795, so we cannot reject the null hypothesis - there is no association between
# no. of adults and no. of bedrooms

# Finding the critical value 
# Use quantile function for chi-squared distribution (qchisq)
# It is one-sided test so we donot need to divide by 2
# Then we specify the degrees of freedom (df)

qchisq(p=1-alpha, df=2) 
