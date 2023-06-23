# Libraries needed

library(tidyverse) # For data manipulation 
library(visdat)    # For missing values   
library(naniar)    # For missing values
library(moments)   # For skewness
library(GGally)    # For correlation plots


# Inspecting the data -----------------------------------------------------

raw_energy_data <- readr::read_csv("./Data/fake_energy_data.csv")

head(raw_energy_data, n = 6)
tail(raw_energy_data, n = 6)
glimpse(raw_energy_data)


# Finding missing values --------------------------------------------------

# check if there are any NAs in our dataset 

anyNA(x=raw_energy_data)

# Visualise the missing value 
# x can be a dataframe

visdat::vis_dat(x=raw_energy_data)

# colSums() function gives the number of missing value in each columns
# is.na() function which checks if the data has missing values

colSums(is.na(x=raw_energy_data))

# Select only categorical variables by using if_select() function in dplyr

cat_raw_energy_data <- raw_energy_data %>% 
  dplyr::select_if(is.character) 


# See unique values by each column 

sapply(cat_raw_energy_data, function(x) unique(x))

# Convert "Unknowns" into NA

raw_energy_data[raw_energy_data == "Unknown"] <- NA
raw_energy_data[raw_energy_data == "unknown"] <- NA

# This will show the sum of missing values in all columns

colSums(is.na(x=raw_energy_data))

# Impute NAs in income with the mean:

# Specify your dataset and then use mutate() function
# Using if_else specify the new column name and conditions
# na.rm omits missing value while calculating mean 

raw_energy_data <- raw_energy_data %>%
  dplyr::mutate(income =
                  dplyr::if_else(condition = is.na(income),     
                                 true = mean(income, na.rm = TRUE), 
                                 false = income))

# check if there are any NAs in income column after mean imputation
anyNA(x=raw_energy_data$income)


# Univariate exploratory data analysis ------------------------------------

# Get the summary statistics of consumption variable

summary(raw_energy_data$consumption)

# Calculating skewness using moments package 

moments::skewness(x=raw_energy_data$consumption, na.rm = TRUE)

# If skewness is less than −1 or greater than +1, the distribution is highly skewed.
# 
# If skewness is between −1 and -0.5 or between +0.5 and +1, the distribution is moderately skewed.
# 
# If skewness is between −0.5 and +0.5, the distribution is approximately symmetric.

# Get the summary statistics of income variable
summary(raw_energy_data$income)

# Skewness using moments package

moments::skewness(x=raw_energy_data$income, na.rm = TRUE)

# Plotting continuous variables -------------------------------------------

# Select thedata
# Create a histogram
# Specify the variable to display in aes(), aesthetic attributes are mapped to variables
# Specify the labels
# Specify the title
# Centre the title

ggplot2::ggplot(raw_energy_data) +   
  aes(x=income) + 
  geom_histogram(bins=15) +
  labs(x="Income (£)", y ="Count") +
  ggtitle("Histogram of Income") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Create a density plot - we just use geom_density instead of geom_histogram

ggplot2::ggplot(raw_energy_data) +
  aes(x = income) +
  geom_density() +
  labs(x="Income (£)", y ="Density") +
  ggtitle("Density Plot of Income") +
  theme(plot.title = element_text(hjust = 0.5))

# Maximum is an outlier (and possible mistake) - replace with mean

# Specify your dataset and then use mutate function
# Using if_else specify the new column name and conditions
# Replace the maximum value of income with mean of income

raw_energy_data <- raw_energy_data %>%
  dplyr::mutate(income =
                  dplyr::if_else(condition = income==max(income),
                                 true = mean(income), 
                                 false = income))

# Find the summary statistics of the income variable

summary(raw_energy_data$income)

# Plot the histogram again 

income_hist <- ggplot2::ggplot(raw_energy_data) + 
  aes(x=income) +                        
  geom_histogram(color="black", fill="white",bins=25) +
  ggtitle("Histogram of Income") +
  labs(x="Income", y ="Count") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Add a mean line in the histogram 

income_hist + geom_vline(aes(xintercept=mean(income)),
                         color="blue", linetype="dashed", size=1)


# Exercise ----------------------------------------------------------------
summary(raw_energy_data$consumption)

# Min value is negative (-434) which doesn't make sense. 
# Replace with mean.

raw_energy_data <- raw_energy_data %>%
  dplyr::mutate(consumption =
                  dplyr::if_else(condition = consumption==min(consumption),
                                 true = mean(consumption), 
                                 false = consumption))

# Check summary stats again
summary(raw_energy_data$consumption)

# Plot histogram

consumption_hist <- ggplot2::ggplot(raw_energy_data) +  
  aes(x=consumption) + 
  geom_histogram(bins=20) +
  labs(x="Consumption (kWh)", y ="Count") +
  ggtitle("Histogram of Consumption") +
  theme(plot.title = element_text(hjust = 0.5)) 


consumption_hist + geom_vline(aes(xintercept=mean(consumption)),
                         color="blue", linetype="dashed", size=1)

# Scaling continuous variables --------------------------------------------
# Create a boxplot
# Add whiskers(whiskers are the two lines outside the box 
# that extend to the highest and lowest observations)

ggplot2::ggplot(raw_energy_data) +
  aes(x = income) +
  geom_boxplot() +
  labs(x="Income") +
  ggtitle("Boxplot of Income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_boxplot(geom = 'errorbar',width = 0.2)

# Can also check for outliers by standardising variables of interest 
# (transforming variable to have a mean of 0 and st dev of 1)
# We standardise the variable and then consider any standard value (z-score) 
# above 3 or below -3 to be an outlier. 

# Scale the value variable to give standard value
standard_value <- scale(raw_energy_data$income)

# Check for values greater than 3
standard_value[standard_value > 3]

# check for scores smaller than -3
standard_value[standard_value < -3]

# Create a new column using mutate()
# standardised income variable as shown above 
# Keep all observation with a standard value less than 3 and greater than -3 using filter()

energy_data <- raw_energy_data %>%
  mutate(scaled_income = scale(income)) %>%
  filter(scaled_income < 3) %>% 
  filter(scaled_income > -3) 

# Our dataset is reduced to 1126 observations

nrow(energy_data)

# See the difference between scaled and unscaled data 
# Using sample() function 
# Sample of 10 observation of unscaled income

sample(energy_data$income, 10)

# Sample of 10 observation of scaled income

sample(energy_data$scaled_income, 10)


# Categorical variables ---------------------------------------------------

# Counting six values with dplyr
# This operation gets us the count for each level in descending order

energy_data %>% count(property_type) %>% arrange(desc(n))

# Exercise 
# Create a frequency table of the tenure variable in the cleaned energy_data.

energy_data %>% count(tenure) %>% arrange(desc(n))

# Creating a bar chart 
# With angled axis labels

ggplot2::ggplot(data = energy_data) + 
  aes(x = property_type) +
  geom_bar() + 
  labs(x="Type of Property", y ="Count") +             
  ggtitle("Frequency of Different Property Type") +                    
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Exercise
# Create a bar chart of the property_age variable in energy_data

ggplot2::ggplot(data = energy_data) + 
  aes(x = property_age) +
  geom_bar() + 
  labs(x="Age of Property", y ="Count") +             
  ggtitle("Frequency of Different Property Ages") +                    
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Multivariate exploratory data analysis ----------------------------------

# Clean column names

airquality_data <- janitor::clean_names(airquality)


# Structure of airquality_data

dplyr::glimpse(airquality_data)

# Unique values in the variable month in airquality_data

unique(airquality_data$month)

# Using the categorical data as factor using factor() when plotting graphs

airquality_data$month <- factor(x=airquality_data$month)

# Convert all variables from integers to numeric

airquality_data <- airquality_data     %>% 
  mutate(solar_r=as.numeric(solar_r)) %>% 
  mutate(ozone=as.numeric(ozone))     %>% 
  mutate(temp=as.numeric(temp))  

# The variables ozone, temp and solar_r are numeric now

dplyr::glimpse(airquality_data)

# Find the summary statistics of airquality_data 

summary(airquality_data)

# Finding skewness of ozone using moments package

moments::skewness(x=airquality_data$ozone, na.rm = TRUE)

# There are 37 missing values in the ozone variable and 7 missing values in the 
# solar_r variable

# Creating scatterplot 
# Specify x and y axes
# geom_miss_point() has shifted the missing values to now be 10% below the minimum value
# facet_wrap() is used when your variable has several levels
# legend.position specifies the position of the legend

ggplot2::ggplot(data = airquality_data) +
  aes(x = ozone, y = solar_r) + 
  geom_miss_point() + 
  facet_wrap(~month, ncol = 2) + 
  theme(legend.position = "bottom")

# Impute missing values in solar_r using mean imputation

airquality_data <- airquality_data %>%
  dplyr::mutate(solar_r=
                  dplyr::if_else(condition=is.na(solar_r),     
                                 true = mean(solar_r, na.rm = TRUE), 
                                 false=solar_r))

# Impute missing values in ozone using mean imputation

airquality_data <- airquality_data %>%
  dplyr::mutate(ozone=
                  dplyr::if_else(condition=is.na(ozone),     
                                 true = mean(ozone, na.rm = TRUE), 
                                 false=ozone))

# However mean imputation can cause variability in the data and it is not the 
# best practice. Refer to Editing and Imputation to gain a better insight of possible imputations.


# Plotting two continuous variables ---------------------------------------

# Create a scatter plot
# Specify the x and y axes
# Make the colour conditional on a variable month
# Remember to make month variable as a factor variable
# Specify the default geom_point()
# Specify the titles, x and y labels

ggplot2::ggplot(data = airquality_data) +
  aes(x =solar_r, y = wind, colour = month) +
  geom_point() +
  labs(x="Solar Radiations (Langleys)", 
       y ="Wind (miles per hours)" ) +
  ggtitle("Wind Speed and Solar Radiations from May till September") +     
  theme(plot.title = element_text(hjust = 0.5)) 

# Exercise

ggplot2::ggplot(data = airquality_data) +
  aes(x =temp, y = wind, colour = month) +
  geom_point() +
  labs(x="Temperature (F)", 
       y ="Wind (miles per hour)" ) +
  ggtitle("Wind Speed and Temperature from May till September") +     
  theme(plot.title = element_text(hjust = 0.5)) 

# There appears to be a negative linear relationship between wind speed and temperature
# (higher wind speed tends to correlate to lower temperatures)

# Correlation and covariance

# Correlation coefficient of variables temp and wind
# Default method is pearson

cor(x=airquality_data$temp,y=airquality_data$wind)

# Scatterplot matrices are a convenient way to check collinearity

# Creating a scatterplot matrix with GGally

scatterplot_matrix <- GGally::ggpairs(data=airquality_data)

# Displaying the results 

scatterplot_matrix
  
# We will look at the relationship between month and ozone 
# by taking out the specific plot we are interested in 
# To look at a specific plot use getPlot function in GGally package

GGally::getPlot(pm= scatterplot_matrix, i=1, j=5)

# Scatterplot matrix using base R
base_r_matrix <- pairs(airquality_data)

# This is not such a good option because just makes scatterplots, even for 
# categorical data. Also GGally gives correlation in the equivalent upper right of
# matrix (ggpairs() shows the shape of distribution, correlation coefficients and boxplots as well)

# Categorical variables

# Tile plots are useful way to visualise these

# Order your floor_area variable and turn it into a factor
# levels specifies the order 

energy_data$floor_area <- factor(energy_data$floor_area,
                                 levels = c("50 or less", "51 to 100",
                                            "101 to 150", "151 to 200",
                                            "201 to 250","Over 250"))

# Create a tile plot

energy_data %>%
  count(property_type, floor_area) %>%
  ggplot2::ggplot() +
  aes(x = property_type,
      y = floor_area, fill = n) +
  geom_tile() +
  labs(x="Type of Property", 
       y = expression("Floor Area (m"^2*")")) +
  scale_fill_gradient(low="white", high="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Making a grouped distribution visualisation
# Use filter to filter out only observations for England and Wales
# Then use ggplot to create a density plot 
# Of gas consumption for each country

dplyr::filter(.data=energy_data, 
              country == "England"|country == "Wales") %>%
  ggplot2::ggplot() +
  aes(x=consumption, fill=country) +
  geom_density(alpha=0.4)

# Exercise
# Remove any missing values from the property_type variable in energy_data and assign it to a new variable.

# Take energy_data 
# use filter() to filter out missing values
# represented as NA
# select the variables you want to create 
# boxplot of 

filtered_property <- energy_data %>%
  filter(property_type != "NA") %>% 
  select(property_type, consumption) 

# Use the function geom_boxplot to see the spread of data of gas consumption by different type of properties.
filtered_property |> 
  ggplot() +
  aes(x = property_type, y = consumption) +
  geom_boxplot()
