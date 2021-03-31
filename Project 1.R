# --------------------------------------------------------------------------------------------------------------------------------
# ECO 321 Project 1 by Group 8
# Linear Regression with Multiple Regressors

# Members: Emirhan Akkaya (SBU ID#: 112768575)
#          Parv Joshi     (SBU ID#: 112169570)
#          Shail Shah     (SBU ID#: 112315115)

# Our Dependent Variable: Government Healthcare Spending (GHS)
# Our Independent Variables: (1) GDP per Capita (GDPpc) 
#                            (2) Military Expenditure (ME)
#                            (3) Government Savings (GS)
#                            (4) Central Government Debt (CGD)
# --------------------------------------------------------------------------------------------------------------------------------

# Setting Working Directory
setwd("~/Fall 2020/ECO 321/Project 1")

# Reading data file in CSV format
data = read.csv("Final Clean Data - Usable Form.csv", fileEncoding = "UTF-8-BOM", sep = ",", header = TRUE)
# Note: We used fileEncoding = "UTF-8-BOM" to convert our latest CSV format to a CSV format readable by R

# We removed the column for countries from our original dataset for analysis purpose. However, to track countries in order using their index, we created a new list of variables in same order of data in case we needed it
countries = c("Afghanistan", "Algeria", "Angola", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahrain", "Bangladesh", "Belarus", "Belgium", "Belize", "Benin", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Canada", "Chile", "Colombia", "Congo, Dem. Rep.", "Costa Rica", "Cote d Ivoire", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Dominican Republic", "Ecuador", "El Salvador", "Estonia", "Eswatini", "Finland", "France", "Germany", "Ghana", "Greece", "Guatemala", "Guinea", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Korea, Rep.", "Kuwait", "Kyrgyz Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Mali", "Malta", "Mauritania", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Paraguay", "Peru", "Poland", "Portugal", "Romania", "Russian Federation", "Rwanda", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovak Republic", "Slovenia", "South Africa", "Spain", "Sri Lanka", "Sudan", "Sweden", "Switzerland", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Turkey", "Uganda", "Ukraine", "United Kingdom", "United States", "Uruguay", "Vietnam", "Zambia", "Zimbabwe")

# Data Visualization and Summary Statistics

# Generating Summary Statistics
summary(data) # gives summary statistics for each of our variables

# Finding and analyzing the correlations of each variable with each other variable
cor(data) # Gives a correlation matrix with each variables as rows and columns

# Plotting histograms for each variable
hist(data$GHS, main = "Histogram for Government Healthcare Spending", xlab = "Government Healthcare Spending (GHS)", ylab = "Frequency of Countries" )
hist(data$GDPpc, main = "Histogram for GDP per capita", xlab = "GDP per capita (GDPpc)", ylab = "Frequency of Countries")
hist(data$ME, main = "Histogram for Military Expenditure", xlab = "Military Expenditure (ME)", ylab = "Frequency of Countries")
hist(data$GS, main = "Histogram for Government Savings", xlab = "Government Savings (GS)", ylab = "Frequency of Countries")
hist(data$CGD, main = "Histogram for Central Government Debt", xlab = "Central Government Debt (CGD)", ylab = "Frequency of Countries")

# Generating scatter plots for dependent variables versus each of our independent variables

# We used ggplot to plot our data instead of a regular plot function. This required using the package 'tidyverse.' If this is not installed in your R-studio, please un-comment the next line and run it
# install.packages("tidyverse")

library(tidyverse) # Calling the package in library

# Plotting the data. 
# Here, 'ggplot' creates a cartesian plane. 
# geom_point plots each point using a mapping of specified x and y, with the color of data points.
# geom_smooth creates a y ~ x regression line using the linear model (lm) method, with specified mapping. Green color is used to show the regression line.
ggplot(data = data) + geom_point(mapping = aes(y = GHS, x = GDPpc), color = "black") + geom_smooth(data = data, formula = y ~ x, method = lm, color = "green", aes(y = GHS, x = GDPpc))
ggplot(data = data) + geom_point(mapping = aes(y = GHS, x = ME), color = "blue") + geom_smooth(data = data, formula = y ~ x, method = lm, color = "green", aes(y = GHS, x = ME))
ggplot(data = data) + geom_point(mapping = aes(y = GHS, x = GS), color = "red") + geom_smooth(data = data, formula = y ~ x, method = lm, color = "green", aes(y = GHS, x = GS))
ggplot(data = data) + geom_point(mapping = aes(y = GHS, x = CGD), color = "purple") + geom_smooth(data = data, formula = y ~ x, method = lm, color = "green", aes(y = GHS, x = CGD))

# Data Interpretation
# We sorted data (inclusive of countries) so that we can form interpretations from them. 
# For example, if we wanted to know which is the country that has the most Government Savings (which we can see from the graph), we can find it using this sorted data.
# Corresponding points from our scatter plots helped in our interpretations.

GDPpc_data = data.frame(data$GDPpc, countries) # Creates a data frame with only GDPpc and the corresponding countries.
attach(GDPpc_data) # attaches data for easy calling of variable names
GDPpc_data_sorted = GDPpc_data[order(data.GDPpc),] # sorts in ascending order of GDPpc
View(GDPpc_data_sorted) # Shows us the sorted data frame
detach(GDPpc_data) # detaches data to avoid confusion when attaching new data frames

ME_data = data.frame(data$ME, countries) # Creates a data frame with only ME and the corresponding countries.
attach(ME_data) # attaches data for easy calling of variable names
ME_data_sorted = ME_data[order(data.ME),] # sorts in ascending order of ME
View(ME_data_sorted) # Shows us the sorted data frame
detach(ME_data) # detaches data to avoid confusion when attaching new data frames

GS_data = data.frame(data$GS, countries) # Creates a data frame with only GS and the corresponding countries.
attach(GS_data) # attaches data for easy calling of variable names
GS_data_sorted = GS_data[order(data.GS),] # sorts in ascending order of GS
View(GS_data_sorted) # Shows us the sorted data frame
detach(GDPpc_data) # detaches data to avoid confusion when attaching new data frames

CGD_data = data.frame(data$CGD, countries) # Creates a data frame with only CGD and the corresponding countries.
attach(CGD_data) # attaches data for easy calling of variable names
CGD_data_sorted = CGD_data[order(data.CGD),] # sorts in ascending order of CGD
View(CGD_data_sorted) # Shows us the sorted data frame
detach(CGD_data) # detaches data to avoid confusion when attaching new data frames

# Data Analysis

# This requires using the packages 'sandwich' and 'lmtest.' If this is not installed in your R-studio, please un-comment the next two lines and run it
# install.packages("sandwich")
# install.packages("lmtest")

library(sandwich) # Calling the package in library
library(lmtest) # Calling the package in library

# Trying regression using one explanatory variable (GDPpc)
single_homo_model = lm(GHS ~ GDPpc, data = data) # Creates linear model using OLS regression (assumes homoskedasticity)
single_model = coeftest(single_homo_model, vcov = vcovHC(single_homo_model, "HC1")) # Corrects the above model for heteroskedasticity robust standard errors
summary(single_homo_model) # Shows summary of the homoskedastic model
single_model # Shows the model statistics, corrected for heteroskedasticty

# Comparing model predictions with summary statistics for GHS 
data$predicted_single = predict(single_homo_model) # Predicts the variable's summary statistics using our model
summary(data$predicted_single) # Displays the prediction statistics using our model
summary(data$GHS) # Shows actual summary statistics for GHS variable. Used to compare with model predictions.


# Using all four independent variables for our regression to find a new model
homo_model = lm(GHS ~ GDPpc + ME + GS + CGD, data = data) # Creates new linear model using OLS regression (assumes homoskedasticity)
model = coeftest(homo_model, vcov = vcovHC(homo_model, "HC1")) # Corrects the above model for heteroskedasticity robust standard errors
summary(homo_model) # Shows summary of the new homoskedastic model
model # Shows the new model's statistics, corrected for heteroskedasticty

# Comparing new model's predictions with summary statistics for GHS 
data$predicted_multiple = predict(homo_model) # Predicts the variable's summary statistics using our new model
summary(data$predicted_multiple) # Displays the prediction statistics using our new model
summary(data$GHS) # Shows actual summary statistics for GHS variable. Used to compare with new model's predictions.

# --------------------------------------------------------------------------------------------------------------------------------