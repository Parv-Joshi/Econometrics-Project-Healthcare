# --------------------------------------------------------------------------------------------------------------------------------
# ECO 321 Project 2 by Group 8
# Linear Regression with Multiple Regressors using Non-Linear Elements

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
setwd("~/Fall 2020/ECO 321/Project 2")

# Reading data file in CSV format
data = read.csv("Final Clean Data - Usable Form.csv", fileEncoding = "UTF-8-BOM", sep = ",", header = TRUE)
# Note: We used fileEncoding = "UTF-8-BOM" to convert our latest CSV format to a CSV format readable by R

# install.packages(c("AER","sandwich","lmtest","car"))
library(AER)
library(sandwich)
library(lmtest)
library(car)

# Data Visualization
# ------------------

# Plotting histograms for each variable
hist(data$GHS, main = "Histogram for Government Healthcare Spending", xlab = "Government Healthcare Spending (GHS)", ylab = "Frequency of Countries" )
hist(data$GDPpc, main = "Histogram for GDP per capita", xlab = "GDP per capita (GDPpc)", ylab = "Frequency of Countries")
hist(data$ME, main = "Histogram for Military Expenditure", xlab = "Military Expenditure (ME)", ylab = "Frequency of Countries")
hist(data$GS, main = "Histogram for Government Savings", xlab = "Government Savings (GS)", ylab = "Frequency of Countries")
hist(data$CGD, main = "Histogram for Central Government Debt", xlab = "Central Government Debt (CGD)", ylab = "Frequency of Countries")

# Plotting histograms for log of each variable
hist(log(data$GHS), main = "Histogram for Log of Government Healthcare Spending", xlab = "Log of Government Healthcare Spending (GHS)", ylab = "Frequency of Countries" )
hist(log(data$GDPpc), main = "Histogram for Log of GDP per capita", xlab = "Log of GDP per capita (GDPpc)", ylab = "Frequency of Countries")
hist(log(data$ME), main = "Histogram for Log of Military Expenditure", xlab = "Log of Military Expenditure (ME)", ylab = "Frequency of Countries")
hist(log(data$GS), main = "Histogram for Log of Government Savings", xlab = "Log of Government Savings (GS)", ylab = "Frequency of Countries")
hist(log(data$CGD), main = "Histogram for Log of Central Government Debt", xlab = "Log of Central Government Debt (CGD)", ylab = "Frequency of Countries")

"We notice that Log(GHS) becomes approximately normal. This could be useful foe our non-linear transformations."

# Generating Transformations (Graphing and Regressions)
# -----------------------------------------------------

# For GDP per capita versus Government Healthcare Spending
y = data$GHS
x = data$GDPpc

plot(x, y, cex = 0.5, xlab="GDP per capita", ylab="Government Healthcare Spending") # Creating the graphing plane

linear = lm(y ~ x, data = data) # Creating linear regression model (assuming homoskedasticity)
hetero_linear = coeftest(linear, vcov = vcovHC(linear, "HC1")) # Correcting for heteroskedasticity
abline(linear,col=7,lwd=2) # Plotting linear regression line

squared = lm(y ~ x + I(x^2), data = data) # Creating quadratic regression model (assuming homoskedasticity)
hetero_squared = coeftest(squared, vcov = vcovHC(squared, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(squared) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=6,lwd=2) # Plotting quadratic regression line

cubed = lm(y ~ x + I(x^2) + I(x^3), data = data) # Creating cubic regression model (assuming homoskedasticity)
hetero_cubed = coeftest(cubed, vcov = vcovHC(cubed, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(cubed) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=5,lwd=2) # Plotting cubic regression line

legend("topleft",c("Cubic","Quadratic","Linear"),lty=c(1,1,1),col=5:7,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="GDP per capita", ylab="Government Healthcare Spending") # Creating the graphing plane
linear_log = lm(y ~ I(log(x)), data = data) # Creating linear-log regression model (assuming homoskedasticity)
hetero_linear_log = coeftest(linear_log, vcov = vcovHC(linear_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(linear_log)) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=4,lwd=2) # Plotting linear-log regression line
legend("topleft",c("Linear-Log"),lty=c(1,1,1),col=4,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="GDP per capita", ylab="Government Healthcare Spending") # Creating the graphing plane
log_linear = lm(I(log(y)) ~ x, data = data) # Creating log-linear regression model (assuming homoskedasticity)
hetero_log_linear = coeftest(log_linear, vcov = vcovHC(log_linear, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_linear)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=3,lwd=2) # Plotting log-linear regression line
legend("topleft",c("Log-Linear"),lty=c(1,1,1),col=3,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="GDP per capita", ylab="Government Healthcare Spending") # Creating the graphing plane
log_log = lm(I(log(y)) ~ I(log(x)), data = data) # Creating log-log regression model (assuming homoskedasticity)
hetero_log_log = coeftest(log_log, vcov = vcovHC(log_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_log)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=2,lwd=2) # Plotting log-log regression line
legend("topleft",c("Log-Log"),lty=c(1,1,1),col=2,lwd=c(2,2,2),bty="n")

# TABLES:

# For Linear:
summary(linear) # Summary of linear model (assuming homoskedasticity)
hetero_linear # Estimates Standard Errors, and p-values for linear model, corrected for heteroskedasticity

# For Squared:
summary(squared) # Summary of quadratic model (assuming homoskedasticity)
hetero_squared # Estimates Standard Errors, and p-values for quadratic model, corrected for heteroskedasticity

# For Cubed:
summary(cubed) # Summary of cubic model (assuming homoskedasticity)
hetero_cubed # Estimates Standard Errors, and p-values for cubic model, corrected for heteroskedasticity

# For Linear-Log:
summary(linear_log) # Summary of linear-log model (assuming homoskedasticity)
hetero_linear_log # Estimates Standard Errors, and p-values for linear-log model, corrected for heteroskedasticity

# For Log-Linear:
summary(log_linear) # Summary of log-linear model (assuming homoskedasticity)
hetero_log_linear # Estimates Standard Errors, and p-values for log-linear model, corrected for heteroskedasticity

# For Log-Log:
summary(log_log) # Summary of log-log model (assuming homoskedasticity)
hetero_log_log # Estimates Standard Errors, and p-values for log-log model, corrected for heteroskedasticity

# FINDING BEST TRANSFORMATION :

waldtest(squared, c("I(x^2)"), vcov = vcovHC(squared, "HC1")) # Comparing Linear and Quadratic transformations
"Since p-value is 0.3158 > 0.05 (our chosen level of significance), linear model is better than quadratic model"
waldtest(cubed, c("I(x^3)","I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Cubic transformations
"Since p-value is 0.00375 < 0.05 (our chosen level of significance), cubic model is better than linear model"



# For Military Expenditure versus Government Healthcare Spending
y = data$GHS
x = data$ME

plot(x, y, cex = 0.5, xlab="Military Expenditure", ylab="Government Healthcare Spending")# Creating the graphing plane

linear = lm(y ~ x, data = data) # Creating linear regression model (assuming homoskedasticity)
hetero_linear = coeftest(linear, vcov = vcovHC(linear, "HC1")) # Correcting for heteroskedasticity
abline(linear,col=7,lwd=2) # Plotting linear regression line

squared = lm(y ~ x + I(x^2), data = data) # Creating quadratic regression model (assuming homoskedasticity)
hetero_squared = coeftest(squared, vcov = vcovHC(squared, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(squared) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=6,lwd=2) # Plotting quadratic regression line

cubed = lm(y ~ x + I(x^2) + I(x^3), data = data) # Creating cubic regression model (assuming homoskedasticity)
hetero_cubed = coeftest(cubed, vcov = vcovHC(cubed, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(cubed) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=5,lwd=2) # Plotting cubic regression line

legend("topleft",c("Cubic","Quadratic","Linear"),lty=c(1,1,1),col=5:7,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Military Expenditure", ylab="Government Healthcare Spending") # Creating the graphing plane
linear_log = lm(y ~ I(log(x)), data = data) # Creating linear-log regression model (assuming homoskedasticity)
hetero_linear_log = coeftest(linear_log, vcov = vcovHC(linear_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(linear_log)) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=4,lwd=2) # Plotting linear-log regression line
legend("topleft",c("Linear-Log"),lty=c(1,1,1),col=4,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Military Expenditure", ylab="Government Healthcare Spending") # Creating the graphing plane
log_linear = lm(I(log(y)) ~ x, data = data) # Creating log-linear regression model (assuming homoskedasticity)
hetero_log_linear = coeftest(log_linear, vcov = vcovHC(log_linear, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_linear)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=3,lwd=2) # Plotting log-linear regression line
legend("topleft",c("Log-Linear"),lty=c(1,1,1),col=3,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Military Expenditure", ylab="Government Healthcare Spending") # Creating the graphing plane
log_log = lm(I(log(y)) ~ I(log(x)), data = data) # Creating log-log regression model (assuming homoskedasticity)
hetero_log_log = coeftest(log_log, vcov = vcovHC(log_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_log)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=2,lwd=2) # Plotting log-log regression line
legend("topleft",c("Log-Log"),lty=c(1,1,1),col=2,lwd=c(2,2,2),bty="n")

# TABLES:

# For Linear:
summary(linear) # Summary of linear model (assuming homoskedasticity)
hetero_linear # Estimates Standard Errors, and p-values for linear model, corrected for heteroskedasticity

# For Squared:
summary(squared) # Summary of quadratic model (assuming homoskedasticity)
hetero_squared # Estimates Standard Errors, and p-values for quadratic model, corrected for heteroskedasticity

# For Cubed:
summary(cubed) # Summary of cubic model (assuming homoskedasticity)
hetero_cubed # Estimates Standard Errors, and p-values for cubic model, corrected for heteroskedasticity

# For Linear-Log:
summary(linear_log) # Summary of linear-log model (assuming homoskedasticity)
hetero_linear_log # Estimates Standard Errors, and p-values for linear-log model, corrected for heteroskedasticity

# For Log-Linear:
summary(log_linear) # Summary of log-linear model (assuming homoskedasticity)
hetero_log_linear # Estimates Standard Errors, and p-values for log-linear model, corrected for heteroskedasticity

# For Log-Log:
summary(log_log) # Summary of log-log model (assuming homoskedasticity)
hetero_log_log # Estimates Standard Errors, and p-values for log-log model, corrected for heteroskedasticity

# FINDING BEST TRANSFORMATION :

waldtest(squared, c("I(x^2)"), vcov = vcovHC(squared, "HC1")) # Comparing Linear and Quadratic transformations
"Since p-value is  0.1221 > 0.05 (our chosen level of significance), linear model is better than quadratic model"
waldtest(cubed, c("I(x^3)","I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Cubic transformations
"Error: computationally singular system: reciprocal condition number = 1.74503e-25. This means that matrix is irrevertible. Hence we will have to use graph to find the better one."


# For Government Savings versus Government Healthcare Spending
y = data$GHS
x = data$GS

plot(x, y, cex = 0.5, xlab="Goverment Savings", ylab="Government Healthcare Spending") # Creating the graphing plane

linear = lm(y ~ x, data = data) # Creating linear regression model (assuming homoskedasticity)
hetero_linear = coeftest(linear, vcov = vcovHC(linear, "HC1")) # Correcting for heteroskedasticity
abline(linear,col=7,lwd=2) # Plotting linear regression line

squared = lm(y ~ x + I(x^2), data = data) # Creating quadratic regression model (assuming homoskedasticity)
hetero_squared = coeftest(squared, vcov = vcovHC(squared, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(squared) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=6,lwd=2) # Plotting quadratic regression line

cubed = lm(y ~ x + I(x^2) + I(x^3), data = data) # Creating cubic regression model (assuming homoskedasticity)
hetero_cubed = coeftest(cubed, vcov = vcovHC(cubed, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(cubed) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=5,lwd=2) # Plotting cubic regression line

legend("topleft",c("Cubic","Quadratic","Linear"),lty=c(1,1,1),col=5:7,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Goverment Savings", ylab="Government Healthcare Spending") # Creating the graphing plane
linear_log = lm(y ~ I(log(x)), data = data) # Creating linear-log regression model (assuming homoskedasticity)
hetero_linear_log = coeftest(linear_log, vcov = vcovHC(linear_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(linear_log)) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=4,lwd=2) # Plotting linear-log regression line
legend("topleft",c("Linear-Log"),lty=c(1,1,1),col=4,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Goverment Savings", ylab="Government Healthcare Spending") # Creating the graphing plane
log_linear = lm(I(log(y)) ~ x, data = data) # Creating log-linear regression model (assuming homoskedasticity)
hetero_log_linear = coeftest(log_linear, vcov = vcovHC(log_linear, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_linear)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=3,lwd=2) # Plotting log-linear regression line
legend("topleft",c("Log-Linear"),lty=c(1,1,1),col=3,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Goverment Savings", ylab="Government Healthcare Spending") # Creating the graphing plane
log_log = lm(I(log(y)) ~ I(log(x)), data = data)# Creating log-log regression model (assuming homoskedasticity)
hetero_log_log = coeftest(log_log, vcov = vcovHC(log_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_log)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=2,lwd=2) # Plotting log-log regression line
legend("topleft",c("Log-Log"),lty=c(1,1,1),col=2,lwd=c(2,2,2),bty="n")

# TABLES:

# For Linear:
summary(linear) # Summary of linear model (assuming homoskedasticity)
hetero_linear # Estimates Standard Errors, and p-values for linear model, corrected for heteroskedasticity

# For Squared:
summary(squared) # Summary of quadratic model (assuming homoskedasticity)
hetero_squared # Estimates Standard Errors, and p-values for quadratic model, corrected for heteroskedasticity

# For Cubed:
summary(cubed) # Summary of cubic model (assuming homoskedasticity)
hetero_cubed # Estimates Standard Errors, and p-values for cubic model, corrected for heteroskedasticity

# For Linear-Log:
summary(linear_log) # Summary of linear-log model (assuming homoskedasticity)
hetero_linear_log # Estimates Standard Errors, and p-values for linear-log model, corrected for heteroskedasticity

# For Log-Linear:
summary(log_linear) # Summary of log-linear model (assuming homoskedasticity)
hetero_log_linear # Estimates Standard Errors, and p-values for log-linear model, corrected for heteroskedasticity

# For Log-Log:
summary(log_log) # Summary of log-log model (assuming homoskedasticity)
hetero_log_log # Estimates Standard Errors, and p-values for log-log model, corrected for heteroskedasticity

# FINDING BEST TRANSFORMATION :

waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Quadratic transformations
"Since p-value is 0.7346 > 0.05 (our chosen level of significance), linear model is better than quadratic model"
waldtest(cubed, c("I(x^3)","I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Cubic transformations
"Error: computationally singular system: reciprocal condition number = 1.74503e-25. This means that matrix is irrevertible. Hence we will have to use graph to find the better one."



# For Central Government Debt versus Government Healthcare Spending
y = data$GHS
x = data$CGD

plot(x, y, cex = 0.5, xlab="Central Goverment Debt", ylab="Government Healthcare Spending")# Creating the graphing plane

linear = lm(y ~ x, data = data) # Creating linear regression model (assuming homoskedasticity)
hetero_linear = coeftest(linear, vcov = vcovHC(linear, "HC1")) # Correcting for heteroskedasticity
abline(linear,col=7,lwd=2) # Plotting linear regression line

squared = lm(y ~ x + I(x^2), data = data) # Creating quadratic regression model (assuming homoskedasticity)
hetero_squared = coeftest(squared, vcov = vcovHC(squared, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(squared) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=6,lwd=2) # Plotting quadratic regression line

cubed = lm(y ~ x + I(x^2) + I(x^3), data = data) # Creating cubic regression model (assuming homoskedasticity)
hetero_cubed = coeftest(cubed, vcov = vcovHC(cubed, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = predict(cubed) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=5,lwd=2) # Plotting cubic regression line

legend("topleft",c("Cubic","Quadratic","Linear"),lty=c(1,1,1),col=5:7,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Central Goverment Debt", ylab="Government Healthcare Spending") # Creating the graphing plane
linear_log = lm(y ~ I(log(x)), data = data) # Creating linear-log regression model (assuming homoskedasticity)
hetero_linear_log = coeftest(linear_log, vcov = vcovHC(linear_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(linear_log)) # finding predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=4,lwd=2) # Plotting linear-log regression line
legend("topleft",c("Linear-Log"),lty=c(1,1,1),col=4,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Central Goverment Debt", ylab="Government Healthcare Spending") # Creating the graphing plane
log_linear = lm(I(log(y)) ~ x, data = data) # Creating log-linear regression model (assuming homoskedasticity)
hetero_log_linear = coeftest(log_linear, vcov = vcovHC(log_linear, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_linear)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=3,lwd=2) # Plotting log-linear regression line
legend("topleft",c("Log-Linear"),lty=c(1,1,1),col=3,lwd=c(2,2,2),bty="n")

plot(x, y, cex = 0.5, xlab="Central Goverment Debt", ylab="Government Healthcare Spending") # Creating the graphing plane
log_log = lm(I(log(y)) ~ I(log(x)), data = data) # Creating log-log  regression model (assuming homoskedasticity)
hetero_log_log = coeftest(log_log, vcov = vcovHC(log_log, "HC1")) # Correcting for heteroskedasticity
order.x = sort(x, index=T) # sorting x
ypredict = exp(predict(log_log)) # finding the exponential of predicted values of y
lines(order.x$x, ypredict[order.x$ix],col=2,lwd=2) # Plotting log-log regression line
legend("topleft",c("Log-Log"),lty=c(1,1,1),col=2,lwd=c(2,2,2),bty="n")

# TABLES:

# For Linear:
summary(linear) # Summary of linear model (assuming homoskedasticity)
hetero_linear # Estimates Standard Errors, and p-values for linear model, corrected for heteroskedasticity

# For Squared:
summary(squared) # Summary of quadratic model (assuming homoskedasticity)
hetero_squared # Estimates Standard Errors, and p-values for quadratic model, corrected for heteroskedasticity

# For Cubed:
summary(cubed) # Summary of cubic model (assuming homoskedasticity)
hetero_cubed # Estimates Standard Errors, and p-values for cubic model, corrected for heteroskedasticity

# For Linear-Log:
summary(linear_log) # Summary of linear-log model (assuming homoskedasticity)
hetero_linear_log # Estimates Standard Errors, and p-values for linear-log model, corrected for heteroskedasticity

# For Log-Linear:
summary(log_linear) # Summary of log-linear model (assuming homoskedasticity)
hetero_log_linear # Estimates Standard Errors, and p-values for log-linear model, corrected for heteroskedasticity

# For Log-Log:
summary(log_log) # Summary of log-log model (assuming homoskedasticity)
hetero_log_log # Estimates Standard Errors, and p-values for log-log model, corrected for heteroskedasticity

# FINDING BEST TRANSFORMATION :

waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Quadratic transformations
"Since p-value is 0.8358 > 0.05 (our chosen level of significance), linear model is better than quadratic model"
waldtest(cubed, c("I(x^3)","I(x^2)"), vcov = vcovHC(cubed, "HC1")) # Comparing Linear and Cubic transformations
"Since p-value is 0.5789 > 0.05 (our chosen level of significance), linear model is better than cubic model too"

# Finding Final Model
# -------------------

# Uses only best transformation found for each variable
# Includes all combinations of interaction terms (using best transformations only)
full_model = lm(GHS ~ GDPpc + I(GDPpc^2) + I(GDPpc^3) + ME + I(ME^2) + I(ME^3) + GS + I(GS^2) + I(GS^3) + I(log(CGD)) + I(GDPpc^3 * ME^3)+ I(GDPpc^3 * GS^3) + I(GDPpc^3 * log(CGD)) + I(ME^3 * GS^3) + I(ME^3 * log(CGD)) +  I(GS^3 * log(CGD)) + I(GDPpc^3 * ME^3 * GS^3) + I(GDPpc^3 * ME^3 * log(CGD)) + I(GDPpc^3 * GS^3 * log(CGD)) + I(GS^3 * ME^3 * log(CGD)) + I(GDPpc^3 * GS^3 * ME^3 * log(CGD)), data = data)
hetero_full_model = coeftest(full_model, vcov = vcovHC(full_model, "HC1")) # Correcting for heteroskedasticity
summary(full_model) # Summary of final model (assuming homoskedasticity)
hetero_full_model # Estimates Standard Errors, and p-values for final model, corrected for heteroskedasticity

"Hence, final model is: GHS = ((-2.0089e-11) * (GDPpc^3)) + ((8.5049e-44)*(GDPpc^3 * GS^3) + ((-1.9819e-44)*(GDPpc^3 * GS^3 * log(CGD)))"

# First Stage Regressions of Two Step Least Squares (TSLS or 2SLS)
# ----------------------------------------------------------------

# We used ME for dependent variable of the first stage regressions since it was the only variable not in the final model

first_stage_GDPpc = lm(GDPpc ~ ME, data = data) # First Stage Regression of GDPpc on ME
hetero_first_stage_GDPpc = coeftest(first_stage_GDPpc, vcov = vcovHC(first_stage_GDPpc, "HC1")) # Correcting for heteroskedasticity
hetero_first_stage_GDPpc # Estimates Standard Errors, and p-values for the first stage model, corrected for heteroskedasticity

first_stage_GS = lm(GS ~ ME, data = data) # First Stage Regression of GS on ME
hetero_first_stage_GS = coeftest(first_stage_GS, vcov = vcovHC(first_stage_GS, "HC1")) # Correcting for heteroskedasticity
hetero_first_stage_GS # Estimates Standard Errors, and p-values for the first stage model, corrected for heteroskedasticity

first_stage_CGD = lm(CGD ~ ME, data = data) # First Stage Regression of CGD on ME
hetero_first_stage_CGD = coeftest(first_stage_CGD, vcov = vcovHC(first_stage_CGD, "HC1")) # Correcting for heteroskedasticity
hetero_first_stage_CGD # Estimates Standard Errors, and p-values for the first stage model, corrected for heteroskedasticity

# Syantax of IV Regession in R
# ----------------------------

# This is just an example. Since we do not have any valid instrumental variable, we have commented the lines. If you un-comment, it will show an error since it does not use any data.

# ivreg_model = ivreg(Y ~ X | IV, data = data)
# summary(ivreg_model, vcov = sandwich, df = Inf)