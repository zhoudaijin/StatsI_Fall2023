# Read the dataset
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
head(inc.sub, n=10)
dim(inc.sub)
names(inc.sub)

# Question 1

# 1-1

# Run linear regression model
regression_model <- lm(voteshare ~ difflog, data = inc.sub)

# Check the result of regression model
summary(regression_model)

# Take the coefficients from model
coefficients_package <- coef(regression_model)

# Sort the coefficients from coefficients package
intercept <- coefficients_package[1]
print(round(intercept, digits = 2))

coef_diddlog <- coefficients_package[2] 
print(round(coef_diddlog, digits = 2))


# 1-2

library(ggplot2)

# Make scatterplot of the two variables and add the regression line by using ggplot
ggplot(inc.sub, aes(x = difflog, y = voteshare))+
  geom_point() + # Add scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Add the regression line
  labs(x = "Difflog", y = "Voteshare") + # Add axis labels
  ggtitle("Scatterplot with Regression Line (Voteshare ~ difflog *1-2*)") # Add title

# 1-3

# Save the residuals of the model in a separate object
residuals <- resid(regression_model)

# Check the head line of residuals
head(residuals)

# Abstract fitted values 
fitted_values <- fitted(regression_model)

# Plot residuals 
plot(fitted_values, residuals, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "blue", lty = 2) 

# Check whether the residual follows the normal distribution
plot (density(residuals) ,
        main= " Density of residuals " ,
        ylab = "Y" , xlab = "X" ,
        cex.axis = 1.5 , cex.lab = 2, cex.main = 1.5 , lwd = 3 )




# Question 2

# 2-1

# Perform linear regression analysis
regression_model_2 <- lm(presvote ~ difflog, data = inc.sub)

# Check the result of regression model
summary(regression_model_2)

# Take the coefficients from model
coefficients_package_2 <- coef(regression_model_2)
print(coefficients_package_2)

# Sort the coefficients from coefficients package
intercept_2 <- coefficients_package_2[1]
print(round(intercept_2, digits = 2))

coef_diddlog_2 <- coefficients_package_2[2] 
print(round(coef_diddlog_2, digits = 2))

# 2-2

library(ggplot2)

# Make scatterplot of the two variables and add the regression line by using ggplot
ggplot(inc.sub, aes(x = difflog, y = presvote))+
  geom_point() + # Add scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Add the regression line
  labs(x = "Difflog", y = "Presvote") + # Add axis labels
  ggtitle("Scatterplot with Regression Line (Presvote ~ difflog *2-2*)") # Add title

# 2-3

# Save the residuals of the model in a separate object
residuals_2 <- resid(regression_model_2)

# Check the head line of residuals_2
head(residuals_2)

# Abstract fitted values 
fitted_values_2 <- fitted(regression_model)

# Plot residuals 
plot(fitted_values_2, residuals_2, xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "blue", lty = 2) 

# Check whether the residual follows the normal distribution
plot (density(residuals_2) ,
      main= " Density of residuals " ,
      ylab = "Y" , xlab = "X" ,
      cex.axis = 1.5 , cex.lab = 2, cex.main = 1.5 , lwd = 3 )



# Question 3

# 3-1

# Perform linear regression analysis
regression_model_3 <- lm(voteshare ~ presvote, data = inc.sub)

# Check the result of regression model
summary(regression_model_3)

# Take the coefficients from model
coefficients_package_3 <- coef(regression_model_3)
print(coefficients_package_3)

# Sort the coefficients from coefficients package
intercept_3 <- coefficients_package_3[1]
print(round(intercept_3, digits = 2))

coef_presvote_3 <- coefficients_package_3[2] 
print(round(coef_presvote_3, digits = 2))


# 3-2

library(ggplot2)

# Make scatterplot of the two variables and add the regression line by using ggplot
ggplot(inc.sub, aes(x = presvote, y = voteshare))+
  geom_point() + # Add scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Add the regression line
  labs(x = "Presvote", y = "Voteshare") + # Add axis labels
  ggtitle("Scatterplot with Regression Line (Voteshare ~ Presvote *3-2*)") # Add title




# Question 4

# 4-1

# Perform linear regression analysis
regression_model_4 <- lm(residuals ~ residuals_2, data = inc.sub)

# Check the result of regression model
summary(regression_model_4)

# Take the coefficients from model
coefficients_package_4 <- coef(regression_model_4)
print(coefficients_package_4)

# Sort the coefficients from coefficients package
intercept_4 <- coefficients_package_4[1]
print(round(intercept_4, digits = 2))

coef_residuals_2_4 <- coefficients_package_4[2] 
print(round(coef_residuals_2_4, digits = 2))

# 4-2

library(ggplot2)

# Make scatterplot of the two variables and add the regression line by using ggplot
ggplot(inc.sub, aes(x = residuals_2, y = residuals))+
  geom_point() + # Add scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Add the regression line
  labs(x = "Residuals_2", y = "Residuals") + # Add axis labels
  ggtitle("Scatterplot with Regression Line (Residuals ~ Residuals_2 *4-2*)") # Add title


# Question 5

# 5-1

# Perform linear regression analysis
regression_model_5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# Check the result of regression model
summary(regression_model_5)

# Take the coefficients from model
coefficients_package_5 <- coef(regression_model_5)
print(coefficients_package_5)

# Sort the coefficients from coefficients package
intercept_5 <- coefficients_package_5[1]
print(round(intercept_5, digits = 2))

coef_difflog_5 <- coefficients_package_5[2] 
print(round(coef_difflog_5, digits = 2))

coef_presvote_5 <- coefficients_package_5[3] 
print(round(coef_presvote_5, digits = 2))








