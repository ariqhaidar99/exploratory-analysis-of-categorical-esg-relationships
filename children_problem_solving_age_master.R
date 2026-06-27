# ==============================================================================
# Project: ESG & Social Indicators (Child Problem Solving)
# Description: Exploratory data analysis, linear regression modelling, and 
#              categorical analysis of problem-solving abilities vs. age.
# ==============================================================================

# 1. Setup and Data Import -----------------------------------------------------
options(scipen=999) # Disable scientific notation
# Import dataset (Updated path for the new folder structure)
child <- read.csv( "child problem solving analysis.csv", stringsAsFactors = T)
# Initial Data Inspection
View(child)
str(child)

# 2. Data Cleaning & Transformation --------------------------------------------
# Convert Likert scale responses (Questions 13-18) to ordered ordinal factors
par(mfrow=c(1, 1))
# Base R Scatterplot; pch = point symbol; age on x-axis, problem solving on y-axis
plot(child$Age, child$ProblemSolve, col="red", pch = 19)
# Advanced ggplot2 Scatterplot with Linear Trendline
# install.package("ggplot2") <- incase you have not install ggplot
library(ggplot2)
ggplot(child, aes(x = Age, y = ProblemSolve)) + 
  geom_point(color = "darkblue", alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se = TRUE) + 
  theme_classic() +
  labs(title = "Trend of Problem Solving Ability by Age",
       x = "Age (Years)", 
       y = "Problem Solving Score")

# 3. Linear Regression Modelling -----------------------------------------------
# Fit a linear model: Problem Solving as a function of Age
model_problemsolve <- lm(ProblemSolve ~ Age, data = d2)
# Review model summary (R-squared, p-values, coefficients)
summary(model_problemsolve)
# Insight: For each 1-year increase in age, the problem-solving score 
# increases theoretically by ~1.968 points.

# 4. Categorical Transformation & Analysis -------------------------------------
# Create a new categorical variable based on school year groups (P1-P7)
child$AgeCat <- cut(child$Age, c(0, 6.9, 8.9, 11.2), 
                    label = c("P1-2", "P3-4", "P5-7"), ordered_results=T)
# Visualise distributions across the new categories
View(child)
boxplot(child$ProblemSolve~child$AgeCat, col = 5, xlab="Age Category", ylab="Problem Solving Ability Score")
summary(child, n.arm=T)
# Extract summary statistics by category
tapply(child$ProblemSolve, child$AgeCat, summary)