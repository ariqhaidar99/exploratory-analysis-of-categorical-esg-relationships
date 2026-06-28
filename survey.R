# ==============================================================================
# Project: Corporate Governance & Employee Sentiment Analysis
# Description: Categorical data analysis evaluating the relationship between 
#              employee outlook (sentiment) and performance metrics using 
#              contingency tables and Chi-Square diagnostics.
# ==============================================================================

options(scipen = 999)

# 1. Setup and Data Import -----------------------------------------------------
# Import dataset 
# (Note: Renamed from 'Dataset 1 - Survey.csv' to reflect business context)
ss <- read.csv("corporate_outlook_performance_survey.csv", stringsAsFactors = TRUE)

# Initial Data Inspection
str(ss)
levels(ss$outlook)
levels(ss$performance)

# 2. Exploratory Data Analysis & Manual Proportion Checks ----------------------
# Ad-hoc calculations for demographic proportions and expected frequencies
15 * 55 / 100    # Expected frequency manual check (Row Total * Col Total / Grand Total)
15 / 298 * 100   # Sub-population percentage check (~5.03%)
283 / 320        # Majority proportion check (~88.4%)
# Build a contingency table comparing Employee Outlook vs Performance
ss.table <- table(ss$outlook, ss$performance)

# Review raw frequency counts
print(ss.table)

# Generate summary statistics for the contingency table
summary(ss.table)

# Calculate a specific targeted business proportion (e.g., 44 out of 70)
target.proportion <- (44 / 70) * 100
print(paste0("Target Demographic Proportion: ", round(target.proportion, 1), "%"))

# 3. Hypothesis Testing (Chi-Square) -------------------------------------------
# Evaluate expected frequencies to ensure Chi-Square statistical assumptions are met
# (e.g., checking if expected counts are > 5 before accepting the full test)
chisq.expected <- chisq.test(ss.table)$expected
print("Expected Frequencies:")
print(chisq.expected)

# Run the full Chi-Square Test to determine if Outlook and Performance are dependent
chisq.result <- chisq.test(ss.table)
print(chisq.result)