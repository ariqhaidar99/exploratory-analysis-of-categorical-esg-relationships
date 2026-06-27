# ==============================================================================
# Project: ESG & Environmental Analysis (Air Pollution in 4 Cities)
# Description: Exploratory data analysis, descriptive statistics, and
#              hypothesis testing (Student's and Paired t-tests) on 
#              atmospheric sulphur dioxide concentrations.
# ==============================================================================
options(scipen=999) # Disable scientific notation for clearer p-values
# Dont forget to setwd()

# 1. Setup and Data Import -----------------------------------------------------
list.files()
ap <- read.csv("4 cities air pollution.csv" , stringsAsFactors = T)
View(ap) # Initial Data Inspection
str(ap)
#Check data inputs
is.na(ap)
#Check data inputs (inverted)
!is.na(ap)

# 2. Exploratory Data Analysis (EDA) -------------------------------------------
# Set up a 2x2 plotting pane for comparative histograms
par(mfrow=c(2, 2))
# Air Quality analysis of the 4 cities
# Histograms for each city
hist(ap$London, col = "red", xlab="Pollution Level", ylab="Tally", main="London")
hist(ap$Beijing, col = "green", xlab="Pollution Level", ylab="Tally", main="Beijing")
hist(ap$Moscow, col = "blue", xlab="Pollution Level", ylab="Tally", main="Moscow")
hist(ap$LosAngeles, col = "purple", xlab="Pollution Level", ylab="Tally", main="Los Angeles")
# 1x1 Box and whisker plot for direct comparison
par(mfrow=c(1, 1))
boxplot(ap, col=rainbow (4), xlab="City", ylab="PollutionLevel", 
        main="Air Pollution Level in 4 Big Cities")
# n.arm 
summary(ap, n.arm=T)

# 3. Descriptive Statistics (Beijing) ------------------------------------------
summary(ap) 
# Range
max(ap$Beijing, na.rm = T) - min(ap$Beijing, na.rm = T)
# Standard Deviation
sd(ap$Beijing, na.rm = T)
# InterQuartile Range
IQR(ap$Beijing, na.rm = T)
# Max Values
max(ap$London, na.rm = T) - min(ap$London, na.rm = T)
max(ap$LosAngeles, na.rm = T) - min(ap$LosAngeles, na.rm = T)
max(ap$Moscow, na.rm = T) - min(ap$Moscow, na.rm = T)

# 4. Hypothesis Testing (T-Tests) ----------------------------------------------
# Comparing Los Angeles to other cities (Independent and Paired)
# Los Angeles vs Moscow
t.test(d1$LosAngeles, d1$Moscow)
t.test(d1$LosAngeles, d1$Moscow, paired = TRUE)
# Los Angeles vs Beijing
t.test(d1$LosAngeles, d1$Beijing)
t.test(d1$LosAngeles, d1$Beijing, paired = TRUE)
# Los Angeles vs London
t.test(d1$LosAngeles, d1$London)
t.test(d1$LosAngeles, d1$London, paired = TRUE)