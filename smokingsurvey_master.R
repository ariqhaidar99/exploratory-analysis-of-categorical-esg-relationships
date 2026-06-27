# ==============================================================================
# Project: ESG & Social Attitude Analysis (Smoking Ban Survey)
# Description: Exploratory data analysis, correlation, and Chi-Square testing 
#              on categorical survey data regarding A Potential Scottish Smoking Ban.
# ==============================================================================

options(scipen = 999)
getwd()
list.files()
smoke <- read.csv("smokingsurvey.csv", stringsAsFactors = T)
View(smoke)
str(smoke)
summary(smoke)
# Summary Table
table(smoke$Smoker)
table(smoke$Smoker)/sum(table(smoke$Smoker))
# QUestions
# Q13. Smoking is a disgusting habit.
# Q14. A person smoking in my company reduces my enjoyment of the situation, 
# whether it is a social, work or personal occasion.
# Q15. The smoking ban is the most important piece of public health legislation in generations.
# Q16. The smoking ban is a fantastic idea.
# Q17. The smoking ban will increase my enjoyment of time spent in social 
# environments: such as pubs, restaurants and nightclubs.
# Q18. The smoking ban will reduce the peer pressure experienced by young people with regard to smoking.

par(mar=c(5, 4, 2,2), xpd=TRUE)
barplot(table(smoke$Smoker), ylim = c(0,80), col="red", ylab="No. Respondents",
        main = "People's Attitude Towards Smoking Ban in Scotland")
table(smoke$Smoker, smoke$Q13)
table(smoke$Smoker, smoke$Q13) / rowSums(table(smoke$Smoker, smoke$Q13))

par(mar=c(5, 4, 4, 10), xpd=TRUE)
plot(smoke$Smoker, smoke$Q13, col=c ("blue", "gray", "red"), ylab="Q13: Smoking is a Disgusting Habit", 
     xlab="Smoker", main="People's Response to Q13 in the Smoking Ban Survey")
legend(1.2,1,fill=c("blue","grey","red"),c("Agree","Neutral","Disagree"))

ftable(smoke$Smoker, smoke$Sex, smoke$Q13)
smoke.n <- smoke[, 1:8]
for (a in 1:8) {smoke.n[,a]=as.numeric(smoke[,a])}
cor.test(smoke.n$Q13, smoke.n$Q14, method = "kendall")
cor(smoke.n, method = "kendall")
# Q14 vs Q13 = 0.45940812
cor.test(smoke.n$Q14, smoke.n$Q17, method = "kendall")
cor.test(smoke.n$Q14, smoke.n$Q17, method = "pearson")
# Q14 vs Q17 = 0.4824657
cor.test(smoke.n$Q14, smoke.n$Q16, method = "kendall")
cor.test(smoke.n$Q14, smoke.n$Q16, method = "pearson")
# Q14 vs Q16 = 0.45814841

# -----------------------------------------------------------------------------
table(smoke$Smoker, smoke$Q14)
table(smoke$Smoker, smoke$Q15)        
table(smoke$Smoker, smoke$Q16)
table(smoke$Smoker, smoke$Q17)
table(smoke$Smoker, smoke$Q18)

# -----------------------------------------------------------------------------
smokews5 <- read.csv("SmokingSurveyWS5.csv", stringsAsFactors = T)
View(smokews5)
for (a in 3:4) {smokews5[,a]<-ordered(smokews5[,a], levels = c("Disagree", "Agree"))}
plot(smokews5$Q13~smokews5$Gender, col=c(2, 4), ylab = "Q13: Smoking is a Disgusting Habit", xlab = "Gender", 
     main = "People's Stance on Smoking in Scotland")
q13.g <- table(smokews5$Gender, smokews5$Q13)
q13.g
chisq.test(q13.g)$expected
chisq.test(q13.g)
