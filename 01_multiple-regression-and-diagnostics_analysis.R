
library(car)
library(ppcor) 
library(effects)

setwd("C:/Users/rober/Desktop/Class Stats")

Week4.Data <- read.delim('Week4Lab.txt')
head(Week4.Data)


################################################
###### Multiple Regression and Diagnostics######
################################################

cor(Week4.Data)

# An indicator multicollinearity can be high correlations between variables (such as 0.6-0.7) and very high correlations (such as 0.8-0.9) may be an
# indicator that it may not be a good idea to put those variables in a regression. Given the set zero-order correlations I conducted above, some of the
# predictor variables that may be of concern are: TIME and PUBS (R=0.65), 


salary.reg.time <- lm(SALARY~TIME, data=Week4.Data)
summary(salary.reg.time)


salary.reg.pubs <- lm(SALARY~PUBS, data=Week4.Data)
summary(salary.reg.pubs)


salary.reg.cits <- lm(SALARY~CITS, data=Week4.Data)
summary(salary.reg.cits)


# TIME on SALARY
# For each additional year passed since finishing a PhD, salary increases by $1379.3. This effect is significant (p<0.001).
# PUBS on SALARY
# For each additional publication published, salary increases by $350.80. This effect is also significant (p<0.001)
# CITS on SALARY
# For each additional citation added, salary increases by $310.75. This effect is also significant (p<0.001)


time.pubs.cits.model <- lm(SALARY ~ TIME + PUBS + CITS, data = Week4.Data)
summary(time.pubs.cits.model)


# TIME on SALARY
# For each additional year passed since finishing a PhD, salary increases by $874.46, controlling for number of publications and citations.
# This effect is still significant (p<0.01) and decreased from the simple regression.
# PUBS on SALARY
# For each additional publication published, salary increases by $93.61, controlling for time passed since completing a a PhD and citations.
# This effect no longer significant (p>0.05) and decreased from the simple regression.
# CITS on SALARY
# For each additional citation added, salary increases by $204.06, controlling for time passed and publications. This effect is also still
# still significant (p<0.001) and it decreased from the simple regression.


vif(time.pubs.cits.model) > 4
# No. we do not have a problem with multicollinearity. We did not violate the assumption of multicollinearity. All of our values are less than 4.

plot(time.pubs.cits.model,1)

ncvTest(time.pubs.cits.model)
# No, we have not violated the assumption of homoscedasticity because the outcome of the error variance test is not significant (p>0.05)


plot(time.pubs.cits.model,2)
# No, we have not violated the assumption of linearity, since the dots are close to the diagonal line 
# with the exception of some variation at the tail ends of the line.

cutoff <- 4/((nrow(Week4.Data) - length(time.pubs.cits.model$coefficients) - 2)) 
cutoff


plot(time.pubs.cits.model, which = 5, cook.levels = cutoff)
plot(time.pubs.cits.model, which = 4, cook.levels = cutoff)
# We have 6 outliers.

Week4.Data$CooksD <- cooks.distance(time.pubs.cits.model)
Week4Data_cleaned <- subset(Week4.Data, CooksD < cutoff)
nrow(Week4Data_cleaned)

time.pubs.cits.model.CLEAN <- lm(SALARY ~ TIME + PUBS + CITS, data = Week4Data_cleaned)
summary(time.pubs.cits.model.CLEAN)

# TIME on SALARY
# For each additional year passed since finishing a PhD, salary increases by $1141.10, controlling for number of publications and citations.
# This effect is still significant (p<0.001) and it increased when we removed outliers.
# PUBS on SALARY
# For each additional publication published, salary increases by $22.42 controlling for time passed since completing a a PhD and citations.
# This effect still not significant (p>0.05) and decreased when we removed outliers.
# CITS on SALARY
# For each additional citation added, salary increases by $204.47, controlling for time passed and publications. This effect is also significant 
# p<0.001) and only slightly increased when we removed outliers.









