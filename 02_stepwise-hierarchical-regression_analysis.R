library(car)
library(stargazer)
library(relaimpo)

setwd("C:/Users/rober/Desktop/Class Stats")

head(Prestige)

Prestige.2 <- Prestige[, c(1:4)] 
head(Prestige.2)


###########################################################
########### Stepwise and Hierarchical Regression ##########
###########################################################

cor(Prestige.2)

prestige.education.m1 <- lm(prestige ~ education, data=Prestige.2)
summary(prestige.education.m1)
R2.m1 <- summary(prestige.education.m1)$r.squared
# For every increase in education scores, prestige scores increase by 5.36. This effect is significant (p<0.001).

prestige.education.income.m2 <- lm(prestige ~ education + income, data=Prestige.2)
summary(prestige.education.income.m2)
R2.m2 <- summary(prestige.education.income.m2)$r.squared

summary(prestige.education.income.m2)

# EDUCATION ON PRESTIGE
# For each increase in education scores, prestige increases by 4.14, controlling for income. This effect
# is significant (p<0.001)
# INCOME ON PRESTIGE 
# For each increase in income scores, prestige increases by 0.001, controlling for education. This effect
# significant (p<0.001)

prestige.education.income.women.m3 <- lm(prestige ~ education + income + women, data=Prestige.2)
summary(prestige.education.income.women.m3)
R2.m3 <- summary(prestige.education.income.women.m3)$r.squared
# EDUCATION ON PRESTIGE
# For each increase in education scores, prestige increases by 4.18, controlling for income and percent of women. This effect
# is significant (p<0.001)
# INCOME ON PRESTIGE 
# For each increase in income scores, prestige increases by 0.001, controlling for education and percent of women. This effect
# significant (p<0.001)
# PERCENT WOMEN ON PRESTIGE
# For each increase in percentage of women, prestige decreases by 0.009, controlling for education and income. This effect
# not significant (p>0.05)

anova(prestige.education.m1,prestige.education.income.m2, prestige.education.income.women.m3)

# Model 2 (education+income) significantly improved the fit of the model compared to model 1 (education, p<0.001), however, model 3 (education+income+women) did not
# significantly improve the fit of the model compared to model 2 (p>0.05). 

calc.relimp(prestige.education.income.women.m3, rela = TRUE)
# Of the 79% of variance explained by the model, 61.57% can be attributed to education, 36.22% can be attributed to income and 
# 2.21% can be attributed to percentage of women.

prestige.women.education.m4 <- lm(prestige ~ women + education , data=Prestige.2)
summary(prestige.women.education.m4)
R2.m4 <- summary(prestige.women.education.m4)$r.squared

# PERCENT WOMEN ON PRESTIGE
# For each increase in percentage of women, prestige decreases by 0.09 (an 0.084 increase in magnitude from model 3), controlling for education. This effect
# now became significant (p<0.001), compared to model 3, where it was not significant (p>0.005).

### Comments
# I think that even from seeing our initial correlation table, women and income have a moderately high (and negative) correlation (-0.44), which leads me to believe that
# perhaps the reason why we did not observe a significant change in the model after adding percentage of women as a predictor is because as the percentage of
# women increase (and the occupation becomes more women-dominated), the less income they receive, and thus, the occupation is consequently perceived as 
# having less prestige. Percentage of women in an occupation affects income, and in turn, income level affects perception of prestige. 






