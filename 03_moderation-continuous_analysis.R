library(rockchalk)
library(car)
library(MASS)
library(corrplot) 
library(stargazer) 

setwd("C:/Users/rober/Desktop/Class Stats")

#############################################################
########## Moderation with continuous predictors ##########
############################################################

Week6.Data <- read.csv("Week6Data.csv")

cor(Week6.Data)

Week6.Data$Moving.Together.C <- scale(Week6.Data$Moving.Together, center = TRUE, scale = FALSE)[,] 
Week6.Data$Personality.Type.C <- scale(Week6.Data$Personality.Type, center = TRUE, scale = FALSE)[,] 

Centered.Bonding.m1 <- lm(Feeling.Bonding ~ Moving.Together.C + Personality.Type.C, Week6.Data)
Centered.Bonding.m2 <- lm(Feeling.Bonding ~ Moving.Together.C * Personality.Type.C, Week6.Data) 
library(stargazer)
stargazer(Centered.Bonding.m1, Centered.Bonding.m2, type = "text",
          column.labels = c("Main Effects", "Interaction"),
          intercept.bottom = FALSE,
          single.row = FALSE, 
          notes.append = FALSE,
          header = FALSE)
ChangeInR <- anova(Centered.Bonding.m1, Centered.Bonding.m2)
knitr::kable(ChangeInR, digits = 4)
vif(Centered.Bonding.m2)

ncvTest(Centered.Bonding.m2)
plot(Centered.Bonding.m2,1)

plot(Centered.Bonding.m2,2)

cutoff <- 4/((nrow(Week6.Data) - length(Centered.Bonding.m2$coefficients) - 2)) 

plot(Centered.Bonding.m2, which = 5, cook.levels = cutoff)
Week6.Data$CooksD <- cooks.distance(Centered.Bonding.m2)
Week6Data_cleaned <- subset(Week6.Data, CooksD < cutoff)
nrow(Week6Data_cleaned) 
nrow(Week6.Data) 

library(rockchalk)
m1ps <- plotSlopes(Centered.Bonding.m2, modx = "Personality.Type.C", plotx = "Moving.Together.C", n = 3, modxVals = "std.dev", ylim = c(-100,100))

m1psts <- testSlopes(m1ps)
round(m1psts$hypotests, 4)

### Findings
# We found a significant interaction between personality type and moving together with feeling
# bonded. Specifically, at average personality scores, moving more together significantly 
# increased feeling bonded. At lower personality scores (more extroverted), moving more
# together significantly increased feeling bonded. However, at higher personality scores 
# (more introverted), there was no significant effect on feeling bonded.
