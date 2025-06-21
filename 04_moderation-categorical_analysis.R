library(rockchalk)
library(effects)
library(car)
library(MASS)
library(corrplot)
setwd("C:/Users/rober/Desktop/Class Stats")

############################################
### Moderation with categorical variables ###
############################################

Week7.Data <- read.csv("Week7Data.csv")

Week7.Data$SeatedF <- factor(Week7.Data$Seated,
                              levels = c(0, 1, 2),
                              labels = c("No physical relation", "Same row", "Next to them"))

Week7.Data$Agreeableness.C <- scale(Week7.Data$Agreeableness, center = TRUE, scale = FALSE)[,] 

maineffect <- lm(Friendship ~ Agreeableness.C + SeatedF, data=Week7.Data)
interactioneffect <- lm(Friendship ~ Agreeableness.C * SeatedF, data=Week7.Data)
library(stargazer)
stargazer(maineffect, interactioneffect, type = "text",
                      column.labels = c("Main Effects", "Interaction"),
                      intercept.bottom = FALSE,
                      single.row = FALSE, 
                      notes.append = FALSE,
                      header = FALSE)
ChangeInR <- anova(maineffect, interactioneffect)
knitr::kable(ChangeInR, digits = 4)

vif(interactioneffect)

ncvTest(interactioneffect)

library(rockchalk)
plotSlopes(interactioneffect, plotx = "Agreeableness.C", modx = "SeatedF")

library(stargazer)
stargazer(interactioneffect, type = "text",
          column.labels = c("Main Effects", "Interaction"),
          intercept.bottom = FALSE,
          single.row = TRUE, 
          notes.append = FALSE,
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          header = FALSE)


testSlopes(plotSlopes(interactioneffect, plotx = "Agreeableness.C", modx = "SeatedF"))

### Findings
# For each unit increase in agreeableness, there is a -0.01 decrease in friendship when students sat no 
# where near each other, this relationship is not significant (p>0.05). For each unit increase in 
# agreeableness, there is a 0.1 increase in friendship when students are seated in the same row; 
# this relationship is significant (p<0.01).For each unit increase in agreeableness, there is a 0.23 
# increase in friendship when students are seated next to each other; this relationship is also 
# significant (p<0.01)
# Alternatively:
# We found a significant interaction between how close students seat together and level of agreeableness 
# with the degree of friendship they developed. Specifically, if students sat next to each other, being more
# agreeable significantly increased friendship. If students sat in the same row, being more agreeable also 
# significantly increased friendship. However, when students sat nowhere near each other, agreeableness
# had no significant effect on the level of friendship that students develop.


