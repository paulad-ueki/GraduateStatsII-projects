library(mediation)
library(haven) 
setwd("C:/Users/rober/Desktop/Class Stats")
Week8.Data <- read_sav("VanZantMoore2015Experiment2.sav")
head(Week8.Data)

##########################################################################
## Replicating the results from Van Zant and Moore (2015) Experiment 2 ##
##########################################################################

Week8.Data$private_frame.f <- factor(Week8.Data$private_frame,
                             levels = c(0, 1),
                             labels = c("pragmatic", "moral"))

Model.1 <- lm(policy_support_index ~ private_frame.f, data = Week8.Data)
summary(Model.1)

Model.2 <- lm(moral_character_index ~ private_frame.f, data = Week8.Data)
summary(Model.2)

Model.3 <- lm(policy_support_index ~ moral_character_index + private_frame.f, data = Week8.Data)
summary(Model.3)

### Findings pt. 1
# When a leader frames a solution in a moral way, support for a policy is significantly higher (p<0.05) compared to when a leader frames a solution in a pragmatic 
# way, controlling for level of perceived moral character.
# When a leader frames a solution in a moral way rather than a pragmatic way, there is an increase of 0.23 unit increase in policy support. This effect is 
# statistically significant (p<0.05)
# As the perception of moral character in a leader increases, the amount of support for a policy significantly increases (p<0.001), controlling for 
# the type of framing that the leader chose to explain the policy.
# For each unit increase in perceived moral character, support for a policy increases by 0.76, controlling for the type of framing. This effect is 
# statistically significant (p<0.001)

library(mediation)
Med.Boot.BCa <- mediate(Model.2, Model.3, boot = TRUE, 
                        boot.ci.type = "bca", sims = 200, treat = "private_frame.f", mediator = "moral_character_index")
summary(Med.Boot.BCa)

plot(Med.Boot.BCa)

### Fidings pt. 2
# Overall, the analysis suggested that perceived moral character in leaders mediated the relationship between the type of frame the leader used to justify
# a policy and policy support.Specifically, leaders who used a moral frame to explain a policy were perceived as having higher moral character, 
# which in turn, predicted greater support for the policy.The indirect effect of type of frame used to explain a policy on policy support through 
# perception of moral character was significant, b = 0.43, p < 0.001, 95% CI [0.23, 0.62].







