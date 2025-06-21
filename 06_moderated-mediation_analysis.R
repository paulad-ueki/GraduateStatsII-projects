
library(mediation)
library(foreign) 
setwd("C:/Users/rober/Desktop/Class Stats")
Week9.Data <- read.spss(file = "Hilletal2016Study3.sav", 
                        to.data.frame = TRUE, use.value.labels = FALSE)

head(Week9.Data)


##############################################################
## Replicating the results from, Hill et al. (2016) Study 3 ##
##############################################################


Week9.Data$WaterSpriteDummy.f <- factor(Week9.Data$WaterSpriteDummy)

Week9.Data$Blood_Draw_2_Post_Maip.C <- scale(Week9.Data$Blood_Draw_2_Post_Maip, center = TRUE, scale = FALSE)[,]
Week9.Data$ChildSES.C <- scale(Week9.Data$ChildSES, center = TRUE, scale = FALSE)[,]

Mod.Med.Model.2 <- lm(Blood_Draw_2_Post_Maip.C ~ WaterSpriteDummy.f + ChildSES.C, data = Week9.Data)
summary(Mod.Med.Model.2)

Mod.Med.Model.3 <- lm(Food_Consumed_Grams ~ WaterSpriteDummy.f + Blood_Draw_2_Post_Maip.C * ChildSES.C, data = Week9.Data)
summary(Mod.Med.Model.3)

highChildSES <- mean(Week9.Data$ChildSES.C) + sd(Week9.Data$ChildSES.C)
lowChildSES <- mean(Week9.Data$ChildSES.C) - sd(Week9.Data$ChildSES.C)

set.seed(42)
Mod.Med.Boot.BCa.1 <- mediate(Mod.Med.Model.2, Mod.Med.Model.3, 
                              covariates = list(ChildSES.C = highChildSES), boot = TRUE, 
                              boot.ci.type = "bca", sims = 200, treat = "WaterSpriteDummy.f", mediator = "Blood_Draw_2_Post_Maip.C")
summary(Mod.Med.Boot.BCa.1)

set.seed(42)
Mod.Med.Boot.BCa.2 <- mediate(Mod.Med.Model.2, Mod.Med.Model.3, 
                              covariates = list(ChildSES.C = lowChildSES), boot = TRUE, 
                              boot.ci.type = "bca", sims = 200, treat = "WaterSpriteDummy.f", mediator = "Blood_Draw_2_Post_Maip.C")
summary(Mod.Med.Boot.BCa.2)

set.seed(42)
Mod.Med.Boot.BCa.3 <- mediate(Mod.Med.Model.2, Mod.Med.Model.3, boot = TRUE, 
                              boot.ci.type = "bca", sims = 200, treat = "WaterSpriteDummy.f", mediator = "Blood_Draw_2_Post_Maip.C")
summary(Mod.Med.Boot.BCa.3)

set.seed(42)
test.modmed(Mod.Med.Boot.BCa.3, covariates.1 = list(ChildSES.C = highChildSES),
            covariates.2 = list(ChildSES.C = lowChildSES), sims = 100)


### Findings
# Socioeconomic status moderated the effect of drink condition (water or Sprite) on calorie consumption through blood glucose 
# levels after the drink. Specifically, there was a significant negative indirect effect of drink condition (Sprite) on calorie consumption 
# mediated by blood glucose levels after the drink for participants of high socioeconomic background (1 SD above the mean). For 
# participants of high socioeconomic background (1 SD above the mean) in the Sprite condition had lower blood glucose levels, 
# which predicted lower calories consumed (b = -6.57, p<0.05, 95% CI [-14.16, -0.28]). For participants of low socioeconomic 
# background (1 SD below the mean), there was a positive indirect effect of drink condition on calorie consumption mediated by 
# blood glucose levels after the drink. In other words, participants who were of low socioeconomic background in the Sprite 
# condition had higher blood glucose levels, which led to increased calorie consumption, but this effect was not significant (b = 1.55, p>0.05, 95% CI [-3.71, 8.42]). 
# The indirect effect of low socioeconomic background is significantly different from the indirect effect of high socioeconomic 
# on the relation between drink condition and calorie consumption through blood glucose levels (p<0.05).
