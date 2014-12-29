lmm1 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DaysVernNoNA  + GrowthEnvironment:Geography + (1 + Geography|Experiment) + (1|Geography:Pop), data=RrrData)

lmm1 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DaysVernNoNA  + GrowthEnvironment:Geography + (1 + Geography|Experiment), data=RrrData)


lmm2 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DOY + DaysVernNoNA  + GrowthEnvironment:Geography + (1 + Geography|Experiment), data=RrrData)

lm1 <- lm(PTF ~ Geography +  Seedstock + GrowthEnvironment + DOY + GrowthEnvironment:Geography, data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lm1)
car::Anova(lm1)
cov2cor(vcov(lm1))
car::vif(lm1)
str(RrrData)

lmm2 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DOY + GrowthEnvironment:Geography + (1 + Geography|Experiment), data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lmm2)
car::Anova(lmm2)

lmm3 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DOY + GrowthEnvironment:Geography 
    + (1 + Geography|Experiment) + (1|Pop), data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lmm3)
car::Anova(lmm3)

lmm4 <- lmer( PTF ~ Geography +  Seedstock + GrowthEnvironment + DOY + Habitat +  
    + (1 + Geography|Experiment) + (1|Pop), data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lmm4)
car::Anova(lmm4)

RrrData$Native <- RrrData$Geography
levels(RrrData$Native) <- c("Native", "Native", "NonNative") 


lmm5 <- lmer( PTF ~ Native +  Seedstock + GrowthEnvironment + DOY + Habitat +  
    + (1 + Native|Experiment) + (1|Pop), data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lmm5)
car::Anova(lmm5)

lmm6 <- lmer( PTF ~ Native +  Seedstock + GrowthEnvironment + DOY + Habitat +  
    + (1 + Native|Experiment) + (1|Native:Pop), data=RrrData[RrrData$DaysVernNoNA == 0, ])
summary(lmm6)
car::Anova(lmm6)