
## ----Settings, echo=FALSE, message=FALSE---------------------------------

rm( list=ls())
source("/Volumes/Storage/RadishData/Scripts/Misc_scripts/Functionarium.R")
require(lme4) #modeling
require(car) #ANOVA
require(RColorBrewer)
require(MuMIn) #summary stats for LMER models

FlowData <- read.csv("CombinedDataSet.csv")
FlowData$FlowDate <- as.Date(FlowData$FlowDate)
FlowData$GermDate <- as.Date(FlowData$GermDate)
FlowData$PlantDate <- as.Date(FlowData$PlantDate)
FlowData$Latency <- FlowData$GermDate - FlowData$PlantDate
FlowData$DOYPS <- ifelse(FlowData$DOYP < 78, FlowData$DOYP + 287, FlowData$DOYP - 78 )
FlowData$DOYGS <- ifelse(FlowData$DOYG < 78, FlowData$DOYG + 287, FlowData$DOYG - 78 )

#write.csv(FlowData, "testdata.csv", quote=F, row.names=F)



## ----Colors, echo=FALSE--------------------------------------------------

col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)



## ----SubsetRrr, echo=FALSE-----------------------------------------------
#Subset out only the *R.r. raphanistrum* populations, and re-factor to make the western populations the base.

RrrData <- droplevels(FlowData[FlowData$Taxonomy == "raphanistrum",])
RrrData$Geography <- relevel(RrrData$Geography, "west")
RrrData$Habitat <- relevel(RrrData$Habitat, "natural")



## ----AllResponsePlots, echo=FALSE, eval=FALSE----------------------------
## par(mfrow=c(2,2))
## hist(FlowData$PTF, probability=T, xlim=c(0,500), breaks=25, main="PTF for entire dataset")
## curve(dnorm(x, mean=mean2(FlowData$PTF), sd=sd2(FlowData$PTF)), col="red", add=T)
## 
## plot(FlowData$PTF, col=col_pal3[FlowData$Experiment], main="PTF by experiment")
## text(x=c(rep(600, 4), rep(1700,5)), y=c(475, 425, 375, 325, 475, 425, 375, 325, 275), labels=levels(FlowData$Experiment), col=col_pal3)
## 
## hist(FlowData$GTF, probability=T, xlim=c(0,500), breaks=25)
## curve(dnorm(x, mean=mean2(FlowData$GTF), sd=sd2(FlowData$GTF)), col="red", add=T)
## 
## plot(FlowData$PTF, col=col_pal3[FlowData$Experiment], type="n", main="GTF by experiment")
## points(FlowData$GTF, col=col_pal3[FlowData$Experiment])
## text(x=c(rep(600, 4), rep(1700,5)), y=c(475, 425, 375, 325, 475, 425, 375, 325, 275), labels=levels(FlowData$Experiment), col=col_pal3)
## 


## ----RrrResponseHist, echo=FALSE-----------------------------------------
par(mfrow=c(1,2))
hist(RrrData$PTF, probability = TRUE, xlim=c(0,200), main="PTF histogram", breaks=50)
curve(dnorm(x, mean=mean2(RrrData$PTF), sd=sd2(RrrData$PTF)), 
  	add=T, col="red")

plot(RrrData$PTF, col=col_pal3[RrrData$Experiment], main="PTF by experiment", ylab="PTF")
text(x=c(rep(600, 4), rep(1200,4)), y=c(225, 200, 175, 150, 225, 200, 175, 150), labels=levels(RrrData$Experiment), col=col_pal3)

hist(RrrData$GTF, probability = TRUE, xlim=c(0,200), main="GTF histogram", breaks=50)
curve(dnorm(x, mean=mean2(RrrData$GTF), sd=sd2(RrrData$GTF)), 
    add=T, col="red")

plot(RrrData$PTF, col=RrrData$Experiment, type="n", main="GTF by experiment", ylab="GTF") #Plotting as same scale as PTF
points(RrrData$GTF, col=col_pal3[RrrData$Experiment])
text(x=c(rep(600, 4), rep(1200,4)), y=c(225, 200, 175, 150, 225, 200, 175, 150), labels=levels(RrrData$Experiment), col=col_pal3)


## ----SeedstockandGeography, echo=FALSE-----------------------------------
par(mfrow=c(1,2), las=1)
plot(RrrData$PTF ~ RrrData$Seedstock, main= "Seedstock")
plot(RrrData$GTF ~ RrrData$Seedstock, main= "Seedstock")
plot(RrrData$PTF ~ RrrData$Geography, main="Geography", xlab="Geography", ylab="PTF")
plot(RrrData$GTF ~ RrrData$Geography, main="Geography", xlab="Geography", ylab="GTF")


## ----Habitat, echo=FALSE, fig.width=9------------------------------------
par(mfrow=c(1,2))

plot(RrrData$PTF ~ RrrData$Habitat, main="Habitat", xlab="Habitat", ylab="PTF")
plot(RrrData$GTF ~ RrrData$Habitat, main="Habitat", xlab="Habitat", ylab="GTF")


## ----DOYPS_DOYGS, echo=FALSE---------------------------------------------
par(mfrow=c(1,2))
plot(RrrData$PTF ~ RrrData$DOYPS, col=col_pal3[RrrData$Experiment], main="DOYPS", xlab="DOYPS", ylab="PTF")
plot(RrrData$GTF ~ RrrData$DOYGS, col=col_pal3[RrrData$Experiment], main="DOYGS", xlab="DOYGS", ylab="GTF")


## ----Vernalized, echo=FALSE----------------------------------------------
par(mfrow=c(1,2))
plot(RrrData$PTF ~ RrrData$DaysVernNoNA, col=col_pal3[RrrData$Experiment], main="Days Vernalized", xlab="DaysVernNoNA", ylab="PTF")
plot(RrrData$GTF ~ RrrData$DaysVernNoNA, col=col_pal3[RrrData$Experiment], main="Days Vernalized", xlab="DaysVernNoNA", ylab="GTF")



## ----Model1Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------
Rrr_lmer_1 <- lmer( PTF ~ Geography + Seedstock + GrowthEnvironment + DOYPS + Habitat + DaysVernNoNA + GrowthEnvironment:Geography + (1 + Geography|Experiment) + (1|Geography:Pop), data=RrrData  )



## ----Model2Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------
Rrr_lmer_2 <- lmer( GTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + Habitat + DaysVernNoNA + GrowthEnvironment:Geography + (1 + Geography|Experiment) + (1|Geography:Pop), data=RrrData  )



## ----Model3Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------
Rrr_lmer_3 <- lmer( GTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + Habitat + DaysVernNoNA + GrowthEnvironment:Geography + (1 + Geography|Experiment) + (1|Geography:Pop), data=RrrData[RrrData$Experiment != "gh2006",]  )



## ----Model4Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------
Rrr_lmer_4 <- lmer( GTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + Habitat + GrowthEnvironment:Geography + (1 + Geography|Experiment) + (1|Geography:Pop), data=RrrData[RrrData$DaysVernNoNA == 0, ]  )


## ----Geo_to_Native, echo=FALSE-------------------------------------------
RrrData$Native <- RrrData$Geography
levels(RrrData$Native) <- c("Native", "Native", "nonNative") 


## ----Model5Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------
Rrr_lmer_5 <- lmer( GTF ~ Native + Seedstock + GrowthEnvironment + DOYGS + Habitat + DaysVernNoNA + GrowthEnvironment:Native + (1 + Native|Experiment) + (1|Native:Pop), data=RrrData  )



## ----LMERsummary, echo=FALSE---------------------------------------------
LMERlist <- list( Rrr_lmer_1, Rrr_lmer_2, Rrr_lmer_3, Rrr_lmer_4, Rrr_lmer_5)

SummaryLMER <- list(rep(NA, length(LMERlist)))



## ----LMER_ANOVA, echo=FALSE, results='hide'------------------------------
for (i in 1:length(LMERlist)){
SummaryLMER[[i]]  <- summary( LMERlist[[i]] )
print(car::Anova(LMERlist[[i]]) )
}


## ----, echo=FALSE--------------------------------------------------------
plot(RrrData$Latency ~ RrrData$Experiment, col=col_pal[RrrData$Pop], pch=16, xlab='Experiment', ylab="Latency in Days")


## ----LMER_goodness, echo=FALSE, fig.height=3-----------------------------
par(mfrow=c(1,3))
for (i in c(3:5)){
plot(fitted(LMERlist[[i]]) ~ LMERlist[[i]]@resp$y, main= c("Response plot",i ), xlab="Response", ylab="Fitted")
abline(0, 1, col="red")
text(min(LMERlist[[i]]@resp$y)+15, c(max(fitted(LMERlist[[i]])), max(fitted(LMERlist[[i]])-8)), labels=c("corr=", round(cor(fitted(LMERlist[[i]]), LMERlist[[i]]@resp$y), 2)))
}

for (i in c(3:5)){
plot(resid(LMERlist[[i]]) ~ fitted(LMERlist[[i]]), main=c("Residual plot", i), ylab="Residuals", xlab="Fitted")
lines(lowess(resid(LMERlist[[i]]) ~ fitted(LMERlist[[i]])), col="red")
#lines(smooth.spline(resid(LMERlist[[i]]) ~ fitted(LMERlist[[i]])), col="green")
#abline(lm(resid(LMERlist[[i]]) ~ fitted(LMERlist[[i]])), col="blue" )
#text("topright", labels=r.squaredGLMM(LMERlist[[i]]))
#print(importance(LMERlist[[i]]))
}

for (i in c(3:5)){
qqnorm(resid(LMERlist[[i]]), main=c("Q-Q plot for residuals", i))
qqline(resid(LMERlist[[i]]))
}


## ----lm_tests1, echo=FALSE, eval=TRUE------------------------------------
Rrr_lm_1 <- lm(PTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + Habitat + DaysVernNoNA + GrowthEnvironment:Geography,  data=RrrData, y=T)
Rrr_lm_2 <- lm(GTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + Habitat + DaysVernNoNA + GrowthEnvironment:Geography,  data=RrrData, y=T)

Rrr_lm_3 <- lm(GTF ~ Native + Seedstock + GrowthEnvironment + DOYGS + Habitat + GrowthEnvironment:Native +  DaysVernNoNA,  data=RrrData, y=T)

car::vif(Rrr_lm_2)


## ----lm_vif, echo=FALSE--------------------------------------------------
car::vif(Rrr_lm_3)


