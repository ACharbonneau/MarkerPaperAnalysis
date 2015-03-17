
## ----Settings, echo=FALSE, message=FALSE---------------------------------

rm( list=ls())
source("/Volumes/Storage/RadishData/Scripts/Misc_scripts/Functionarium.R")
source("~/Dropbox/DworkinLabSharedMaterial/scripts/WRP_FUNCTIONS.R")
require(lme4) #modeling
require(car) #ANOVA
require(RColorBrewer)
require(MuMIn) #summary stats for LMER models
require(arm) #extractAIC

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

RrrData <- keepCols(RrrData, c("ID", "GTF", "Geography", "Seedstock", "GrowthEnvironment", "DOYGS", "DaysVernNoNA", "Pop", "Experiment", "Habitat", "PTF", "DOYPS", "Native", "Latency"))

RrrData <- RrrData[!is.na(RrrData$GTF),]
RrrData <- RrrData[!is.na(RrrData$DOYGS),]
RrrData$ID <- row.names(RrrData)
RrrData$Geography <- relevel(RrrData$Geography, "west")
RrrData$Habitat <- relevel(RrrData$Habitat, "natural")
RrrData$Ag <- RrrData$Habitat
levels(RrrData$Ag) <- c("nonag", "ag", "nonag")


## ----Model8Rrr, echo=TRUE, eval=TRUE, message=FALSE----------------------

Rrr_lmer_8 <- lmer( GTF ~ Geography + Seedstock + GrowthEnvironment + DOYGS + DaysVernNoNA + GrowthEnvironment:Geography + (1|Experiment) + (1|Geography:Pop), data=RrrData  )


## ----, echo=FALSE--------------------------------------------------------
summary(Rrr_lmer_8)

## ----ModelNoNus, echo=FALSE, eval=TRUE, message=FALSE--------------------
#Making figures: modeling just the stuff we don't care about, then modeling residuals against things we do care about

NoNus <- lmer(GTF  ~ Seedstock + GrowthEnvironment + DOYGS + DaysVernNoNA + (1|Experiment), data=RrrData)

#add the intercept back into the residuals so they give response in days in the second model, and also add in a weighted average of the days caused by the Seedstock coeffecient, because it has such a large effect, and the graphed values will be noticably weird without it. All the other effects are 3 days or less, and so shouldn't make a big difference

NoNusValues <- resid(NoNus) + 
  summary(NoNus)$coefficients["(Intercept)",1] + 
  (summary(NoNus)$coefficients["SeedstockGreenhouse",1] * 
     (table(RrrData$Seedstock)[[2]]/
        (table(RrrData$Seedstock)[[1]]+table(RrrData$Seedstock)[[2]])))


ResidModel <- lmer(NoNusValues ~ Geography * GrowthEnvironment + (1|Geography:Pop), data=RrrData)



## ----GetModelMeans, echo=FALSE-------------------------------------------
RMP_all <- cbind(RrrData$ID, RrrData$Pop, RrrData$GrowthEnvironment, RrrData$Geography, data.frame(ResidModel@resp$mu))
names(RMP_all) <- c("ID", "Pop", "GE", "Geo", "Mean")
#unique(RMP_all[,2:5])



## ----bootMerCI, echo=FALSE, results='hide'-------------------------------

grabvalues <- function( mermod ){
  return( mermod@resp$mu )
}

semiparaboot <- bootMer( ResidModel, FUN= grabvalues, nsim=1000, type="semiparametric", use.u=T )

paraboot <- bootMer( ResidModel, FUN= grabvalues, nsim=1000, type="parametric")

semiSEM <- as.matrix(apply( semiparaboot$t, MARGIN=2, FUN=SEM))

paraSEM <- as.matrix(apply( paraboot$t, MARGIN=2, FUN=SEM))

semiquantCI <- t(apply( semiparaboot$t, MARGIN=2, FUN=quantile, probs=c(0.025, 0.975)))

paraquantCI <- t(apply( paraboot$t, MARGIN=2, FUN=quantile, probs=c(0.025, 0.975)))

#bootMerPopulations
BMP_all <- data.frame(RrrData$ID, RrrData$Pop, RrrData$GrowthEnvironment, RrrData$Geography, semiSEM, paraSEM, semiquantCI, paraquantCI )
names(BMP_all) <- c("ID", "Pop", "GE", "Geo", "semiSEM", "paraSEM", "semiQlow", "semiQhigh", "paraQlow", "paraQhigh")


## ----mergevalues, echo=FALSE---------------------------------------------
Model8Values <- merge( BMP_all, RMP_all )
Model8Values$paraSEMlow <- Model8Values$Mean - (Model8Values$semiSEM*1.96)
Model8Values$paraSEMhigh <- Model8Values$Mean + (Model8Values$semiSEM*1.96)
Model8Values$semiSEMlow <- Model8Values$Mean - (Model8Values$paraSEM*1.96)
Model8Values$semiSEMhigh <- Model8Values$Mean + (Model8Values$paraSEM*1.96)



## ----GetModelSE, echo=FALSE----------------------------------------------

RM_profile <- profile(ResidModel)
confint(ResidModel)



## ----Aggregates, echo=FALSE, results='hide'------------------------------

#Means for field and greenhouse plants for each population
AvgFG <- aggregate(Model8Values[,5:length(names(Model8Values))], by=list( "Pop"=Model8Values$Pop, "Geo"=Model8Values$Geo, "GE"=Model8Values$GE ), FUN=mean)


WM <- mean(Model8Values$Mean[Model8Values$Geo == "west"])
EM <- mean(Model8Values$Mean[Model8Values$Geo == "east"])
NNM <- mean(Model8Values$Mean[Model8Values$Geo == "nonNative"])



## ----non-paraboot, echo=FALSE--------------------------------------------

BSloop = 102
NoNusData <- as.data.frame(cbind(NoNusValues, RrrData))

BSvalues <- as.data.frame(matrix(nrow=29), ncol=BSloop)
names(BSvalues) <- "Pop"

for (i in 3:BSloop){
  thing <- NoNusData[ sample(length(NoNusData$Pop), size=length(NoNusData$Pop), replace=T), ]
  BSModel <- lmer(NoNusValues ~ Geography * GrowthEnvironment + (1|Geography:Pop), data=thing)
  popdata <- data.frame(predict(BSModel), NoNusData$Pop, NoNusData$GrowthEnvironment)
  names(popdata) <- c("Mean", "Pop", "GE")
  tempvalues <- aggregate(popdata[,1], by=list("Pop"=popdata$Pop, GE=popdata$GE), FUN=mean)
  BSvalues$Pop <- tempvalues$Pop
  BSvalues$GE <- tempvalues$GE
  BSvalues[,i] <- tempvalues[,3]
}

#BootStrapPopulations
BSP <- apply( BSvalues[,3:BSloop], MARGIN=1, FUN=quantile, probs=c(0.025, 0.975))
BSP_all <- data.frame(BSvalues$Pop, BSvalues$GE, t(BSP))  
names(BSP_all) <- c("Pop", "GE", "BSlow", "BShigh")
AvgFG <- merge(BSP_all, AvgFG)


#Final merge of all the CI caluculations
Ntable <- as.data.frame(table(RrrData$Pop, RrrData$GrowthEnvironment))
names(Ntable) <- c("Pop", "GE", "N")
AvgFG <- merge(AvgFG, Ntable)
levels(AvgFG$GE) <- c("f", "gh")
AvgFG$PopGE <- as.factor(paste(AvgFG$Pop, AvgFG$GE, sep=""))
AvgFG <- AvgFG[order( AvgFG$Geo, partial=AvgFG$Pop ), ]
AvgFG$GEGeo <- as.factor(paste(AvgFG$GE, AvgFG$Geo, sep=""))
AvgFG

## ----PopMeansGraph1.96, echo=FALSE, fig.height=5-------------------------
par(las=2)
barplot(height=AvgFG$Mean, names.arg=AvgFG$Pop, cex.names=.75, col=col_pal3[c(1, 3, 5, 2, 4,6)][AvgFG$GEGeo], space=c(rep(0, length(AvgFG$Geo[AvgFG$Geo=="west"])), 1, rep(0,(length(AvgFG$Geo[AvgFG$Geo=="east"])-1)), 1, rep(0, 0,(length(AvgFG$Geo[AvgFG$Geo=="nonNative"])-1))), ylim=c(-5, 90), main="Model means with parametric bootMer 1.96SE" ) 


text(y=-2, x=c(.5:length(AvgFG$Geo[AvgFG$Geo=="west"])), labels=AvgFG$N[AvgFG$Geo=="west"], cex=.6)
text(y=-2, x=seq(9.5, 14.5, by=1), labels=AvgFG$N[AvgFG$Geo=="east"], cex=.6)
text(y=-2, x=seq(16.5, 30.5, by=1), labels=AvgFG$N[AvgFG$Geo=="nonNative"], cex=.6)
text(y=-2, x=-.7, labels="N=", cex=.8)

legend("topright", legend=c( "East, Field", "East, Greenhouse", "West, Field", "West, Greenhouse", "NonNative, Field", "NonNative, Greenhouse"),text.col=col_pal3[c(5,6,1,2,3,4)], bty="n" ) 

arrows(c(seq(.5, 7.5, 1), seq(9.5, 14.5, 1), seq(16.5, 30.5, 1)), AvgFG$paraSEMlow, c(seq(.5, 7.5, 1), seq(9.5, 14.5, 1), seq(16.5, 30.5, 1)), AvgFG$paraSEMhigh, length=.05, angle=90, code=3 )



## ----PopMeansGraphSamp, echo=FALSE, fig.height=5-------------------------
par(mfrow=c(1,1), las=2)
barplot(height=AvgFG$Mean, names.arg=AvgFG$Pop, cex.names=.75, col=col_pal3[c(1, 3, 5, 2, 4,6)][AvgFG$GEGeo], space=c(rep(0, length(AvgFG$Geo[AvgFG$Geo=="west"])), 1, rep(0,(length(AvgFG$Geo[AvgFG$Geo=="east"])-1)), 1, rep(0, 0,(length(AvgFG$Geo[AvgFG$Geo=="nonNative"])-1))), ylim=c(-5, 90), , main="Model means with semiparametric bootMer quantiles" ) 


text(y=-2, x=c(.5:length(AvgFG$Geo[AvgFG$Geo=="west"])), labels=AvgFG$N[AvgFG$Geo=="west"], cex=.6)
text(y=-2, x=seq(9.5, 14.5, by=1), labels=AvgFG$N[AvgFG$Geo=="east"], cex=.6)
text(y=-2, x=seq(16.5, 30.5, by=1), labels=AvgFG$N[AvgFG$Geo=="nonNative"], cex=.6)
text(y=-2, x=-.7, labels="N=", cex=.8)

legend("topright", legend=c( "East, Field", "East, Greenhouse", "West, Field", "West, Greenhouse", "NonNative, Field", "NonNative, Greenhouse"),text.col=col_pal3[c(5,6,1,2,3,4)], bty="n" ) 

arrows(c(seq(.5, 7.5, 1), seq(9.5, 14.5, 1), seq(16.5, 30.5, 1)),AvgFG$semiQlow, c(seq(.5, 7.5, 1), seq(9.5, 14.5, 1), seq(16.5, 30.5, 1)), AvgFG$semiQhigh,length=.05, angle=90, code=3 )
