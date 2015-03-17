rm( list=ls())
source("~/Dropbox/RadishWeedEvolution/MarkerPaperMeta/Functionarium.R")
source("~/Dropbox/DworkinLabSharedMaterial/scripts/WRP_FUNCTIONS.R")
require(lme4) #modeling
require(car) #ANOVA
require(RColorBrewer)
require(MuMIn) #summary stats for LMER models
require(arm) #extractAIC

#Import dataset
FlowData <- read.csv("CombinedDataSet.csv")
FlowData$FlowDate <- as.Date(FlowData$FlowDate)
FlowData$GermDate <- as.Date(FlowData$GermDate)
FlowData$PlantDate <- as.Date(FlowData$PlantDate)
FlowData$DOYPS <- ifelse(FlowData$DOYP < 78, FlowData$DOYP + 287, FlowData$DOYP - 78 )
FlowData$DOYGS <- ifelse(FlowData$DOYG < 78, FlowData$DOYG + 287, FlowData$DOYG - 78 )

# Data Subset for R.r. raphanistrum analysis
RrrData <- droplevels(FlowData[FlowData$Taxonomy == "raphanistrum",])
RrrData <- keepCols(RrrData, c("ID", "GTF", "Geography", "Seedstock", "GrowthEnvironment", "DOYGS", "DaysVernNoNA", "Pop", "Experiment", "Habitat", "PTF", "DOYPS", "Native", "PTG", "SurviveGTF"))

RrrData$ID <- row.names(RrrData)
RrrData$Geography <- relevel(RrrData$Geography, "west")
RrrData$Habitat <- relevel(RrrData$Habitat, "natural")
RrrData$Ag <- RrrData$Habitat
levels(RrrData$Ag) <- c("nonag", "ag", "nonag")
RrrDataSurvive<- RrrData[!is.na(RrrData$SurviveGTF),]
RrrDataSurvive <- RrrDataSurvive[!is.na(RrrDataSurvive$DOYGS),]


# Data Subset for R.raphanistrum natives analysis
NatData <- droplevels(FlowData[FlowData$Geography != "nonNative" & FlowData$Species != "sativus",])
NatData <- keepCols(NatData, c("ID", "GTF", "Geography", "Seedstock", "GrowthEnvironment", "DOYGS", "DaysVernNoNA", "Pop", "Experiment", "Habitat", "PTF", "DOYPS", "Native", "Latency", "Species", "SubSpecies", "Taxonomy", "SurviveGTF"))

NatData$ID <- row.names(NatData)
NatData$Geography <- relevel(NatData$Geography, "west")
NatData$Habitat <- relevel(NatData$Habitat, "natural")
NatData$SubSpecies <- relevel(NatData$SubSpecies, "raphanistrum")
NatDataSurvive <- NatData[!is.na(NatData$SurviveGTF),]
NatDataSurvive <- NatDataSurvive[!is.na(NatDataSurvive$DOYGS),]


#####Model 8
#This is the model that is roughly equivalent to the model we used to look at the differences between the ***R.r. raphanistrum*** plants, just using **SubSpecies** instead of **Geography**, and using the new **SurviveGTF** response. Dropped **GrowthEnvironment** as in Rrr_lmer_14

Nat_lmer_8 <- lmer( SurviveGTF ~ SubSpecies + Seedstock + DaysVernNoNA + (1|Experiment) + (1|SubSpecies:Pop), data=NatDataSurvive  )

#####Model 14:
######Response variable: Days from germination to flowering
#Here, I'm using germination to flowering (or last alive date) as the response, which will be more accurate than planting to flowering, but have many more NA's. Dropping the GrowthEnvironment variables.

Rrr_lmer_14 <- lmer( SurviveGTF ~ Geography + Seedstock + DOYGS + DaysVernNoNA + (1|Experiment) + (1|Geography:Pop), data=RrrDataSurvive  )




