
## ----Settings, echo=FALSE, message=FALSE---------------------------------
rm( list=ls())
require(plyr)


## ----ReadIN, echo=FALSE--------------------------------------------------

cult_2013 <- read.csv("Cultivar GH 2013.csv", na.strings=".")

gh_2006 <- read.csv("2006Greenhouse.csv", na.strings=".")

IS_2013 <- read.csv("IsraelSpain Pops 2013 GH.csv", na.strings=".")

lale2005 <- read.csv("LaleField2005.csv", na.strings=".")

plants2012 <- read.csv("2012FieldData.csv", sep=",", na.strings="")

qst_2003 <- read.csv("2003QstParents.csv", na.strings=".")

qst_2004 <- read.csv("2004 QstOffspring.csv", na.strings=".")

spring2013 <- read.csv("2013plantsSpring.csv", sep=",", na.strings="")

sum2010 <- read.csv("Summer2010dataSummary.csv", na.strings=".")


## ----Dates1--------------------------------------------------------------
# fix dates for Cultivar GH 2013.csv
cult_2013$planted <- as.POSIXct( strptime(cult_2013$planted, format="%m/%d/%y"))
cult_2013$Germ <- as.POSIXct( strptime(cult_2013$Germ, format="%m/%d/%y"))
cult_2013$crownTop <- as.POSIXct( strptime(cult_2013$crownTop, format="%m/%d/%y"))
cult_2013$crownSide <- as.POSIXct( strptime(cult_2013$crownSide, format="%m/%d/%y"))
cult_2013$FlowerDate <- as.POSIXct( strptime(cult_2013$FlowerDate, format="%m/%d/%y"))
cult_2013$FlowerPics <- as.POSIXct( strptime(cult_2013$FlowerPics, format="%m/%d/%y"))
cult_2013$TissueColl <- as.POSIXct( strptime(cult_2013$TissueColl, format="%m/%d/%y"))



## ----Dates2, echo=FALSE--------------------------------------------------
# fix dates for gh_2006
gh_2006$flowerDate <- as.POSIXct( strptime(gh_2006$flowerDate, format="%m/%d/%y"))
gh_2006$PlantingDate <- as.POSIXct( strptime(gh_2006$PlantingDate, format="%m/%d/%y"))

# fix dates for 2006Greenhouse.csv
IS_2013$Planted <- as.POSIXct( strptime( IS_2013$Planted, format="%m/%d/%y"))
IS_2013$Germ <- as.POSIXct( strptime( IS_2013$Germ, format="%m/%d/%y"))
IS_2013$crownTop <- as.POSIXct( strptime( IS_2013$crownTop, format="%m/%d/%y"))
IS_2013$crownSide <- as.POSIXct( strptime( IS_2013$crownSide, format="%m/%d/%y"))
IS_2013$X1stFlwrDate <- as.POSIXct( strptime( IS_2013$X1stFlwrDate, format="%m/%d/%y"))
IS_2013$FlwrPicDate <- as.POSIXct( strptime( IS_2013$FlwrPicDate, format="%m/%d/%y"))
IS_2013$Tissue.coll <- as.POSIXct( strptime( IS_2013$Tissue.coll, format="%m/%d/%y"))
IS_2013$GTF <- IS_2013$X1stFlwrDate - IS_2013$Germ

# fix dates for LaleField2005.csv
lale2005$Planting_Date <- as.POSIXct( strptime(lale2005$Planting_Date, format="%m/%d/%y"))
lale2005$Germination_Date <- as.POSIXct( strptime(lale2005$Germination_Date, format="%m/%d/%y"))
lale2005$First_FlowerDate <- as.POSIXct( strptime(lale2005$First_FlowerDate, format="%m/%d/%y"))
lale2005$Mortality_Date <- as.POSIXct( strptime(lale2005$Mortality_Date, format="%m/%d/%y"))
lale2005$OvuleDate <- as.POSIXct( strptime(lale2005$OvuleDate, format="%m/%d/%y"))
lale2005$family <- as.factor(lale2005$family)
lale2005$PTF <- lale2005$First_FlowerDate - lale2005$Planting_Date

# fix dates for 2012FieldData.csv
plants2012$Planting_Date <- as.POSIXct( strptime(plants2012$Planting_Date, format="%m/%d/%y"))
plants2012$Germ_Date <- as.POSIXct( strptime(plants2012$Germ_Date, format="%m/%d/%y"))
plants2012$Died_By <- as.POSIXct( strptime(plants2012$Died_By, format="%m/%d/%y"))
plants2012$Bolted_Date <- as.POSIXct( strptime(plants2012$Bolted_Date, format="%m/%d/%y"))
plants2012$Flowering_Date <- as.POSIXct( strptime(plants2012$Flowering_Date, format="%m/%d/%y"))
plants2012$Date_Blossom_Collected <- as.POSIXct( strptime(plants2012$Date_Blossom_Collected, format="%m/%d/%y"))
plants2012$second_bolt <- as.POSIXct( strptime(plants2012$second_bolt, format="%m/%d/%y"))
plants2012 <- droplevels(plants2012[plants2012$subpop != "unknown" & plants2012$subpop != "cross" & plants2012$subpop != "confusus", ])
plants2012$PTF <- plants2012$Flowering_Date - plants2012$Planting_Date
plants2012$Survive_winter <- ifelse(plants2012$Survive_winter==0, "No", "Yes")

# fix dates for 2003QstParents.csv"
qst_2003$PlantingDate <- as.POSIXct( strptime( qst_2003$PlantingDate, format="%m/%d/%y"))

# fix dates for 2004 QstOffspring.csv
qst_2004$plantingDate <- as.POSIXct( strptime( qst_2004$plantingDate, format="%d-%b-%y"))

# fix dates for 2013plantsSpring.csv
spring2013$PD <- as.POSIXct( strptime(spring2013$Plant_Date, format="%m/%d/%y"))
spring2013$GD <- as.POSIXct( strptime(spring2013$Germ_Date, format="%m/%d/%y"))
spring2013$BD <- as.POSIXct( strptime(spring2013$Bolt_Date, format="%m/%d/%y"))
spring2013$FD <- as.POSIXct( strptime(spring2013$Flower_Date, format="%m/%d/%y"))
spring2013$blossom <- as.POSIXct( strptime(spring2013$Blossom_Collected, format="%m/%d/%y"))
spring2013$PTF <- spring2013$FD - spring2013$PD
spring2013$DTB <- spring2013$BD - spring2013$GD
spring2013$DTF <- spring2013$FD - spring2013$GD

# fix dates for Summer2010dataSummary.csv
sum2010$planted <- as.POSIXct( strptime( sum2010$planted, format="%m/%d/%y"))
sum2010$germDate <- as.POSIXct( strptime( sum2010$germDate, format="%m/%d/%y"))
sum2010$rosettePhoto <- as.POSIXct( strptime( sum2010$rosettePhoto, format="%m/%d/%y"))
sum2010$X1stflwr <- as.POSIXct( strptime( sum2010$X1stflwr, format="%m/%d/%y"))
sum2010$blossomPhoto <- as.POSIXct( strptime( sum2010$blossomPhoto, format="%m/%d/%y"))
sum2010$tissueColl <- as.POSIXct( strptime( sum2010$tissueColl, format="%m/%d/%y"))
sum2010$X2ndRosettePhoto <- as.POSIXct( strptime( sum2010$X2ndRosettePhoto, format="%m/%d/%y"))


## ----Subsetting1---------------------------------------------------------
columnnames <- c("Experiment", "ID", "GrowthEnvironment", "Pop", "PTF", "DTB", "GTF", "Year", "FlowDate", "PlantDate", "GermDate", "HeightFFcm", "Vernalized", "DaysVern", "TimesVern")

# subset cult_2013
cult_2013sub <- data.frame(rep("Cult2013", length(cult_2013$Cultivar)),  	#Experiment
                         paste(cult_2013$Cultivar, cult_2013$Pot, sep=""),		#ID
                         rep("greenhouse", length(cult_2013$Cultivar)), 		#GrowthEnvironment
                         cult_2013$Cultivar, 		#Pop
                         cult_2013$FlwrTime,		#PTF
                         rep(NA, length(cult_2013$Cultivar)), 		#DTB
                         round(cult_2013$FlowerDate - cult_2013$Germ), 		#GTF
                         rep("2013", length(cult_2013$Cultivar)), 		#Year
                         cult_2013$FlowerDate, 		#FlowDate
                         cult_2013$planted,     #PlantDate
                         cult_2013$Germ,		#GermDate
                         cult_2013$X1stFlwrHt_cm,		#HeightFFcm
                         rep("No", length(cult_2013$Cultivar)),		#Vernalized
                         rep(NA, length(cult_2013$Cultivar)),		#DaysVern
                         rep(NA, length(cult_2013$Cultivar)))		#TimesVern


## ----Vernalization, echo=FALSE-------------------------------------------
lale_05_06 <- read.csv("Weatherdata/maxtemps_05_06.txt", header=F)
lale_05_06$year <- substr( as.character(lale_05_06$V1), 1, 4 )
lale_05_06$vern <- ifelse(lale_05_06$V2 < 55, "vern", "")


winter_12_13 <- read.csv("Weatherdata/maxtemps_12_13.txt", header=F)
winter_12_13$year <- substr( as.character(winter_12_13$V1), 1, 4 )
winter_12_13$vern <- ifelse(winter_12_13$V2 < 55, "vern", "")


winter_13_14 <- read.csv("Weatherdata/maxtemps_13_14.txt", header=F)
winter_13_14$year <- substr( as.character(winter_13_14$V1), 1, 4 )
winter_13_14$vern <- ifelse(winter_13_14$V2 < 55, "vern", "")




## ----Subsetting2, echo=FALSE---------------------------------------------
gh2006sub <- data.frame(rep("gh2006", length(gh_2006$Pop)),  	#Experiment
                         paste(gh_2006$Pop, gh_2006$Individual,	sep=""),	#ID
                         rep("greenhouse", length(gh_2006$Pop)), 		#GrowthEnvironment
                         gh_2006$Pop, 		#Pop
                         gh_2006$FlwrTime,		#PTF
                         rep(NA, length(gh_2006$Pop)), 		#DTB
                         rep(NA, length(gh_2006$Pop)), 		#GTF
                         rep("2006", length(gh_2006$Pop)), 		#Year
                         gh_2006$flowerDate, 		#FlowDate
                         gh_2006$PlantingDate,     #PlantDate
                         rep(NA, length(gh_2006$Pop)),		#GermDate
                         (gh_2006$Hgt1stFlwr_mm / 100),		#HeightFFcm
                         ifelse(gh_2006$DaysVernal > 0, "Yes", "No"),		#Vernalized
                         gh_2006$DaysVernal,		#DaysVern
                         gh_2006$NumVernaliz)		#TimesVern
                         
IS_2013sub <- data.frame(rep("IS2013", length(IS_2013$Population)),		#Experiment
                         paste(IS_2013$Population, round(IS_2013$Pot), sep=""),		#ID
                         rep("greenhouse", length(IS_2013$Population)), 		#GrowthEnvironment
                         IS_2013$Population, 		#Pop
                         IS_2013$FlwrT,		#PTF
                         rep(NA, length(IS_2013$Population)), 		#DTB
                         IS_2013$GTF, 		#GTF
                         rep("2013", length(IS_2013$Population)), 		#Year
                         IS_2013$X1stFlwrDate, 		#FlowDate
                         IS_2013$Planted,      #PlantDate
                         IS_2013$Germ,		#GermDate
                         (IS_2013$X1stFlwrHt_mm / 100),		#HeightFFcm
                         rep("No", length(IS_2013$Population)),		#Vernalized
                         rep(NA, length(IS_2013$Population)),		#DaysVern
                         rep(NA, length(IS_2013$Population)))		#TimesVern
                                                  
lalesub <- data.frame(rep("Lale05", length(lale2005$population)),		#Experiment
                      paste(lale2005$family, lale2005$sibling, sep=""),		#ID
                      rep("field", length(lale2005$population)), 		#GrowthEnvironment
                      lale2005$population,		#Pop
                      lale2005$PTF, 		#PTF
                      rep(NA, length(lale2005$population)), 		#DTB
                      lale2005$DaysGermToFlwr, 		#GTF
                      rep("2005", length(lale2005$population)), 		#Year
                      lale2005$First_FlowerDate,		#FlowDate
                      lale2005$Planting_Date,      #PlantDate
                      lale2005$Germination_Date,		#GermDate
                      (lale2005$firstFlwrHt_mm / 100),		#HeightFFcm
                      ifelse(lale2005$First_FlowerDate > as.POSIXct("12/31/05", format="%m/%d/%y"), "Yes", "No"),		#Vernalized
                      ifelse(lale2005$First_FlowerDate > as.POSIXct("12/31/05", format="%m/%d/%y"), 150, NA),  #Nov 16- Apr 15		#DaysVern
                      ifelse(lale2005$First_FlowerDate > as.POSIXct("12/31/05", format="%m/%d/%y"), 1, NA))		#TimesVern
	
p2012sub <- data.frame(rep("field2012", length(plants2012$Code)),		#Experiment
                       paste(plants2012$Row, plants2012$Column, sep=""),		#ID
                       rep("field", length(plants2012$Code)),		#GrowthEnvironment
                       plants2012$Code,		#Pop
                       plants2012$PTF, 		#PTF
                       plants2012$Days_Germ_to_Bolt, 		#DTB
                       plants2012$Days_Germ_to_Flow, 		#GTF
                       rep("2012", length(plants2012$Code)), 		#Year
                       plants2012$Flowering_Date, 		#FlowDate
                       plants2012$Planting_Date,     #PlantDate
                       plants2012$Germ_Date,		#GermDate
                       plants2012$Height_1st_Flower_cm,		#HeightFFcm
                       ifelse(plants2012$Flowering_Date > as.POSIXct("12/31/12", format="%m/%d/%y"), "Yes", "No"),		#Vernalized
                       ifelse(plants2012$Flowering_Date > as.POSIXct("12/31/12", format="%m/%d/%y"), 168, NA), #Oct 29 - Apr 15		#DaysVern
                       ifelse(plants2012$Survive_winter=="No", NA, 1))		#TimesVern
	
qst2003sub <- data.frame(rep("qst2003", length(qst_2003$Pop)),		#Experiment
                         as.factor(qst_2003$UniqueID),		#ID
                         rep("greenhouse", length(qst_2003$Pop)),		#GrowthEnvironment
                         qst_2003$Pop,		#Pop
                         qst_2003$PlantToFlower,		#PTF
                         rep(NA, length(qst_2003$Pop)),		#DTB
                         qst_2003$GermToFlwr,		#GTF
                         rep("2003", length(qst_2003$Pop)),		#Year
                         (qst_2003$PlantingDate + (qst_2003$PlantToFlower * 60^2 * 24)),		#FlowDate
                         qst_2003$PlantingDate,    #PlantDate
                         (qst_2003$PlantingDate + (qst_2003$DaysToGerm * 60^2 * 24)),		#GermDate
                         qst_2003$X1stFlwrHgt_cm,		#HeightFFcm
                         ifelse(qst_2003$NumVernalz > 0, "Yes", "No"),		#Vernalized
                         qst_2003$DaysVernalized,		#DaysVern
                         qst_2003$NumVernalz)		#TimesVern

qst2004sub <- data.frame(rep("qst2004", length(qst_2004$Pop)),		#Experiment
                         as.factor(qst_2004$ID),		#ID
                         rep("greenhouse", length(qst_2004$Pop)),		#GrowthEnvironment
                         qst_2004$Pop,		#Pop
                         qst_2004$plant2Flwr,		#PTF
                         rep(NA, length(qst_2004$Pop)),		#DTB
                         qst_2004$germ2flwr,		#GTF
                         rep("2004", length(qst_2004$Pop)),		#Year
                         (qst_2004$plantingDate + (qst_2004$plant2Flwr * 60^2 * 24)),		#FlowDate
                         qst_2004$plantingDate,    #PlantDate
                         (qst_2004$plantingDate + (qst_2004$germTime * 60^2 * 24)),		#GermDate
                         qst_2004$X1stFlwrHgt_cm,		#HeightFFcm
                         ifelse(qst_2004$NumVernal > 0, "Yes", "No"),		#Vernalized
                         qst_2004$DaysVernal,		#DaysVern
                         qst_2004$NumVernal)		#TimesVern


s2013sub <- data.frame(rep("field2013", length(spring2013$Name)),		#Experiment
                       paste(spring2013$NS_Row, spring2013$WE_Row, sep=""),		#ID
                       rep("field", length(spring2013$Name)), 		#GrowthEnvironment
                       spring2013$Name, 		#Pop
                       spring2013$PTF,		#PTF
                       spring2013$DTB, 		#DTB
                       spring2013$DTF, 		#GTF
                       rep("2013", length(spring2013$Name)), 		#Year
                       spring2013$FD, 		#FlowDate
                       spring2013$PD,     #PlantDate
                       spring2013$GD,		#GermDate
                       spring2013$Height_1st_Flower_cm,		#HeightFFcm
                       rep("No", length(spring2013$Name)), # Oct 24 - Apr 6		#Vernalized
                       rep(NA, length(spring2013$Name)),		#DaysVern
                       rep(NA, length(spring2013$Name)))		#TimesVern
                        

sum2010sub <- data.frame(rep("sum2010", length(sum2010$Pop)),		#Experiment
                         paste(sum2010$Pop, sum2010$Plant, sep=""),		#ID
                         rep("greenhouse", length(sum2010$Pop)),		#GrowthEnvironment
                         sum2010$Pop,		#Pop
                         sum2010$DaysToFlwr,		#PTF
                         rep(NA, length(sum2010$Pop)),		#DTB
                         round(sum2010$X1stflwr - sum2010$germDate),		#GTF
                         rep("2010", length(sum2010$Pop)),		#Year
                         sum2010$X1stflwr,		#FlowDate
                         sum2010$planted,     #PlantDate
                         sum2010$germDate,		#GermDate
                         rep(NA, length(sum2010$Pop)),		#HeightFFcm
                         ifelse(sum2010$DaysVernal > 0, "Yes", "No"),		#Vernalized
                         sum2010$DaysVernal,		#DaysVern
                         sum2010$NumVernaliz)		#TimesVern


## ----Parsing, echo=FALSE-------------------------------------------------
ParseData <- function( mydata ) {
  names( mydata ) <- columnnames
  mydata$Pop <- factor( mydata$Pop)
  mydata$Pop <- as.factor(as.character(droplevels(mydata$Pop)))
  mydata$Flowered <- ifelse(mydata$PTF > 0, "Yes", "No")
  mydata$Flowered[is.na(mydata$Flowered)] <- "No"
  mydata$FlowWoVern <- ifelse(mydata$PTF > 0 & mydata$Vernalized == "No", "Yes",
                              ifelse(mydata$PTF > 0 & mydata$Vernalized == "Yes", "No", NA))
  mydata$PTF <- round(mydata$PTF)
  mydata$DTB <- round(mydata$DTB)
  mydata$GTF <- round(mydata$GTF)
  mydata$Seedstock <- ifelse(  (mydata$Experiment == "IS2013" & mydata$Pop == "MAES") | 
                                       (mydata$Experiment == "Lale05" & mydata$Pop != "PBFR") |
                                       (mydata$Experiment == "qst2004"), "Greenhouse",      
                                          "Field")
  mydata$GermLocation <-   as.factor(ifelse(mydata$Experiment == "gh2006" | mydata$Experiment == "field2013", "InField", "InGH" ))

  mydata
}
cult_2013sub <- ParseData( cult_2013sub )
gh2006sub <- ParseData(gh2006sub)
IS_2013sub <- ParseData(IS_2013sub)
lalesub <- ParseData(lalesub)
p2012sub <- ParseData(p2012sub)
qst2003sub <- ParseData(qst2003sub)
qst2004sub <- ParseData(qst2004sub)
s2013sub <- ParseData(s2013sub)
sum2010sub <- ParseData(sum2010sub)


## ----Combine-------------------------------------------------------------
CombinedDataSet <- data.frame(rbind(cult_2013sub, gh2006sub, IS_2013sub, lalesub, p2012sub, qst2003sub, qst2004sub, s2013sub, sum2010sub))


## ----Assign1-------------------------------------------------------------
SubSpeciesList <- c(ADOL="sativus", AROL="sativus", CBBG="sativus", COOL="sativus", DAJO="sativus", ESNK="sativus", MABG="sativus", MYJO="sativus", NELO="sativus", OIBG="sativus", RABG="sativus", RACA="sativus", SPEU="sativus", TOBG="sativus", WMBG="sativus", AFFR="raphanistrum", BBCA="CAHybrid", BINY="raphanistrum", GSCA="CAHybrid", MAES="raphanistrum", PBFR="landra", SAES="maritimus", DEES="raphanistrum", GHIL="raphanistrum", HCES="raphanistrum", HMES="raphanistrum", HZIL="raphanistrum", IMES="raphanistrum", ZYIL="raphanistrum", AUFI="raphanistrum", COAU="raphanistrum", KAMI="raphanistrum", AL="raphanistrum", CBES="maritimus", GMIL="rostratus", M3AU="raphanistrum", N3="raphanistrum", PG6="raphanistrum", REIL="raphanistrum", MAFI="raphanistrum", NAAU="raphanistrum", WEAU="raphanistrum", CGBC="sativus", FGBC="sativus", FRSI="sativus", LBBC="sativus", MBBC="sativus", NTJO="sativus", PABS="sativus", RABS="sativus", RBBC="sativus", SPNK="sativus", NZIL="confusus", TYIL="raphanistrum")


## ----Assign2, echo=FALSE-------------------------------------------------

SpeciesList <- c(ADOL="sativus", AROL="sativus", CBBG="sativus", COOL="sativus", DAJO="sativus", ESNK="sativus", MABG="sativus", MYJO="sativus", NELO="sativus", OIBG="sativus", RABG="sativus", RACA="sativus", SPEU="sativus", TOBG="sativus", WMBG="sativus", AFFR="raphanistrum", BBCA="CAHybrid", BINY="raphanistrum", GSCA="CAHybrid", MAES="raphanistrum", PBFR="raphanistrum", SAES="raphanistrum", DEES="raphanistrum", GHIL="raphanistrum", HCES="raphanistrum", HMES="raphanistrum", HZIL="raphanistrum", IMES="raphanistrum", ZYIL="raphanistrum", AUFI="raphanistrum", COAU="raphanistrum", KAMI="raphanistrum", AL="raphanistrum", CBES="raphanistrum", GMIL="rostratus", M3AU="raphanistrum", N3="raphanistrum", PG6="raphanistrum", REIL="raphanistrum", MAFI="raphanistrum", NAAU="raphanistrum", WEAU="raphanistrum", CGBC="sativus", FGBC="sativus", FRSI="sativus", LBBC="sativus", MBBC="sativus", NTJO="sativus", PABS="sativus", RABS="sativus", RBBC="sativus", SPNK="sativus", NZIL="confusus", TYIL="raphanistrum")

GeographyList <- c(ADOL="NA", AROL="NA", CBBG="NA", COOL="NA", DAJO="NA", ESNK="NA", MABG="NA", MYJO="NA", NELO="NA", OIBG="NA", RABG="NA", RACA="NA", SPEU="NA", TOBG="NA", WMBG="NA", AFFR="west", BBCA="nonNative", BINY="nonNative", GSCA="nonNative", MAES="west", PBFR="west", SAES="west", DEES="west", GHIL="east", HCES="west", HMES="west", HZIL="east", IMES="west", ZYIL="east", AUFI="nonNative", COAU="nonNative", KAMI="nonNative", AL="nonNative", CBES="west", GMIL="rostratus", M3AU="nonNative", N3="nonNative", PG6="nonNative", REIL="east", MAFI="nonNative", NAAU="nonNative", WEAU="nonNative", CGBC="NA", FGBC="NA", FRSI="NA", LBBC="NA", MBBC="NA", NTJO="NA", PABS="NA", RABS="NA", RBBC="NA", SPNK="NA", NZIL="confusus", TYIL="east")

TaxonomyList <- c(ADOL="oleifera", AROL="oleifera", CBBG="european", COOL="oleifera", DAJO="european", ESNK="european", MABG="caudatus", MYJO="daikon", NELO="daikon", OIBG="oleifera", RABG="caudatus", RACA="caudatus", SPEU="european", TOBG="daikon", WMBG="daikon", AFFR="raphanistrum", BBCA="CAHybrid", BINY="raphanistrum", GSCA="CAHybrid", MAES="raphanistrum", PBFR="landra", SAES="maritimus", DEES="raphanistrum", GHIL="raphanistrum", HCES="raphanistrum", HMES="raphanistrum", HZIL="raphanistrum", IMES="raphanistrum", ZYIL="raphanistrum", AUFI="raphanistrum", COAU="raphanistrum", KAMI="raphanistrum", AL="raphanistrum", CBES="maritimus", GMIL="rostratus", M3AU="raphanistrum", N3="raphanistrum", PG6="raphanistrum", REIL="raphanistrum", MAFI="raphanistrum", NAAU="raphanistrum", WEAU="raphanistrum", CGBC="daikon", FGBC="daikon", FRSI="european", LBBC="european", MBBC="european", NTJO="european", PABS="european", RABS="european", RBBC="european", SPNK="european", NZIL="confusus", TYIL="raphanistrum")

HabitatList <- c(ADOL="cultivar", AROL="cultivar", CBBG="cultivar", COOL="cultivar", DAJO="cultivar", ESNK="cultivar", MABG="cultivar", MYJO="cultivar", NELO="cultivar", OIBG="cultivar", RABG="cultivar", RACA="cultivar", SPEU="cultivar", TOBG="cultivar", WMBG="cultivar", AFFR="agricultural", BBCA="natural", BINY="agricultural", GSCA="natural", MAES="disturbed", PBFR="natural", SAES="natural", DEES="natural", GHIL="disturbed", HCES="disturbed", HMES="disturbed", HZIL="disturbed", IMES="disturbed", ZYIL="disturbed", AUFI="agricultural", COAU="agricultural", KAMI="agricultural", AL="agricultural", CBES="natural", GMIL="natural", M3AU="agricultural", N3="agricultural", PG6="agricultural", REIL="disturbed", MAFI="disturbed", NAAU="agricultural", WEAU="agricultural", CGBC="cultivar", FGBC="cultivar", FRSI="cultivar", LBBC="cultivar", MBBC="cultivar", NTJO="cultivar", PABS="cultivar", RABS="cultivar", RBBC="cultivar", SPNK="cultivar", NZIL="natural", TYIL="disturbed")

SpecificOriginList <- c(ADOL="MSU", AROL="MSU", CBBG="BountifulGardens", COOL="MSU", DAJO="JohnScheepers", ESNK="NKLawn", MABG="BountifulGardens", MYJO="JohnScheepers", NELO="JohnScheepers", OIBG="BountifulGardens", RABG="BountifulGardens", RACA="California", SPEU="unknown", TOBG="BountifulGardens", WMBG="BountifulGardens", AFFR="France", BBCA="California", BINY="NewYork", GSCA="California", MAES="Spain", PBFR="France", SAES="Spain", DEES="Spain", GHIL="Israel", HCES="Spain", HMES="Spain", HZIL="Israel", IMES="Spain", ZYIL="Israel", AUFI="Finland", COAU="Australia", KAMI="Michigan", AL="Australia", CBES="Spain", GMIL="Israel", M3AU="Australia", N3="Australia", PG6="Australia", REIL="Israel", MAFI="Finland", NAAU="Australia", WEAU="Australia", CGBC="BakersCreek", FGBC="BakersCreek", FRSI="SeedsItaly", LBBC="BakersCreek", MBBC="BakersCreek", NTJO="JohnScheepers", PABS="Burpees", RABS="Burpees", RBBC="BakersCreek", SPNK="NKLawn", NZIL="Israel", TYIL="Israel")

RegionOriginList <- c(ADOL="oleifera", AROL="oleifera", CBBG="european", COOL="oleifera", DAJO="european", ESNK="european", MABG="caudatus", MYJO="daikon", NELO="daikon", OIBG="oleifera", RABG="caudatus", RACA="caudatus", SPEU="european", TOBG="daikon", WMBG="daikon", AFFR="France", BBCA="California", BINY="NewYork", GSCA="California", MAES="Spain", PBFR="France", SAES="Spain", DEES="Spain", GHIL="Israel", HCES="Spain", HMES="Spain", HZIL="Israel", IMES="Spain", ZYIL="Israel", AUFI="Finland", COAU="Australia", KAMI="Michigan", AL="Australia", CBES="Spain", GMIL="Israel", M3AU="Australia", N3="Australia", PG6="Australia", REIL="Israel", MAFI="Finland", NAAU="Australia", WEAU="Australia", CGBC="daikon", FGBC="daikon", FRSI="european", LBBC="european", MBBC="european", NTJO="european", PABS="european", RABS="european", RBBC="european", SPNK="european", NZIL="Israel", TYIL="Israel")


## ----Assign3, echo=FALSE-------------------------------------------------
#This section uses the assignment lists created above to make new columns based on CombinedDataSet$Pop, and calculates the day of year (1-365) for planting and germination day

CombinedDataSet$Geography <- as.factor(GeographyList[CombinedDataSet$Pop])
CombinedDataSet$Taxonomy <- as.factor(TaxonomyList[CombinedDataSet$Pop])
CombinedDataSet$SubSpecies <- SubSpeciesList[CombinedDataSet$Pop]
CombinedDataSet$Species <- as.factor(SpeciesList[CombinedDataSet$Pop])
CombinedDataSet$Habitat <- as.factor(HabitatList[CombinedDataSet$Pop])
CombinedDataSet$SpecificOrigin <- as.factor(SpecificOriginList[CombinedDataSet$Pop])
CombinedDataSet$RegionOrigin <- as.factor(RegionOriginList[CombinedDataSet$Pop])
CombinedDataSet$Seedstock[CombinedDataSet$SubSpecies == "sativus"] <- "Nursery"
CombinedDataSet$SubSpecies <- as.factor(CombinedDataSet$SubSpecies)
CombinedDataSet$Seedstock <- as.factor(CombinedDataSet$Seedstock)
CombinedDataSet$Flowered <- as.factor(CombinedDataSet$Flowered)
CombinedDataSet$FlowWoVern <- as.factor(CombinedDataSet$FlowWoVern)
CombinedDataSet$DOYP <- strftime(CombinedDataSet$PlantDate, format = "%j")
CombinedDataSet$DOYP <- as.numeric(CombinedDataSet$DOYP)
CombinedDataSet$DOYG <- strftime(CombinedDataSet$GermDate, format = "%j")
CombinedDataSet$DOYG <- as.numeric(CombinedDataSet$DOYG)
CombinedDataSet$DOYGP <- ifelse(is.na(CombinedDataSet$DOYG), CombinedDataSet$DOYP, CombinedDataSet$DOYG)
CombinedDataSet$DaysVernNoNA <- CombinedDataSet$DaysVern
CombinedDataSet$DaysVernNoNA[is.na(CombinedDataSet$DaysVernNoNA)] <- 0
CombinedDataSet$FlowDate <- strftime(CombinedDataSet$FlowDate, format="%Y-%m-%d" )





## ----write---------------------------------------------------------------
write.csv(CombinedDataSet, "CombinedDataSet.csv", quote=F, row.names=F)


## ----Attach, echo=FALSE--------------------------------------------------
str(CombinedDataSet)

attach(CombinedDataSet)


## ----Levels--------------------------------------------------------------
length(levels(Pop))
levels(Geography)
levels(Taxonomy)
levels(SubSpecies)
levels(Species)
levels(Habitat)
levels(SpecificOrigin)
levels(RegionOrigin)



## ----Tables--------------------------------------------------------------
table(Pop)
table(SubSpecies)
table(Experiment)


## ----Plots1, echo=FALSE--------------------------------------------------
par(las=2, cex.axis=.7)
plot(Pop, PTF)


## ----Plots2, echo=FALSE--------------------------------------------------
par(las=2, cex.axis=.6)
plot(Pop)


