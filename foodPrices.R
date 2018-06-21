library(zoo)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
WFPVAM_FoodPrices_05_12_2017 <- read_csv("~/Downloads/WFPVAM_FoodPrices_05-12-2017.csv")
syriaFoodPrices <- subset(WFPVAM_FoodPrices_05_12_2017,adm0_name=="Syrian Arab Republic")
syriaFoodPrices$Date <- as.yearmon(paste(syriaFoodPrices$mp_year, syriaFoodPrices$mp_month), "%Y %m")
rice <- subset(syriaFoodPrices,cm_name=="Rice")
ggplot(rice, aes(x=Date, y=mp_price)) + geom_line()
#ggplot(syriaFoodPrices, aes(x=Date, y=mp_price,colour=cm_name)) + geom_line()
plot(rice$Date,rice$mp_price)
plot(riceWFP$Date,riceWFP$mp_price)

test <- rice %>% 
  group_by(Date) %>%
  summarize(median = median(mp_price))
  
plotDF <- syriaFoodPrices %>%
  group_by(cm_name,Date) %>%
  summarize(median = median(mp_price))

plotDF <- subset(plotDF, cm_name != "Livestock (sheep, two-year-old male)")

#plot by food types over time
ggplot(plotDF,aes(x=Date,y=median,colour=cm_name)) + geom_line() + geom_point()

byRegion <- syriaFoodPrices %>%
  group_by(Date,adm1_name) %>%
  summarize(median = median(mp_price))

#food prices for all governorates
ggplot(byRegion,aes(x=Date,y=median,colour=adm1_name)) + geom_point() + geom_line()


byRegion %>% group_by(adm1_name) %>%
  summarize(medmed = median(median)) #deir ezzor is the outlier region

#Siege of Deir ez-Zor (2014–17) imposed by ISIL
byRegion2 <- subset(byRegion,adm1_name != "Deir Ezzor")
ggplot(byRegion2,aes(x=Date,y=median,colour=adm1_name)) + geom_point() + geom_line()

medianRegions <- syriaFoodPrices %>%
  group_by(Date) %>%
  summarize(median = median(mp_price))

medianRegions16 <- subset(medianRegions,as.numeric(Date)>=2016)

totalFlow <- c(270075,275122,125764,149916,128847,128628,164913,232011,
               163784,94540,133677,181052,112789,200064,304766,262176,
               253150,130265,121790,137997,265078)
medianRegions16$flow <- scale(totalFlow)
medianRegions16$median <- scale(medianRegions16$median)
medianRegions16 <- melt(medianRegions16, id.vars="Date")
ggplot(medianRegions16,aes(x=Date,y=value,colour=variable)) + geom_point() + geom_line()


aleppo <- subset(byRegion,adm1_name == "Aleppo")
ggplot(aleppo,aes(x=Date,y=median)) + geom_point() + geom_line()
aleppoFlow <- c( 134951,93867,26346,78044,30378,52616,51335,105490,
                 41070,28030,57902,121720,32319,111572,155397,38800,
                 44658,29349,22960,26927,31007)
aleppo16 <- subset(aleppo,as.numeric(Date)>=2016)
aleppo16$flow <- scale(aleppoFlow)
aleppo16$median <- scale(aleppo16$median)
aleppo16 <- melt(aleppo16,id.vars=c("Date","adm1_name"))
ggplot(aleppo16,aes(x=Date,y=value,colour=variable)) + geom_point() + geom_line()
originDestinationDataCleaned <- read_delim("~/research/basu/migrationPatterns/originDestinationDataCleaned.csv",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)

originData <- originDestinationDataCleaned %>%
  group_by(M_Date,Origin) %>%
  summarize(outFlow = sum(M_Size))

originData$Date <- as.yearmon(originData$M_Date, "%m/%d/%y")
originData$Origin[originData$Origin=="Al-Hasakeh"] <- "Hassakeh"
originData$Origin[originData$Origin=="Ar-Raqqa"] <- "Raqqa"
originData$Origin[originData$Origin=="Damascus"] <- "City-Damascus"
originData$Origin[originData$Origin=="Dar'a"] <- "Daraa"
originData$Origin[originData$Origin=="Deir-ez-Zor"] <- "Deir Ezzor"
originData$Origin[originData$Origin=="Idleb"] <- "Edlib"
originData$Origin[originData$Origin=="Rural Damascus"] <- "Damascus"
originData$Origin[originData$Origin=="As-Sweida"] <- "As_Suweida"
originData$Origin[originData$Origin=="Quneitra"] <- "Al-Qunaytirah"
originData <- subset(originData,Origin!="Unknown")
originData$M_Date <- NULL


ggplot(originData,aes(x=Date,y=outFlow,colour=Origin)) + geom_point() + geom_line() #outflow for all governorates



originAleppo <- subset(originData,Origin=="Aleppo")
aleppo16 <- subset(aleppo,as.numeric(Date)>=2016)
ggplot(originAleppo,aes(x=Date,y=outFlow)) + geom_point() + geom_line()

#split "median" into food, wages, fuel, etc.
byRegionFoodType <- syriaFoodPrices
byRegionFoodType$cm_name[byRegionFoodType$cm_name=="Fuel (diesel)"] <- "Fuel"
byRegionFoodType$cm_name[byRegionFoodType$cm_name=="Fuel (gas)"] <- "Fuel"
byRegionFoodType$cm_name[(byRegionFoodType$cm_name!= "Fuel" & byRegionFoodType$cm_name != "Wage (non-qualified labour)")] <- "Food"

byRegionFoodType <- byRegionFoodType %>%
  group_by(Date,adm1_name,cm_name) %>%
  summarize(median = median(mp_price))

byRegionFoodType <- dcast(byRegionFoodType, Date+adm1_name ~ cm_name,measure.vars="median")
byRegionFoodType16 <- subset(byRegionFoodType,as.numeric(Date)>=2016)

agg <- merge(byRegionFoodType,originData,by.x = c("Date","adm1_name"),by.y = c("Date","Origin"))
startdate <- agg$Date[100]
agg$NumMonths  <- difftime(agg$Date,startdate ,units="weeks")+4
colnames(agg)[5] <- "Wage"

library(lme4)
model1 <- lmer(outFlow~Food + (NumMonths | adm1_name),data=agg)
model2 <- lmer(outFlow~Food+Fuel+Wage+NumMonths+(NumMonths|adm1_name),data=agg,REML = F)
meanz <- rep(mean(agg$outFlow),275)
rmse(predict(model2,agg),agg$outFlow)
rmse(meanz,agg$outFlow)



originDestinationDataCleaned$Date <- as.yearmon(originDestinationDataCleaned$M_Date, "%m/%d/%y")
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Al-Hasakeh"] <- "Hassakeh"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Ar-Raqqa"] <- "Raqqa"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Damascus"] <- "City-Damascus"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Dar'a"] <- "Daraa"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Deir-ez-Zor"] <- "Deir Ezzor"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Idleb"] <- "Edlib"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Rural Damascus"] <- "Damascus"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="As-Sweida"] <- "As_Suweida"
originDestinationDataCleaned$Origin[originDestinationDataCleaned$Origin=="Quneitra"] <- "Al_Qunaytirah"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Al-Hasakeh"] <- "Hassakeh"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Ar-Raqqa"] <- "Raqqa"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Damascus"] <- "City-Damascus"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Dar'a"] <- "Daraa"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Deir-ez-Zor"] <- "Deir Ezzor"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Idleb"] <- "Edlib"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Rural Damascus"] <- "Damascus"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="As-Sweida"] <- "As_Suweida"
originDestinationDataCleaned$Destination[originDestinationDataCleaned$Destination=="Quneitra"] <- "Al_Qunaytirah"

originDestinationDataCleaned$M_Date <- NULL

#account for zero flow between governorates
#fix this later. not all zeroes are correct.
#originDestinationDataCleaned <- originDestinationDataCleaned %>% complete(Destination,Origin,Date,fill=list(M_Size=0))
#originDestinationDataCleaned <- subset(originDestinationDataCleaned,Origin!="Unknown") #WARNING: account for this later

#these could be left joins to account for missing data from food prices
#food prices are missing for the end of 2017 and early 2018. for now we are just throwing out those days.
odAgg <- inner_join(originDestinationDataCleaned,byRegionFoodType,by = c("Origin" = "adm1_name", "Date" = "Date"))

colnames(odAgg)[5:7] <- c("OriginFood","OriginFuel","OriginWage")
odAgg <- inner_join(odAgg,byRegionFoodType,by = c("Destination" = "adm1_name", "Date" = "Date"))
colnames(odAgg)[8:10] <- c("DestFood","DestFuel","DestWage")
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

odAgg$NumMonths  <- mondf(startdate,odAgg$Date)
#odAgg$Date <- NULL
odAgg$Destination <- factor(odAgg$Destination)
odAgg$Origin <- factor(odAgg$Origin)

library(caret)



#importing conflict data from ICEWS
dataverseConflicts2016 <- read_delim("~/research/basu/migrationPatterns/dataverseConflicts2016.tab", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
dataverseSyria <- subset(dataverseConflicts2016,(dataverseConflicts2016$`Target Country`=="Syria" & dataverseConflicts2016$`Country`=="Syria"))
#dataverseSyria2 <- subset(dataverseConflicts2016,dataverseConflicts2016$`Country`=="Syria")

intenseICEWS <- subset(dataverseSyria,Intensity<=-9)

intenseICEWS <- subset(intenseICEWS,!is.na(Province))

intenseICEWS$Province[intenseICEWS$Province=="Muhafazat al Qunaytirah"] <- "Al_Qunaytirah"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Hamah"] <- "Hama"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Hims"] <- "Homs"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Rif Dimashq"] <- "Damascus"
intenseICEWS$Province[intenseICEWS$Province=="Ar Raqqah"] <- "Raqqa"
intenseICEWS$Province[intenseICEWS$Province=="Dimashq"] <- "City-Damascus"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Halab"] <- "Aleppo"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Dar`a"] <- "Daraa"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat al Ladhiqiyah"] <- "Lattakia"
intenseICEWS$Province[intenseICEWS$Province=="Idlib"] <- "Edlib"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat al Hasakah"] <- "Hassakeh"
intenseICEWS$Province[intenseICEWS$Province=="Dayr az Zawr"] <- "Deir Ezzor"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat as Suwayda'"] <- "As_Suweida"
intenseICEWS$Province[intenseICEWS$Province=="Golan Heights"] <- "Al_Qunaytirah"
intenseICEWS$Province[intenseICEWS$Province=="Muhafazat Tartus"] <- "Tartous"
intenseICEWS$Province[intenseICEWS$Province=="Qasr"] <- "Edlib"
intenseICEWS$Province <- factor(intenseICEWS$Province)

intenseICEWS$Date <- as.yearmon(intenseICEWS$`Event Date`, "%y/%m/%d")

conflictICEWS <- intenseICEWS %>%
  complete(Date,Province) %>%
  group_by(Date,Province) %>%
  summarize(conflict = sum(!is.na(Country)))

conflictICEWS$conflictScaled <- scale(conflictICEWS$conflict)

syriaConflictsACLED <- read_csv("~/research/basu/migrationPatterns/syriaConflictsACLED.csv")
syriaConflictsACLED <- syriaConflictsACLED[-1,]
syriaConflictsACLED <- subset(syriaConflictsACLED,country=="Syria")

syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Dar'a"] <- "Daraa"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Damascus"] <- "City-Damascus"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Rural Damascus"] <- "Damascus"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Idleb"] <- "Edlib"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Deir-ez-Zor"] <- "Deir Ezzor"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Al-Hasakeh"] <- "Hassakeh"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="As-Sweida"] <- "As_Suweida"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Ar-Raqqa"] <- "Raqqa"
syriaConflictsACLED$admin1[syriaConflictsACLED$admin1=="Quneitra"] <- "Al_Qunaytirah"
syriaConflictsACLED$admin1 <- factor(syriaConflictsACLED$admin1)
syriaConflictsACLED$Date <- as.yearmon(syriaConflictsACLED$event_date, "%Y-%m-%d")

conflictACLED <- syriaConflictsACLED %>%
  complete(Date,admin1) %>%
  group_by(Date,admin1) %>%
  summarize(conflict = sum(!is.na(year)))

conflictACLED$conflictScaled <- scale(conflictACLED$conflict)
colnames(conflictACLED)[2] <- "Province"

conflictDF <- rbind(conflictICEWS,conflictACLED)

conflictDF$doubleScaled <- scale(conflictDF$conflictScaled)

conflictDF$Date <- as.yearmon(conflictDF$Date)

conflictDFSmall <- conflictDF
conflictDFSmall$conflictScaled <- NULL
conflictDFSmall$conflict <- NULL

conflictAgg <- left_join(odAgg,conflictDFSmall,by = c("Origin" = "Province", "Date" = "Date"))
colnames(conflictAgg)[12] <- "OriginConflict"
conflictAgg <- inner_join(conflictAgg,conflictDFSmall,by=c("Destination" = "Province","Date" = "Date"))
colnames(conflictAgg)[13] <- "DestConflict"
conflictAgg$Destination <- as.character(conflictAgg$Destination)
conflictAgg$Origin <- as.character(conflictAgg$Origin)
conflictAgg$Destination[conflictAgg$Destination=="Damascus"] <- "Rural Damascus"
conflictAgg$Destination[conflictAgg$Destination=="City-Damascus"] <- "Damascus"
conflictAgg$Origin[conflictAgg$Origin=="Damascus"] <- "Rural Damascus"
conflictAgg$Origin[conflictAgg$Origin=="City-Damascus"] <- "Damascus"
conflictAgg$Destination <- factor(conflictAgg$Destination)
conflictAgg$Origin <- factor(conflictAgg$Origin)

#conflictAgg$Date <- NULL #remove this later
conflictAgg$logFlow <- log(conflictAgg$M_Size+1)
enet3 <- train(logFlow ~ .-M_Size, data = conflictAgg, 
               method = "enet") #0.4969678 on predict, not resampled
lmConflictAgg1 <- lm(logFlow~.-M_Size,data=conflictAgg) #0.2507
bayesglm <- train(logFlow ~ .-M_Size, data = conflictAgg, 
                method = "bayesglm") #0.203
xgbTree <- train(logFlow ~ .-M_Size, data = conflictAgg, 
                  method = "xgbTree") #0.5474254 resampled, 0.8938031 not resampled

ctrl <- trainControl(method = "cv",
                     number=10)
xgbTreeCV <- train(logFlow ~ .-M_Size-Date, data = conflictAgg,
                   method="xgbTree",trControl=ctrl) #0.5977326 w/ CV, 0.9124592 not resampled
plot(predict(xgbTreeCV,conflictAgg),conflictAgg$logFlow)
xgbTreeCV2 <- train(logFlow ~ .-M_Size-Date, data = conflictAgg,
                   method="xgbTree",trControl=ctrl)

coordinates <- read_delim("~/research/basu/migrationPatterns/coordinates.txt", 
                          +     " ", escape_double = FALSE, trim_ws = TRUE)

coordinates$country <- factor(levels(conflictAgg$Origin))
coordAgg <- inner_join(conflictAgg,coordinates,by=c("Origin" = "country"))
colnames(coordAgg)[15:16] <- c("OriginLon","OriginLat")
coordAgg <- inner_join(coordAgg,coordinates,by=c("Destination" = "country"))
colnames(coordAgg)[17:18] <- c("DestLon","DestLat")
library(geosphere)
distance <- distHaversine(cbind(coordAgg$OriginLon,coordAgg$OriginLat), cbind(coordAgg$DestLon, coordAgg$DestLat))
coordAgg$dist <- distance

coordxgbTreeCV <- train(logFlow ~ .-M_Size-Date-OriginLon-OriginLat-DestLon-DestLat, data = coordAgg,
                    method="xgbTree",trControl=ctrl)
plot(predict(coordxgbTreeCV,coordAgg),coordAgg$logFlow)
coordPreds <- predict(coordxgbTreeCV,coordAgg)
coordPreds[coordPreds <= 0] <- 0

dataSplit <- coordAgg[order(coordAgg$Date),]
trainCA <- dataSplit[1:1140,]
testCA <- dataSplit[1141:1297,]

trainCAxgb <- train(logFlow ~ .-M_Size-Date-OriginLon-OriginLat-DestLon-DestLat, data = trainCA,
                          method="xgbTree",trControl=ctrl)

splitPreds <- predict(trainCAxgb,testCA)
plot(splitPreds,testCA$logFlow)
cor(splitPreds,testCA$logFlow) #0.622
rmse(splitPreds,testCA$logFlow) #1.81



historicalMean <- trainCA %>%
  group_by(Destination,Origin) %>%
  summarize(histMean = log(mean(M_Size)))

testHMCA <- left_join(testCA,historicalMean)
testHMCA$histMean[is.na(testHMCA$histMean)] <- 0
plot(testHMCA$histMean,testHMCA$logFlow)
cor(testHMCA$histMean,testHMCA$logFlow) #0.609
rmse(testHMCA$histMean,testHMCA$logFlow) #1.98

locfGroup <- trainCA %>%
  group_by(Destination,Origin) %>%
  filter(NumMonths==max(NumMonths)) %>%
  arrange(Destination,Origin,logFlow)
locfGroup <- locfGroup[c("Destination","Origin","logFlow")]
colnames(locfGroup)[3] <- "locfFlow"
testLocf <- left_join(testCA,locfGroup)
testLocf$locfFlow[is.na(testLocf$locfFlow)] <- 0
plot(testLocf$locfFlow,testLocf$logFlow)
cor(testLocf$locfFlow,testLocf$logFlow) #0.715
rmse(testLocf$locfFlow,testLocf$logFlow) #2.27


dataSplit2 <- coordAgg[order(coordAgg$Date),]
dataSplit2$M_Size<-NULL
dataSplit2$Date<-NULL
dataSplit2$OriginLon<-NULL
dataSplit2$OriginLat<-NULL
dataSplit2$DestLon<-NULL
dataSplit2$DestLat<-NULL
dataSplit2[3:8] <- scale(dataSplit2[3:8])
dataSplit2$dist <- scale(dataSplit2$dist)
trainCA2 <- dataSplit2[1:1140,]
testCA2 <- dataSplit2[1141:1297,]
trainCAglmm <- lmer(logFlow~OriginFood+OriginFuel+OriginWage+DestFood+DestFuel+
                      DestWage+OriginConflict+DestConflict+dist+NumMonths+(NumMonths|Destination/Origin),data=trainCA2,REML = F)
glmmPreds <- predict(trainCAglmm,testCA2,allow.new.levels = TRUE)
plot(glmmPreds,testCA2$logFlow)
cor(glmmPreds,testCA2$logFlow) #0.728
rmse(glmmPreds,testCA2$logFlow) #1.56

trainCAglmm2 <- lmer(logFlow~OriginFood+OriginFuel+OriginWage+DestFood+DestFuel+
                      DestWage+OriginConflict+DestConflict+dist+NumMonths+
                       NumMonths*OriginConflict+(NumMonths|Destination/Origin),data=trainCA2,REML = F)
glmmPreds2 <- predict(trainCAglmm2,testCA2,allow.new.levels = TRUE)
rmse(glmmPreds2,testCA2$logFlow) #1.60

trainCAglmm3 <- lmer(logFlow~OriginConflict+DestConflict+dist+NumMonths+
                       (NumMonths|Destination/Origin),data=trainCA2,REML = F)
glmmPreds3 <- predict(trainCAglmm3,testCA2,allow.new.levels = TRUE)
rmse(glmmPreds3,testCA2$logFlow) #1.58

trainCAglmm4 <- lmer(logFlow~OriginConflict+DestConflict+dist+
                       (NumMonths|Destination/Origin),data=trainCA2,REML = F)
glmmPreds4 <- predict(trainCAglmm4,testCA2,allow.new.levels = TRUE)
rmse(glmmPreds4,testCA2$logFlow) #1.58

trainCAglmm5 <- lmer(logFlow~OriginFood+OriginFuel+OriginWage+DestFood+DestFuel+
                       DestWage+OriginFood*DestFood+OriginFuel*DestFuel+OriginWage*DestWage+
                       OriginConflict*DestConflict+OriginConflict+DestConflict+dist+NumMonths+
                       (NumMonths|Destination/Origin),
                     data=trainCA2,REML = F)
glmmPreds5 <- predict(trainCAglmm5,testCA2,allow.new.levels = TRUE)
rmse(glmmPreds5,testCA2$logFlow) #1.58

trainCAglmm6 <- lmer(logFlow~Destination+Origin+OriginFood+OriginFuel+OriginWage+DestFood+DestFuel+
                       DestWage+OriginFood*DestFood+OriginFuel*DestFuel+OriginWage*DestWage+
                       OriginConflict*DestConflict+OriginConflict+DestConflict+dist+NumMonths+
                       (NumMonths|Destination/Origin),
                     data=trainCA2,REML = F)
glmmPreds6 <- predict(trainCAglmm6,testCA2,allow.new.levels = TRUE)
rmse(glmmPreds6,testCA2$logFlow) #1.56

