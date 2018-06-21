library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(ggmap)
library(lme4)
library(Metrics)

originDestinationDataCleaned <- read_delim("~/research/basu/migrationPatterns/originDestinationDataCleaned.csv",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
WFPVAM_FoodPrices_05_12_2017 <- read_csv("~/Downloads/WFPVAM_FoodPrices_05-12-2017.csv")
syriaConflictsACLED <- read_csv("~/research/basu/migrationPatterns/syriaConflictsACLED.csv")
icewsConflicts2016 <- read_delim("~/research/basu/migrationPatterns/dataverseConflicts2016.tab", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
icewsConflicts2015 <- read_delim("~/research/basu/migrationPatterns/events.2015.20170206133646.tab", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)


###Food Prices
syriaFoodPrices <- subset(WFPVAM_FoodPrices_05_12_2017,adm0_name=="Syrian Arab Republic")
syriaFoodPrices$Date <- as.yearmon(paste(syriaFoodPrices$mp_year, syriaFoodPrices$mp_month), "%Y %m")

byRegionFoodType <- syriaFoodPrices
syriaFullPrices <- byRegionFoodType #added
byRegionFoodType$cm_name[byRegionFoodType$cm_name=="Fuel (diesel)"] <- "Fuel"
byRegionFoodType$cm_name[byRegionFoodType$cm_name=="Fuel (gas)"] <- "Fuel"
byRegionFoodType$cm_name[(byRegionFoodType$cm_name!= "Fuel" & byRegionFoodType$cm_name != "Wage (non-qualified labour)")] <- "Food"

byRegionFoodType <- byRegionFoodType %>%
  group_by(Date,adm1_name,cm_name) %>%
  summarize(median = median(mp_price))


syriaFullPrices <- syriaFullPrices %>%
  group_by(Date,adm1_name,cm_name) %>%
  summarize(median = median(mp_price))
syriaFullPrices <- subset(syriaFullPrices,Date>=2015)
syriaFullPrices <- dcast(syriaFullPrices,Date+adm1_name~cm_name,measure.vars="median")




byRegionFoodType <- dcast(byRegionFoodType, Date+adm1_name ~ cm_name,measure.vars="median")
byRegionFoodType <- subset(byRegionFoodType,Date>=2015)
colnames(byRegionFoodType)[5] <- "Wage"

##imputing
syriaPrices <- byRegionFoodType %>%
  complete(Date,adm1_name) %>%
  group_by(Date) %>%
  mutate(Food = ifelse(is.na(Food),median(Food,na.rm=T),Food),
         Fuel = ifelse(is.na(Fuel),median(Fuel,na.rm=T),Fuel),
         Wage = ifelse(is.na(Wage),median(Wage,na.rm=T),Wage))

syriaFullPrices <- syriaFullPrices %>%
  complete(Date,adm1_name) %>%
  group_by(Date) %>%
  mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))
syriaFullPrices <- syriaFullPrices[ , colSums(is.na(syriaFullPrices)) == 0]



###Flow data
originDestinationDataCleaned$Date <- as.yearmon(originDestinationDataCleaned$M_Date, "%m/%d/%y")
originDestinationDataCleaned$M_Date <- NULL
originDestinationDataCleaned$DataDate <- originDestinationDataCleaned$Date-.25

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

originDestinationDataCleaned <- subset(originDestinationDataCleaned,Origin!="Unknown") #Fix this later

###Conflict data

##icews data
icews1516 <- rbind(icewsConflicts2015,icewsConflicts2016)
icewsSyria <- subset(icews1516,(icews1516$`Target Country`=="Syria" & icews1516$`Country`=="Syria"))

intenseICEWS <- subset(icewsSyria,Intensity<=-9)

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

##acled data
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

###Aggregating data


odAgg <- left_join(originDestinationDataCleaned,syriaPrices,by = c("Origin" = "adm1_name", "DataDate" = "Date"))
odAgg <- na.omit(odAgg)
colnames(odAgg)[6:8] <- c("OrigFood","OrigFuel","OrigWage")
odAgg <- left_join(odAgg,syriaPrices,by = c("Destination" = "adm1_name", "DataDate" = "Date"))
colnames(odAgg)[9:11] <- c("DestFood","DestFuel","DestWage")
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

odAgg$NumMonths  <- mondf(unique(odAgg$Date)[1],odAgg$Date)
#odAgg$Date <- NULL
odAgg$Destination <- factor(odAgg$Destination)
odAgg$Origin <- factor(odAgg$Origin)

conflictAgg <- left_join(odAgg,conflictDFSmall,by = c("Origin" = "Province", "Date" = "Date"))
colnames(conflictAgg)[13] <- "OriginConflict"
conflictAgg <- left_join(conflictAgg,conflictDFSmall,by=c("Destination" = "Province","Date" = "Date"))
colnames(conflictAgg)[14] <- "DestConflict"
conflictAgg <- left_join(conflictAgg,conflictDFSmall,by = c("Origin" = "Province", "DataDate" = "Date"))
colnames(conflictAgg)[15] <- "OriginConflict3"
conflictAgg <- inner_join(conflictAgg,conflictDFSmall,by=c("Destination" = "Province","DataDate" = "Date"))
colnames(conflictAgg)[16] <- "DestConflict3"

conflictAgg$Destination <- as.character(conflictAgg$Destination)
conflictAgg$Origin <- as.character(conflictAgg$Origin)
conflictAgg$Destination[conflictAgg$Destination=="Damascus"] <- "Rural Damascus"
conflictAgg$Destination[conflictAgg$Destination=="City-Damascus"] <- "Damascus"
conflictAgg$Origin[conflictAgg$Origin=="Damascus"] <- "Rural Damascus"
conflictAgg$Origin[conflictAgg$Origin=="City-Damascus"] <- "Damascus"
conflictAgg$Destination <- factor(conflictAgg$Destination)
conflictAgg$Origin <- factor(conflictAgg$Origin)

##distances/coordinates
coordinates <- read_delim("~/research/basu/migrationPatterns/coordinates.txt", 
                          " ", escape_double = FALSE, trim_ws = TRUE)
coordinates$country <- factor(levels(conflictAgg$Origin))
coordAgg <- inner_join(conflictAgg,coordinates,by=c("Origin" = "country"))
colnames(coordAgg)[17:18] <- c("OriginLon","OriginLat")
coordAgg <- inner_join(coordAgg,coordinates,by=c("Destination" = "country"))
colnames(coordAgg)[19:20] <- c("DestLon","DestLat")
library(geosphere)
distance <- distHaversine(cbind(coordAgg$OriginLon,coordAgg$OriginLat), cbind(coordAgg$DestLon, coordAgg$DestLat))
coordAgg$dist <- distance


###Model Comparisons

##data splitting
syrSplit <- coordAgg[order(coordAgg$Date),]
syrSplit$logFlow <- log(syrSplit$M_Size)
syrSplit[6:11] <- scale(syrSplit[6:11])
syrSplit$dist <- scale(syrSplit$dist)
uniqueMonth <- unique(syrSplit$NumMonths)

##lagged values for ML models
#put in lagged flow models. impute within month by median for missing values.

# lagAgg <- syrSplit %>%
#   group_by(Destination,Origin) %>%
#   mutate(flowLag1 = lag(logFlow,order_by=Date)) %>%
#   ungroup() %>%
#   group_by(Date) %>%
#   mutate(flowLag1 = ifelse(is.na(flowLag1),log(median(M_Size)),flowLag1)) %>%
#   ungroup()

#imputes by taking historical median within groups instead of taking current median.
lagAgg <- syrSplit %>%
  group_by(Destination,Origin) %>%
  mutate(flowLag1 = lag(logFlow,order_by=NumMonths)) %>%
  ungroup() 

lagAgg <- lagAgg %>%
  group_by(NumMonths,Destination) %>%
  summarize(prevmedian = log(median(M_Size))) %>%
  mutate(prevmedian = lag(prevmedian,order_by=NumMonths)) %>%
  right_join(lagAgg) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian,flowLag1)) %>%
  ungroup()

lagAgg <- lagAgg %>%
  group_by(NumMonths,Origin) %>%
  summarize(prevmedian2 = log(median(M_Size))) %>%
  mutate(prevmedian2 = lag(prevmedian2,order_by=NumMonths)) %>%
  right_join(lagAgg) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian2,flowLag1)) %>%
  ungroup()

lagAgg <- lagAgg %>%
  group_by(NumMonths) %>%
  summarize(prevmedian3 = log(median(M_Size))) %>%
  mutate(prevmedian3 = lag(prevmedian3,order_by=NumMonths)) %>%
  right_join(lagAgg) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian3,flowLag1)) %>%
  ungroup()


syriaFullPrices$adm1_name[syriaFullPrices$adm1_name=="Damascus"] <- "Rural Damascus"
syriaFullPrices$adm1_name[syriaFullPrices$adm1_name=="City-Damascus"] <- "Damascus"
syriaFullPrices$adm1_name <- factor(syriaFullPrices$adm1_name)


lagAgg2 <- left_join(lagAgg,syriaFullPrices,by=c("Destination"="adm1_name","DataDate"="Date"))
lagAgg2 <- left_join(lagAgg2,syriaFullPrices,by=c("Origin"="adm1_name","DataDate"="Date"))
lagAgg2[22:45] <- scale(lagAgg2[22:45])
lagAgg2[15:18] <- NULL #removing lon/lat columns




##mixed effects model (should be origin/destination)
glmmAggLoop <- data_frame()
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+flowLag1+
                       OriginConflict+DestConflict+dist+NumMonths+OriginConflict3+DestConflict3+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  glmmAggLoop <- rbind(glmmAggLoop,testLoop)
}
rmse(glmmAggLoop$logFlow,glmmAggLoop$preds) #1.549256
cor(glmmAggLoop$logFlow,glmmAggLoop$preds) #0.7468824
mae(glmmAggLoop$logFlow,glmmAggLoop$preds) #1.197189


##random forest
rfAggLoop <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

library(caret)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop <- rbind(rfAggLoop,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop$logFlow,rfAggLoop$preds) #1.488771
cor(rfAggLoop$logFlow,rfAggLoop$preds) #0.7671495
mae(rfAggLoop$logFlow,rfAggLoop$preds) #1.139823

##rf high dimensional price case
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg2,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg2,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ .-Date-M_Size,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop <- rbind(rfAggLoop,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop$logFlow,rfAggLoop$preds) #1.506811
cor(rfAggLoop$logFlow,rfAggLoop$preds) #0.7601074
mae(rfAggLoop$logFlow,rfAggLoop$preds) #1.150988

##svm
svmAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  svmAggLoop <- rbind(svmAggLoop,testLoop)
}
registerDoSEQ()
rmse(svmAggLoop$logFlow,svmAggLoop$preds) #1.556459
cor(svmAggLoop$logFlow,svmAggLoop$preds) #0.7454772
mae(svmAggLoop$logFlow,svmAggLoop$preds) #1.172541

##svm high dimensional price case
svmAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg2,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg2,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ .-M_Size-Date,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  svmAggLoop <- rbind(svmAggLoop,testLoop)
}
registerDoSEQ()
rmse(svmAggLoop$logFlow,svmAggLoop$preds) #1.552393
cor(svmAggLoop$logFlow,svmAggLoop$preds) #0.7460827
mae(svmAggLoop$logFlow,svmAggLoop$preds) #1.177513


##xgbT
xgbAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  xgbAggLoop <- rbind(xgbAggLoop,testLoop)
}
registerDoSEQ()
rmse(xgbAggLoop$logFlow,xgbAggLoop$preds) #1.58782
cor(xgbAggLoop$logFlow,xgbAggLoop$preds) #0.7311744
mae(xgbAggLoop$logFlow,xgbAggLoop$preds) #1.227431

##longitudinal boostmtree
library(boostmtree)
X <- lagAgg[c("Destination","Origin","OrigFood","OrigFuel","OrigWage","DestFood","DestFuel","DestWage",
              "OriginConflict","DestConflict","OriginConflict3","DestConflict3","dist","flowLag1")]
tm <- lagAgg$NumMonths
y <- lagAgg$logFlow
id <- paste0(lagAgg$Destination,lagAgg$Origin)
bmtDF <- cbind(X,tm,y,id)
bmtDF$id2 <- as.numeric(bmtDF$id)

bootest <- boostmtree(x=bmtDF[colnames(X)],tm=bmtDF$tm,id=bmtDF$id2,y=bmtDF$y,cv.flag = F)

boopreds <- predict.boostmtree(bootest,x=bmtDF[c("Destination","Origin","OrigFood","OrigFuel","OrigWage","DestFood","DestFuel","DestWage",
                                               "OriginConflict","DestConflict","OriginConflict3","DestConflict3","dist","flowLag1")],
                               tm=bmtDF$tm,y=bmtDF$y,id=bmtDF$id2)


boopreds <- predict.boostmtree(bootest,x=bmtDF[Xvars],
                               tm=bmtDF$tm,y=bmtDF$y,id=bmtDF$id2)

bmtDFOrders <- bmtDF[order(bmtDF$id2),]
rmse(unlist(boopreds$y),bmtDFOrders$y)

boostDF <- lagAgg
boostDF$id <- paste0(boostDF$Destination,boostDF$Origin)
boostDF$id <- as.numeric(factor(boostDF$id))
mboostAggLoop <- data.frame()
for(i in 5:23) {
  trainLoop <- subset(boostDF,NumMonths <= uniqueMonth[i])
  testLoop <- subset(boostDF,NumMonths == uniqueMonth[i+1])
  testLoop$NumMonths[1] <- testLoop$NumMonths[1] + 1
  btm <- boostmtree(x=boostDF[colnames(X)],tm=boostDF$NumMonths,id=boostDF$id,y=boostDF$logFlow,cv.flag = F)
  btm.p <- predict.boostmtree(btm,x=as.data.frame(testLoop[colnames(X)]),tm=testLoop$NumMonths,id=testLoop$id,y=testLoop$logFlow)
  testLoop$preds <- unlist(btm.p$y) #should be comparing btm.p$mu and btm.p$muhat; doesn't matter anyway, results are poor.
  mboostAggLoop <- rbind(mboostAggLoop,testLoop)
}

boopreds2 <- predict.boostmtree(btm,x=bmtDF[c("Destination","Origin","OrigFood","OrigFuel","OrigWage","DestFood","DestFuel","DestWage",
                                                 "OriginConflict","DestConflict","OriginConflict3","DestConflict3","dist","flowLag1")],
                               tm=bmtDF$tm,y=bmtDF$y,id=bmtDF$id2)



for(i in 5:23) {
X <- trainLoop[c("Destination","Origin","OrigFood","OrigFuel","OrigWage","DestFood","DestFuel","DestWage",
              "OriginConflict","DestConflict","OriginConflict3","DestConflict3","dist","flowLag1")]
tm <- trainLoop$NumMonths
y <- trainLoop$logFlow
id <- paste0(trainLoop$Destination,trainLoop$Origin)
bmtDF <- cbind(X,tm,y,id)
bmtDF$id2 <- as.numeric(bmtDF$id)

bootest2 <- boostmtree(x=bmtDF[colnames(X)],tm=bmtDF$tm,id=bmtDF$id2,y=bmtDF$y,cv.flag = F)

boopreds2 <- predict.boostmtree(bootest2,x=bmtDF[c("Destination","Origin","OrigFood","OrigFuel","OrigWage","DestFood","DestFuel","DestWage",
                                                 "OriginConflict","DestConflict","OriginConflict3","DestConflict3","dist","flowLag1")],
                               tm=bmtDF$tm,y=bmtDF$y,id=bmtDF$id2)

btm.p <- predict.boostmtree(btm,x=as.data.frame(trainLoop[colnames(X)]),tm=trainLoop$NumMonths,id=trainLoop$id,y=trainLoop$logFlow)


}




##Historical Mean

hmAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = log(mean(M_Size)))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  hmAggLoop <- rbind(hmAggLoop,testLoopHM)
}

rmse(hmAggLoop$logFlow,hmAggLoop$histMean) #2.148195
cor(hmAggLoop$logFlow,hmAggLoop$histMean) #0.6194765
mae(hmAggLoop$logFlow,hmAggLoop$histMean) #1.658668


##LOCF
locfAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,logFlow)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  locfAggLoop <- rbind(locfAggLoop,testLooplocf)
}

rmse(locfAggLoop$logFlow,locfAggLoop$locfFlow) #2.011211
cor(locfAggLoop$logFlow,locfAggLoop$locfFlow) #0.6813568
mae(locfAggLoop$logFlow,locfAggLoop$locfFlow) #1.438674

## non-log flow

#glmm
# glmmAggLoop <- data_frame()
# for (i in 5:23) {
#   print(i)
#   trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
#   testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+1])
#   trainModel <- lmer(logFlow~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+
#                        OriginConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
#   testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
#   glmmAggLoop <- rbind(glmmAggLoop,testLoop)
# }
rmse(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) #10074.47
cor(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) #0.5597621
mae(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) #2370.81

rmse(svmAggLoop$M_Size,exp(svmAggLoop$preds)) #10292.38
cor(svmAggLoop$M_Size,exp(svmAggLoop$preds)) #0.5145036
mae(svmAggLoop$M_Size,exp(svmAggLoop$preds)) #2383.206

rmse(rfAggLoop$M_Size,exp(rfAggLoop$preds)) #9576.611
cor(rfAggLoop$M_Size,exp(rfAggLoop$preds)) #0.6728813
mae(rfAggLoop$M_Size,exp(rfAggLoop$preds)) #2237.728

rmse(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) #9760.461
cor(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) #0.5910114
mae(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) #2351.414


#poisson regression
poisAggLoop <- data_frame()
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- glmer(M_Size~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+flowLag1+
                       OriginConflict+DestConflict+dist+NumMonths+OriginConflict3+DestConflict3+(NumMonths|Origin/Destination),
                      data=trainLoop,REML = F,family=poisson)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  poisAggLoop <- rbind(poisAggLoop,testLoop)
}
rmse(poisAggLoop$M_Size,poisAggLoop$preds) #12211.18
cor(poisAggLoop$M_Size,poisAggLoop$preds) #0.3622831
mae(poisAggLoop$M_Size,poisAggLoop$preds) #2944.45

write.csv(poisAggLoop,here("data","poisAggLoop.csv"))

##random forest directly modeling flow
rfAggLoop2 <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

library(caret)
rfctrl=trainControl(method="none",verboseIter = TRUE)
rfGrid = expand.grid(mtry=20)


for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(M_Size ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl,tuneGrid=rfGrid)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop2 <- rbind(rfAggLoop2,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop2$M_Size,rfAggLoop2$preds) #9117.248
cor(rfAggLoop2$M_Size,rfAggLoop2$preds) #0.6459479
mae(rfAggLoop2$M_Size,rfAggLoop2$preds) #3178.28






##Historical Mean

hmAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = mean(M_Size))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  hmAggLoop <- rbind(hmAggLoop,testLoopHM)
}

rmse(hmAggLoop$M_Size,hmAggLoop$histMean) #10587.07
cor(hmAggLoop$M_Size,hmAggLoop$histMean) #0.4888199
mae(hmAggLoop$M_Size,hmAggLoop$histMean) #3066.022


##LOCF
locfAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,M_Size)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  locfAggLoop <- rbind(locfAggLoop,testLooplocf)
}

rmse(locfAggLoop$M_Size,locfAggLoop$locfFlow) #12211.19
cor(locfAggLoop$M_Size,locfAggLoop$locfFlow) #0.4051277
mae(locfAggLoop$M_Size,locfAggLoop$locfFlow) #2944.922

###three month ahead forecasting

#glmm three months ahead
glmmAggLoopF <- data_frame()
for (i in 5:20) {
  print(i)
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+3])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+
                       OriginConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  glmmAggLoopF <- rbind(glmmAggLoopF,testLoop)
}
rmse(glmmAggLoopF$M_Size,exp(glmmAggLoopF$preds)) #1.870691
cor(glmmAggLoopF$M_Size,exp(glmmAggLoopF$preds)) #0.6359486
mae(glmmAggLoopF$M_Size,exp(glmmAggLoopF$preds)) #1.462514

#historical mean three months ahead
hmAggLoopF <- data_frame()
for (i in 5:20) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+3])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = mean(M_Size))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  hmAggLoopF <- rbind(hmAggLoopF,testLoopHM)
}

rmse(hmAggLoopF$M_Size,hmAggLoopF$histMean) #11154.41
cor(hmAggLoopF$M_Size,hmAggLoopF$histMean) #0.4423951
mae(hmAggLoopF$M_Size,hmAggLoopF$histMean) #3202.353



##LOCF three months ahead
locfAggLoopF <- data_frame()
for (i in 5:20) {
  trainLoop <- subset(syrSplit,NumMonths <= uniqueMonth[i])
  testLoop <- subset(syrSplit,NumMonths == uniqueMonth[i+3])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,M_Size)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  locfAggLoopF <- rbind(locfAggLoopF,testLooplocf)
}

rmse(locfAggLoopF$M_Size,locfAggLoopF$locfFlow) #12417.77
cor(locfAggLoopF$M_Size,locfAggLoopF$locfFlow) #0.3351661
mae(locfAggLoopF$M_Size,locfAggLoopF$locfFlow) #2911.942


##coefficients
library(xtable)
syriaModelz <- lmer(logFlow~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+
                     OriginConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=syrSplit,REML = F)

summary(syriaModelz)
xtable(coef(summary(syriaModelz)))

syriaModelrf <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                      OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=lagAgg,
                    method= "rf",verbose=T,trControl=ctrl,importance=T)
syrVimp <- varImp(syriaModelrf)

#importance plot
library(forcats)
library(tibble)
syrVimp$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  labs(y = "Importance",x="Variable Name",title="Variable Importance (Syria)") +
  theme_bw() +
  theme(text = element_text(size = 18))


##sankey chart
syrSankdf <- lagAgg %>%
  group_by(Destination,Origin) %>%
  summarize(flow = sum(M_Size)) %>%
  as.data.frame()
syrSankdf$Destination <- as.character(syrSankdf$Destination)
syrSankdf$Origin <- as.character(syrSankdf$Origin)
syrSankdf$Destination <- paste0(syrSankdf$Destination," ")
#syrSankdf$Destination <- factor(syrSankdf$Destination)

library(networkD3)
name_vec <- c(sort(unique(syrSankdf$Destination)),sort(unique(syrSankdf$Origin)))

nodes <- data.frame(name = name_vec, id = 0:27)

links <- syrSankdf %>%
  left_join(nodes, by = c('Origin' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('Destination' = 'name')) %>%
  rename(dest_id = id)
# links$linkgroup = as.character(links$origin_id)
# nodes$nodegroup <- nodes$id
# nodes$nodegroup[15:28] <- nodes$nodegroup[15:28]-14
# nodes$nodegroup <- as.character(nodes$nodegroup)
sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'flow', NodeID = 'name', fontSize = 16,
              fontFamily="sans-serif",iterations=0)







###descriptive stats on data
syrSummary <- lagAgg %>%
  group_by(Destination) %>%
  mutate(prev = lag(M_Size,order_by=Date)) %>%
  summarize(n = n(),
            mean = mean(M_Size),
            median = median(M_Size),
            sd = sd(M_Size),
            max = max(M_Size),
            quantile25 = quantile(M_Size,probs=.25),
            quantile75 = quantile(M_Size,probs=.75),
            spikes = length(M_Size[M_Size >= prev+3*sd]))

syrTotal <- coordAgg %>%
  summarize(Country = "Syria",
            n = n(),
            flowMean = mean(M_Size),
            flowSD = sd(M_Size),
            foodMean = mean(OrigFood),
            foodSD = sd(OrigFood),
            wageMean = mean(OrigWage),
            wageSD = sd(OrigWage),
            fuelMean = mean(OrigFuel),
            fuelSD = sd(OrigFuel),
            conflictMean = mean(OriginConflict),
            conflictSD = sd(OriginConflict)
            )
syrTotal$spikes = sum(syrSummary$spikes)
syrSummary <- rbind(syrSummary,syrTotal)

xtable(syrSummary)

###results table
predictionTablesSyria <- read_csv("~/research/basu/migrationPatterns/predictionTablesSyria.csv")
xtable(predictionTablesSyria)


###plots
glmmAggLoop$locfFlow <- locfAggLoop$locfFlow
glmmAggLoop$histMean <- hmAggLoop$histMean
glmmAggLoop$predexp <- exp(glmmAggLoop$preds)
rfAggLoop$predexp <- exp(rfAggLoop$preds)
svmAggLoop$predexp <- exp(svmAggLoop$preds)

glmmAggLoopF$locfFlow <- locfAggLoopF$locfFlow
glmmAggLoopF$histMean <- hmAggLoopF$histMean

##origin-destination logflow plots

#glmm
glmmPlot <- glmmAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(logFlow,preds),factor_key=T) %>%
  filter(n() >= 30)

ggplot(data=glmmPlot,aes(x=NumMonths,y=measurement,color=type,group=type)) + 
  geom_line(data = filter(glmmPlot,type=="preds")) +
  geom_point(data=filter(glmmPlot,type=="logFlow")) +
  facet_wrap(~Origin+Destination,scales="free")

#rf
rfPlot <- rfAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(logFlow,preds),factor_key=T) %>%
  filter(n() >= 30)

ggplot(data=rfPlot,aes(x=NumMonths,y=measurement,color=type,group=type)) + 
  geom_line(data = filter(rfPlot,type=="preds")) +
  geom_point(data=filter(rfPlot,type=="logFlow")) +
  facet_wrap(~Origin+Destination,scales="free")



##destination true flow plots

#glmm
glmmPlotExp <- glmmAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=glmmPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted"))

#rf
rfPlotExp <- rfAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=rfPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted"))


#rfdirect
rfPlotExp2 <- rfAggLoop2 %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,preds),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=rfPlotExp2,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "preds"),labels=c("Observed", "Predicted"))




#svm
svmPlotExp <- svmAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=svmPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted"))

#merf
hmAggLoop$merfpreds <- exp(merfpreds4$X1)
merfPlotExp <- hmAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,merfpreds),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=merfPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "merfpreds"),labels=c("Observed", "Predicted"))



#example plot
glmmPlotExpExample <- subset(glmmPlotExp,(Destination=="Hassakeh") | (Destination=="Raqqa") | (Destination=="Homs") | (Destination=="Deir Ezzor"))
ggplot(data=glmmPlotExpExample,aes(x=NumMonths,y=log(sum),color=type,group=type)) + geom_line() +
  facet_wrap(~Destination,scales="free") + labs(x = "Number of Months", y = "Log Flow", title = "Observed vs Forecasted") +
  theme(text = element_text(size = 18))  + scale_color_discrete(name="Legend",
                                                               breaks=c("M_Size", "predexp"),
                                                               labels=c("Observed", "Predicted"))


##prediction vs observed values

#one month ahead

#glmm
ggplot(data=glmmAggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (LMM)") +
  theme(text = element_text(size = 18))

#historical mean
ggplot(data=glmmAggLoop,aes(x=log(histMean+1),y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

#locf
ggplot(data=glmmAggLoop,aes(x=locfFlow,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))

#rf
ggplot(data=rfAggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))

#rf direct
ggplot(data=rfAggLoop2,aes(x=preds,y=M_Size)) + geom_point(alpha=.5) +
  theme_bw() + #geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18)) + scale_y_log10() + scale_x_log10()



#three months ahead

#glmm
ggplot(data=glmmAggLoopF,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (GLMM)") +
  theme(text = element_text(size = 18))

#historical mean
ggplot(data=glmmAggLoopF,aes(x=log(histMean+1),y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

#locf
ggplot(data=glmmAggLoopF,aes(x=locfFlow,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))





##histograms
ggplot(data = syrSplit, aes(M_Size)) + geom_histogram() +
  labs(x="Flow",y="Frequency",title="Syria IDP Flow Histogram") +
  theme(text = element_text(size = 20))

ggplot(data = syrSplit, aes(logFlow)) + geom_histogram() +
  labs(x="Log Flow",y="Frequency",title="Syria IDP Log Flow Histogram") +
  theme(text = element_text(size = 20))


