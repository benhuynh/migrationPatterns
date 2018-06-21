library(readr)
library(readxl)
#library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(ggmap)
library(lme4)
library(lmerTest)
#library(Hmisc)
yemenData <- read_csv("~/research/basu/migrationPatterns/tfpm_16th_dataset_oct2017.csv")
acledMiddleEast <- read_excel("~/research/basu/migrationPatterns/acledMiddleEast.xlsx")
icews16 <- read_delim("~/research/basu/migrationPatterns/dataverseConflicts2016.tab", 
                                     "\t", escape_double = FALSE, trim_ws = TRUE)
icews14 <- read_delim("~/research/basu/migrationPatterns/events.2014.20160121105408.tab", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
icews15 <- read_delim("~/research/basu/migrationPatterns/events.2015.20170206133646.tab", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
icews13 <- read_delim("~/research/basu/migrationPatterns/events.2013.20150313084929.tab", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)



###Original Flow Data
yemenDataGrouped <- yemenData %>%
  group_by(`Assessed Governorate`,`Governorate of Origin`,`Year of Displacement`,`Month of displacement  (2015 - 2016)`) %>%
  summarize(flow = sum(`# of IDPs Individuals`))
yemenDataGrouped <- subset(yemenDataGrouped,`Year of Displacement`!="2014 and before")
yemenDataGrouped$Date <- as.yearmon(paste(yemenDataGrouped$`Year of Displacement`,
                                          yemenDataGrouped$`Month of displacement  (2015 - 2016)`),"%Y %m")
yemenDataGrouped$`Year of Displacement` <- NULL
yemenDataGrouped$`Month of displacement  (2015 - 2016)` <- NULL
yemenDataGrouped <- subset(yemenDataGrouped,`Assessed Governorate`!="Socotra")
yemenDataGrouped$DataDate <- yemenDataGrouped$Date-.25

###ICEWS conflict data
icewsTransform <- function(df) {
  yemenICEWS <- subset(df,
                       (df$`Target Country`=="Yemen" & df$`Country`=="Yemen"))
  yemenICEWS <- subset(yemenICEWS,Intensity<=-9)
  yemenICEWS <- subset(yemenICEWS,!is.na(Province))
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Abyan"] <- "Abyan"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Dhamar"] <- "Dhamar"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat `Adan"] <- "Aden"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Sa`dah"] <- "Sa'ada"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Ma'rib"] <- "Marib"
  yemenICEWS$Province[yemenICEWS$Province=="Sanaa"] <- "Sana'a"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Hajjah"] <- "Hajjah"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Ta`izz"] <- "Taizz"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat al Jawf"] <- "Al Jawf"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Hadramawt"] <- "Hadramaut"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Lahij"] <- "Lahj"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat al Hudaydah"] <- "Al Hudaydah"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat al Bayda'"] <- "Al Bayda"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat ad Dali`"] <- "Al Dhale'e"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Raymah"] <- "Raymah"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat Raymah"] <- "Raymah"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat al Bayda"] <- "Al Bayda"
  yemenICEWS$Province[yemenICEWS$Province=="Muhafazat `Amran"] <- "Amran"
  yemenICEWS$Date <- as.yearmon(yemenICEWS$`Event Date`,"%y/%m/%d")
  yemenICEWS[nrow(yemenICEWS)+21,] <- NA
  yemenICEWS$Province[nrow(yemenICEWS)-21:nrow(yemenICEWS)] <- unique(yemenDataGrouped$`Assessed Governorate`)
  
  yemenConflictICEWS <- yemenICEWS %>%
    complete(Date,Province) %>%
    group_by(Date,Province) %>%
    summarize(conflict = sum(!is.na(Country)))
  
  yemenConflictICEWS <- subset(yemenConflictICEWS,Province!="Socotra")
  return(yemenConflictICEWS)
}
yemenICEWS13 <- icewsTransform(icews13)
yemenICEWS14 <- icewsTransform(icews14)
yemenICEWS15 <- icewsTransform(icews15)
yemenICEWS16 <- icewsTransform(icews16)
yemenICEWSAgg <- rbind(yemenICEWS13,yemenICEWS14,yemenICEWS15,yemenICEWS16)
yemenICEWSAgg$conflictScaled <- scale(yemenICEWSAgg$conflict)


###ACLED Conflict Data
yemenACLED <- subset(acledMiddleEast,COUNTRY=="Yemen")

yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Sadah"] <- "Sa'ada"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Sanaa"] <- "Sana'a"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Ad Dali"] <- "Al Dhale'e"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Lahij"] <- "Lahj"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Amanat al Asimah"] <- "Amanat Al Asimah"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Al Mahrah"] <- "Al Maharah"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Hadramawt"] <- "Hadramaut"
yemenACLED$ADMIN1[yemenACLED$ADMIN1=="Sadah"] <- "Sa'ada"
yemenACLED[nrow(yemenACLED)+1,] <- NA
yemenACLED$ADMIN1[10019] <- "Socotra"

yemenACLED$Date <- as.yearmon(yemenACLED$EVENT_DATE,"%Y-%m-%d")
yemenConflictACLED <- yemenACLED %>%
  complete(Date,ADMIN1) %>%
  group_by(Date,ADMIN1) %>%
  summarize(conflict = sum(!is.na(COUNTRY)))
yemenConflictACLED$conflictScaled <- scale(yemenConflictACLED$conflict)
colnames(yemenConflictACLED)[2] <- "Province"

###Aggregate Conflict Data
yemenConflictAgg <- rbind(yemenICEWSAgg,yemenConflictACLED)
yemenConflictAgg$Date <- as.yearmon(yemenConflictAgg$Date)
yemenConflictAgg$conflict <- NULL


###Food prices
WFPVAM_FoodPrices_05_12_2017 <- read_csv("~/Downloads/WFPVAM_FoodPrices_05-12-2017.csv")
yemenFoodPrices <- subset(WFPVAM_FoodPrices_05_12_2017,adm0_name=="Yemen")
yemenFoodPrices <- subset(yemenFoodPrices,mp_year >= 2013)
yemenFoodPrices$Date <- as.yearmon(paste(yemenFoodPrices$mp_year, yemenFoodPrices$mp_month), "%Y %m")
yemenFoodPrices$adm1_name[yemenFoodPrices$adm1_name=="$Amanat Al Asimah"] <- "Amanat Al Asimah"
yemenFoodPrices$adm1_name[yemenFoodPrices$adm1_name=="$Raymah"] <- "Raymah"
yemenFoodPrices <- subset(yemenFoodPrices,cm_name!="Wage (qualified labour)")
yemenFoodPrices <- subset(yemenFoodPrices,cm_name!="Wage (non-qualified labour)")
#full prices is do use more than just median covariates. doesn't work that well.
yemenFullPrices <- yemenFoodPrices
yemenFoodPrices$cm_name[yemenFoodPrices$cm_name=="Fuel (diesel)"] <- "Fuel"
yemenFoodPrices$cm_name[yemenFoodPrices$cm_name=="Fuel (petrol-gasoline)"] <- "Fuel"
yemenFoodPrices$cm_name[yemenFoodPrices$cm_name=="Fuel (gas)"] <- "Fuel"
#wage data is only available from Jul 2016 onward...ignoring since it usually doesn't do anything
yemenFoodPrices <- subset(yemenFoodPrices,cm_name!="Livestock (sheep, two-year-old male)")
yemenFoodPrices$cm_name[(yemenFoodPrices$cm_name!="Fuel")] <- "Food"
  

yemenPrices <- yemenFoodPrices %>%
  group_by(Date,adm1_name,cm_name) %>%
  summarize(median = median(mp_price))

yemenFullPrices <- yemenFullPrices %>%
  group_by(Date,adm1_name,cm_name) %>%
  summarize(price = median(mp_price))
yemenFullPrices <- dcast(yemenFullPrices,Date+adm1_name~cm_name,measure.vars="price")

yemenPrices <- dcast(yemenPrices, Date+adm1_name ~ cm_name,measure.vars="median")

#imputing by median of month
#many food prices are unavailable from Jul 2016 onwards, so we complete the cases
yemenPrices <- yemenPrices %>%
  complete(Date,adm1_name) %>%
  group_by(Date) %>%
  mutate(Food = ifelse(is.na(Food),median(Food,na.rm=T),Food),
         Fuel = ifelse(is.na(Fuel),median(Fuel,na.rm=T),Fuel))

yemenFullPrices <- yemenFullPrices %>%
  complete(Date,adm1_name) %>%
  group_by(Date) %>%
  mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))
yemenFullPrices <- yemenFullPrices[ , colSums(is.na(yemenFullPrices)) == 0]


###Combining all datasets
yemenAgg <- left_join(yemenDataGrouped,yemenPrices,by=c("Assessed Governorate" = "adm1_name","DataDate" = "Date"))
colnames(yemenAgg) <- c("Destination","Origin","Flow","Date","DataDate","DestFood","DestFuel")
yemenAgg <- left_join(yemenAgg,yemenPrices,by=c("Origin" = "adm1_name","DataDate" = "Date"))
colnames(yemenAgg)[8:9] <- c("OrigFood","OrigFuel")
yemenAgg <- na.omit(yemenAgg) #killed four observations, missing food/fuel prices. could've done this using inner_join above.
yemenAgg <- left_join(yemenAgg,yemenConflictAgg,by=c("Destination" = "Province","DataDate" = "Date"))
colnames(yemenAgg)[10] <- "DestConflict3"
yemenAgg <- left_join(yemenAgg,yemenConflictAgg,by=c("Origin" = "Province","DataDate" = "Date"))
colnames(yemenAgg)[11] <- "OrigConflict3"
yemenAgg <- left_join(yemenAgg,yemenConflictAgg,by=c("Destination" = "Province","Date" = "Date"))
colnames(yemenAgg)[12] <- "DestConflict"
yemenAgg <- left_join(yemenAgg,yemenConflictAgg,by=c("Origin" = "Province","Date" = "Date"))
colnames(yemenAgg)[13] <- "OrigConflict"

# yemenAgg <- inner_join(yemenAgg,yemenConflictAgg,by=c("Destination" = "Province","Date" = "Date"))
# colnames(yemenAgg)[12] <- "DestConfCurrent"
# yemenAgg <- inner_join(yemenAgg,yemenConflictAgg,by=c("Origin" = "Province","Date" = "Date"))
# colnames(yemenAgg[13]) <- "OrigConfCurrent"

yemenCoordinates <- read_delim("~/research/basu/migrationPatterns/yemenCoordinates.txt", 
                               " ", escape_double = FALSE, trim_ws = TRUE)
yemenCoordinates$province <- unique(yemenAgg$Destination)

yemenAgg <- inner_join(yemenAgg,yemenCoordinates,by=c("Origin" = "province"))
yemenAgg <- inner_join(yemenAgg,yemenCoordinates,by=c("Destination" = "province"))
colnames(yemenAgg)[14:17] <- c("OrigLon","OrigLat","DestLon","DestLat")
library(geosphere)
yemenDistance <- distHaversine(cbind(yemenAgg$OrigLon,yemenAgg$OrigLat),cbind(yemenAgg$DestLon,yemenAgg$DestLat))
yemenAgg$dist <- yemenDistance

yemenAgg2 <- inner_join(yemenAgg,yemenFullPrices,by=c("Destination"="adm1_name","Date"="Date"))
yemenAgg2 <- inner_join(yemenAgg2,yemenFullPrices,by=c("Origin"="adm1_name","Date"="Date"))




###Model Comparisons
library(lme4)
ySplit <- yemenAgg[order(yemenAgg$Date),]
ySplit[6:18] <- scale(ySplit[6:18]) 
ySplit[14:17] <- NULL

monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
ySplit$NumMonths <- mondf(ySplit$Date[1],ySplit$Date)
ySplit$Destination <- factor(ySplit$Destination)
ySplit$Origin <- factor(ySplit$Origin)
ySplit$logFlow <- log(ySplit$Flow+1)
#ySplit$Flow <- NULL
#ySplit$Date <- NULL
trainY <- ySplit[1:3445,]
testY <- ySplit[3446:3563,]


##add lags
# laggyAggy <- ySplit %>%
#   group_by(Destination,Origin) %>%
#   mutate(flowLag1 = lag(logFlow,order_by=Date)) %>%
#   ungroup() %>%
#   group_by(Date) %>%
#   mutate(flowLag1 = ifelse(is.na(flowLag1),log(median(Flow)),flowLag1)) %>%
#   ungroup()

#imputes by taking historical median within groups instead of taking current median.
laggyAggy <- ySplit %>%
  group_by(Destination,Origin) %>%
  mutate(flowLag1 = lag(logFlow,order_by=NumMonths)) %>%
  ungroup() 

laggyAggy <- laggyAggy %>%
  group_by(NumMonths,Destination) %>%
  summarize(prevmedian = log(median(Flow,na.rm=T))) %>%
  mutate(prevmedian = lag(prevmedian,order_by=NumMonths)) %>%
  right_join(laggyAggy) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian,flowLag1)) %>%
  ungroup()

laggyAggy <- laggyAggy %>%
  group_by(NumMonths,Origin) %>%
  summarize(prevmedian2 = log(median(Flow,na.rm=T))) %>%
  mutate(prevmedian2 = lag(prevmedian2,order_by=NumMonths)) %>%
  right_join(laggyAggy) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian2,flowLag1)) %>%
  ungroup()

laggyAggy <- laggyAggy %>%
  group_by(NumMonths) %>%
  summarize(prevmedian3 = log(median(Flow,na.rm=T))) %>%
  mutate(prevmedian3 = lag(prevmedian3,order_by=NumMonths)) %>%
  right_join(laggyAggy) %>%
  mutate(flowLag1 = ifelse(is.na(flowLag1),prevmedian3,flowLag1)) %>%
  ungroup()

laggyAggy <- subset(laggyAggy, NumMonths != 0)
laggyAggy$prevmedian <- NULL
laggyAggy$prevmedian2 <- NULL
laggyAggy$prevmedian3 <- NULL






##forecasting one step ahead internal validation

#svm
library(caret)
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLoopsvm <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopsvm <- rbind(aggLoopsvm,testLoop)
}
registerDoSEQ()
rmse(aggLoopsvm$logFlow,aggLoopsvm$preds) #1.371603
cor(aggLoopsvm$logFlow,aggLoopsvm$preds) #0.5730462
mae(aggLoopsvm$logFlow,aggLoopsvm$preds) #1.055826
sqrt(1/(2*1915))



#rf
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="none",verboseIter = TRUE)
rfGrid = expand.grid(mtry=26)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLooprf <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl,tuneGrid=rfGrid)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLooprf <- rbind(aggLooprf,testLoop)
}
registerDoSEQ()
rmse(aggLooprf$logFlow,aggLooprf$preds) #1.232085
cor(aggLooprf$logFlow,aggLooprf$preds) #0.6269232
mae(aggLooprf$logFlow,aggLooprf$preds) #0.9754136


#xgboost
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLoopxgb <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopxgb <- rbind(aggLoopxgb,testLoop)
}
registerDoSEQ()
rmse(aggLoopxgb$logFlow,aggLoopxgb$preds) #1.265008
cor(aggLoopxgb$logFlow,aggLoopxgb$preds) #0.6079901
mae(aggLoopxgb$logFlow,aggLoopxgb$preds) #0.9768768




#glmm
uniqueMonths <- unique(ySplit$NumMonths)
aggLoop <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+1])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+
                       OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoop <- rbind(aggLoop,testLoop)
}
rmse(aggLoop$logFlow,aggLoop$preds) #1.332766
cor(aggLoop$logFlow,aggLoop$preds) #0.5982949
mae(aggLoop$logFlow,aggLoop$preds) #1.049195
sqrt(1/(2*1915))

#glmm with lagged values
uniqueMonths <- unique(laggyAggy$NumMonths)
laggLoop <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+DestConflict3+OrigConflict3+
                       flowLag1+OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  laggLoop <- rbind(laggLoop,testLoop)
}
rmse(laggLoop$logFlow,laggLoop$preds) #1.295109
cor(laggLoop$logFlow,laggLoop$preds) #0.6005198
mae(laggLoop$logFlow,laggLoop$preds) #1.027069
sqrt(1/(2*1915))


#historical mean
aggLoop2 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = log(mean(Flow)))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  aggLoop2 <- rbind(aggLoop2,testLoopHM)
}

rmse(aggLoop2$logFlow,aggLoop2$histMean) #2.100152
cor(aggLoop2$logFlow,aggLoop2$histMean) #0.545445
mae(aggLoop2$logFlow,aggLoop2$histMean) #1.750422



#locf
aggLoop3 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,logFlow)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  aggLoop3 <- rbind(aggLoop3,testLooplocf)
}

rmse(aggLoop3$logFlow,aggLoop3$locfFlow) #1.484201
cor(aggLoop3$logFlow,aggLoop3$locfFlow) #0.5778199
mae(aggLoop3$logFlow,aggLoop3$locfFlow) #1.130139


##forecasting one step ahead internal validation with nonlog flow

#svm
rmse(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) #1149.046
cor(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) #0.4654572
mae(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) #254.3692

#rf
rmse(aggLooprf$Flow,exp(aggLooprf$preds)) #1140.014
cor(aggLooprf$Flow,exp(aggLooprf$preds)) #0.4548381
mae(aggLooprf$Flow,exp(aggLooprf$preds)) #247.0467

#xgb
rmse(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) #1236.938
cor(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) #0.3437585
mae(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) #258.5135





#glmm
# uniqueMonths <- unique(ySplit$NumMonths)
# aggLoop <- data_frame()
# for (i in 20:41) {
#   print(i)
#   trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
#   testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+1])
#   trainModel <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+
#                        OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop)
#   testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
#   aggLoop <- rbind(aggLoop,testLoop)
# } #*exp(.5*var(aggLoop$preds)) add smearing transformation within loop
rmse(aggLoop$Flow,exp(aggLoop$preds)) #1175.498
cor(aggLoop$Flow,exp(aggLoop$preds)) #0.4080612
mae(aggLoop$Flow,exp(aggLoop$preds)) #276.5948
sqrt(1/(2*1915))
untr<-smearingEst(aggLoop$preds,exp,statistic="fitted")

#historical mean
aggLoop2 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = mean(Flow))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  aggLoop2 <- rbind(aggLoop2,testLoopHM)
}

rmse(aggLoop2$Flow,aggLoop2$histMean) #1896.004
cor(aggLoop2$Flow,aggLoop2$histMean) #0.4190261
mae(aggLoop2$Flow,aggLoop2$histMean) #709.356

signYemenHM <- aggLoop2 %>%
  group_by(Destination,Origin) %>%
  mutate(prev = lag(Flow,order_by=Date)) %>%
  ungroup() %>%
  mutate(sign = ifelse(Flow >= prev,1,0),
         predsign = ifelse(histMean >= prev,1,0)) %>%
  mutate(sign = ifelse(is.na(sign),0,sign),
         predsign = ifelse(is.na(predsign),0,predsign))

confusionMatrix(factor(signYemenHM$sign),factor(signYemenHM$predsign)) #0.67



#locf
aggLoop3 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,Flow)
  locfLoop <- locfLoop[c("Destination","Origin","Flow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  aggLoop3 <- rbind(aggLoop3,testLooplocf)
}

rmse(aggLoop3$Flow,aggLoop3$locfFlow) #1413.295
cor(aggLoop3$Flow,aggLoop3$locfFlow) #0.4062796
mae(aggLoop3$Flow,aggLoop3$locfFlow) #325.9204


signYemenlocf <- aggLoop3 %>%
  group_by(Destination,Origin) %>%
  mutate(prev = lag(Flow,order_by=NumMonths)) %>%
  mutate(sign = ifelse(Flow >= prev,1,0),
         predsign = ifelse(preds >= prev,1,0)) %>%
  mutate(sign = ifelse(is.na(sign),0,sign),
         predsign = ifelse(is.na(predsign),0,predsign)) %>%
  ungroup()
  

confusionMatrix(factor(signYemenlocf$sign),factor(signYemenlocf$predsign)) #0





#poisson regression
uniqueMonths <- unique(laggyAggy$NumMonths)
aggLoopPois <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- glmer(Flow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+DestConflict3+OrigConflict3+
                        flowLag1+OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),
                     data=trainLoop,REML = F,family=poisson)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopPois <- rbind(aggLoopPois,testLoop)
}
rmse(aggLoopPois$Flow,aggLoopPois$preds) #1303.026
cor(aggLoopPois$Flow,aggLoopPois$preds) #0.2549441
mae(aggLoopPois$Flow,aggLoopPois$preds) #288.6764

write.csv(aggLoopPois,here("data","aggLoopPois.csv"))


#rf
ctrl=trainControl(method="none",verboseIter = TRUE)
rfGrid = expand.grid(mtry=26)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLooprf2 <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(Flow ~ .-logFlow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl,tuneGrid=rfGrid)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLooprf2 <- rbind(aggLooprf2,testLoop)
}
registerDoSEQ()
rmse(aggLooprf2$Flow,aggLooprf2$preds) #1262.999
cor(aggLooprf2$Flow,aggLooprf2$preds) #0.5566198
mae(aggLooprf2$Flow,aggLooprf2$preds) #398.7216




##forecasting three months ahead internal validation

#glmm three months ahead
uniqueMonths <- unique(ySplit$NumMonths)
aggLoopF <- data_frame()
for (i in 20:38) {
  print(i)
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+3])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+
                       OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  #testLoop$smearpreds <- exp(testLoop$preds)*exp(.5*var(testLoop$preds))
  aggLoopF <- rbind(aggLoopF,testLoop)
} #*exp(.5*var(aggLoop$preds)) add smearing transformation within loop
rmse(aggLoopF$Flow,exp(aggLoopF$preds)) #3484.872
cor(aggLoopF$Flow,exp(aggLoopF$preds)) #0.1272133
mae(aggLoopF$Flow,exp(aggLoopF$preds)) #540.2977

#historical mean three months ahead
aggLoop2F <- data_frame()
for (i in 20:38) {
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+3])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(histMean = mean(Flow))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$histMean[is.na(testLoopHM$histMean)] <- 0
  aggLoop2F <- rbind(aggLoop2F,testLoopHM)
}

rmse(aggLoop2F$Flow,aggLoop2F$histMean) #2060.475
cor(aggLoop2F$Flow,aggLoop2F$histMean) #0.4027134
mae(aggLoop2F$Flow,aggLoop2F$histMean) #763.6037

#locf 3 months ahead
aggLoop3F <- data_frame()
for (i in 20:38) {
  trainLoop <- subset(ySplit,NumMonths <= uniqueMonths[i])
  testLoop <- subset(ySplit,NumMonths == uniqueMonths[i+3])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,Flow)
  locfLoop <- locfLoop[c("Destination","Origin","Flow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  aggLoop3F <- rbind(aggLoop3F,testLooplocf)
}

rmse(aggLoop3F$Flow,aggLoop3F$locfFlow) #1492.056
cor(aggLoop3F$Flow,aggLoop3F$locfFlow) #0.389134
mae(aggLoop3F$Flow,aggLoop3F$locfFlow) #348.5546

##strongest predictors
library(xtable)
trainYglmmz <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+
                     OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=ySplit,REML = F)
summary(trainYglmmz)
texreg(trainYglmmz)

trainYrf <- train(logFlow ~ .-Flow-Date-DataDate,data=laggyAggy,
                    method= "rf",verbose=T,trControl=ctrl,tuneGrid=rfGrid,importance=T)
yemVimp <- varImp(trainYrf)

#importance plot
library(forcats)
library(tibble)
yemVimp$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  labs(y = "Importance",x="Variable Name",title="Variable Importance (Yemen)") +
  theme_bw() +
  theme(text = element_text(size = 18))





xtable(coef(summary(trainYglmmz)))

##sankey plot
yemSankdf <- laggyAggy %>%
  group_by(Origin,Destination) %>%
  summarize(flow = sum(Flow))
yemSankdf <- as.data.frame(yemSankdf)
yemSankdf$Destination <- as.character(yemSankdf$Destination)
yemSankdf$Origin <- as.character(yemSankdf$Origin)
yemSankdf$Destination <- paste0(yemSankdf$Destination," ")


library(networkD3)
name_vec <- c(sort(unique(yemSankdf$Destination)),sort(unique(yemSankdf$Origin)))

nodes <- data.frame(name = name_vec, id = 0:41)
nodes <- nodes[order(nodes$id,nodes$name),]
links <- links[order(links$Origin,links$Destination),]

links <- yemSankdf %>%
  left_join(nodes, by = c('Origin' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('Destination' = 'name')) %>%
  rename(dest_id = id)
sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'flow', NodeID = 'name', fontSize = 16,
              fontFamily="sans-serif",iterations=0)



###spikes evaluation
spikeYemen <- aggLooprf %>%
  group_by(Destination) %>%
  mutate(prev = lag(Flow,order_by=Date),
         exppreds = exp(preds),
         sd = sd(Flow)) %>%
  mutate(spike = ifelse(Flow >= prev+2*sd,1,0),
         predspike = ifelse(exppreds >= prev+1*sd,1,0))
#works very poorly. let's train a classifier to detect spikes.

#signs
signYemen <- laggyAggy %>%
  group_by(Destination,Origin) %>%
  mutate(prev = lag(Flow,order_by=Date)) %>%
  mutate(sign = ifelse(Flow >= prev,1,0)) %>%
  mutate(sign = ifelse(is.na(sign),0,sign)) %>%
  ungroup()
signYemen$prev <- NULL
signYemen$sign <- factor(signYemen$sign)
levels(signYemen$sign) <- c("Zero","One")


signYemenPreds <- aggLooprf %>%
  group_by(Destination,Origin) %>%
  mutate(prev = lag(Flow,order_by=Date),
         exppreds = exp(preds)) %>%
  ungroup() %>%
  mutate(sign = ifelse(Flow >= prev,1,0),
         predsign = ifelse(exppreds >= prev,1,0)) %>%
  mutate(sign = ifelse(is.na(sign),0,sign),
         predsign = ifelse(is.na(predsign),0,predsign))

confusionMatrix(factor(signYemenPreds$sign),factor(signYemenPreds$predsign)) #0.74

#spikes
spikeYemenAgg <- laggyAggy %>%
  group_by(Destination,Origin) %>%
  mutate(prev = lag(Flow,order_by=Date),
         sd = sd(Flow)) %>%
  mutate(spike = ifelse(Flow >= prev+2*sd,1,0)) %>%
  mutate(spike = ifelse(is.na(spike), 0, spike)) %>%
  ungroup()

#sd is taken historically, needs to be based on historical amounts.
spikeYemenAgg$prev <- NULL
spikeYemenAgg$sd <- NULL
spikeYemenAgg$spike <- factor(spikeYemenAgg$spike)
levels(spikeYemenAgg$spike) <- c("Zero","One")
spikeYemenAgg <- as.data.frame(spikeYemenAgg)

#rf spike model
ctrlSpike=trainControl(method="cv",number=5,verboseIter = TRUE,classProbs=T,summaryFunction=twoClassSummary)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
#rfGrid = expand.grid(mtry=26)

uniqueMonths <- unique(spikeYemenAgg$NumMonths)
aggLoopSpike <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(spikeYemenAgg,NumMonths <= uniqueMonths[i])
  testLoop <- subset(spikeYemenAgg,NumMonths == uniqueMonths[i+1])
  trainModel <- train(spike ~ .-Flow-logFlow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrlSpike,metric="ROC") #tuneGrid=rfGrid,
  testLoop$preds <- predict(trainModel,testLoop,type="prob")
  aggLoopSpike <- rbind(aggLoopSpike,testLoop)
}
registerDoSEQ()
auc(aggLoopSpike$preds$Zero,aggLoopSpike$spike) #works very poorly. auc is like 0.48.

#rf sign model
ctrlSpike=trainControl(method="cv",number=5,verboseIter = TRUE,classProbs=T,summaryFunction=twoClassSummary)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
#rfGrid = expand.grid(mtry=26)

uniqueMonths <- unique(signYemen$NumMonths)
aggLoopSign <- data.frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(signYemen,NumMonths <= uniqueMonths[i])
  testLoop <- subset(signYemen,NumMonths == uniqueMonths[i+1])
  trainModel <- train(sign ~ .-Flow-logFlow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrlSpike,metric="ROC") #tuneGrid=rfGrid,
  testLoop$preds <- predict(trainModel,testLoop,type="prob")$Zero
  aggLoopSign <- rbind(aggLoopSign,testLoop)
}
registerDoSEQ()
auc(aggLoopSign$sign,aggLoopSign$preds) #0.7351



###descriptive stats on data
yemSummary <- laggyAggy %>%
  group_by(Destination) %>%
  mutate(prev = lag(Flow,order_by=Date)) %>%
  summarize(n = n(),
            mean = mean(Flow),
            median = median(Flow),
            sd = sd(Flow),
            max = max(Flow),
            quantile25 = quantile(Flow,probs=.25),
            quantile75 = quantile(Flow,probs=.75),
            spikes = length(Flow[Flow >= prev+3*sd]))

yemTotal <- yemenAgg %>%
  summarize(Country = "Yemen",
            n = n(),
            flowMean = mean(Flow),
            flowSD = sd(Flow),
            foodMean = mean(OrigFood),
            foodSD = sd(OrigFood),
            wageMean = NA,
            wageSD = NA,
            fuelMean = mean(OrigFuel),
            fuelSD = sd(OrigFuel),
            conflictMean = mean(OrigConflict),
            conflictSD = sd(OrigConflict))
yemTotal$spikes <- sum(yemSummary$spikes)
yemSummary <- rbind(yemSummary,yemTotal)
xtable(yemSummary)

countrySummary <- rbind(syrTotal,yemTotal)
xtable(countrySummary)

yemenFoodPrices %>%
  group_by(cm_name) %>%
  summarize(price = mean(mp_price),
            sd = sd(mp_price))

#this accounts for non-imputed fluctuation in prices


###results tables
predictionTablesYemen <- read_csv("~/research/basu/migrationPatterns/predictionTablesYemen.csv")
xtable(predictionTablesYemen)


###Plots
aggLoop$predexp <- exp(aggLoop$preds)
##observed vs predicted

ggplot(data=aggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (GLMM)") +
  theme(text = element_text(size = 18))

ggplot(data=aggLooprf,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))


ggplot(data=aggLoop2,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

ggplot(data=aggLoop3,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))

##destination true flow

#glmm
yemenDestPlotdf <- aggLoop %>%
      group_by(Origin,Destination) %>%
      gather(type,measurement,c(Flow,predexp),factor_key=T) %>%
      #filter(n() >= 30) %>%
      ungroup() %>%
      group_by(Destination,NumMonths,type) %>%
      dplyr::summarize(sum = sum(measurement))

ggplot(data=yemenDestPlotdf,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Yemen)") +
  scale_color_discrete(name="Legend", breaks=c("Flow", "predexp"), labels=c("Observed", "Predicted"))

#rf
aggLooprf$predexp <- exp(aggLooprf$preds)
yemenDestPlotdfrf <- aggLooprf %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(Flow,predexp),factor_key=T) %>%
  #filter(n() >= 30) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=yemenDestPlotdfrf,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Yemen)") +
  scale_color_discrete(name="Legend", breaks=c("Flow", "predexp"), labels=c("Observed", "Predicted"))







##histograms
ggplot(data = ySplit, aes(Flow)) + geom_histogram(bins=25) +
  labs(x="Flow",y="Frequency",title="Yemen IDP Flow Histogram") +
  theme(text = element_text(size = 20))

ggplot(data = ySplit, aes(logFlow)) + geom_histogram(bins=25) +
  labs(x="Log Flow",y="Frequency",title="Yemen IDP Log Flow Histogram") +
  theme(text = element_text(size = 20))
