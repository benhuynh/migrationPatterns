library(here)
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(reshape2)

odAgg <- read_csv(here("data","odAgg.csv"))
odAgg$Date <- as.yearmon(odAgg$Date)

syriaConflictsACLED <- read_csv(here("data","syriaConflictsACLED.csv"))
icewsConflicts2016 <- read_delim(here("data","dataverseConflicts2016.tab"), 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
icewsConflicts2015 <- read_delim(here("data","events.2015.20170206133646.tab"), 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)


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
  summarize(conflict = sum(!is.na(year)),
            deaths = sum(as.numeric(fatalities),na.rm=T),
            civilianViolence = sum(ifelse(event_type=="Violence against civilians",1,0),na.rm=T),
            territoryChange = sum(ifelse(event_type=="Battle-Non-state actor overtakes territory"|event_type=="Battle-Government regains territory",
                                   1,0),na.rm=T)) #%>%
  #ungroup() %>%
  #group_by(admin1) %>%
  #mutate(conflict1 = lag(conflict,order_by=Date))

colnames(conflictACLED)[2] <- "Province"

conflictACLED[3:6] <- scale(conflictACLED[3:6])

conflictAgg <- left_join(odAgg,conflictACLED,by = c("Origin" = "Province", "Date" = "Date"))
conflictAgg <- left_join(conflictAgg,conflictACLED,by=c("Destination" = "Province","Date" = "Date"))

conflictAgg <- na.omit(conflictAgg)

coordinates <- read_delim(here("data","coordinates.txt"), 
                          " ", escape_double = FALSE, trim_ws = TRUE)
coordinates$country <- levels(factor(conflictAgg$Origin))
coordAgg <- inner_join(conflictAgg,coordinates,by=c("Origin" = "country"))
coordAgg <- inner_join(coordAgg,coordinates,by=c("Destination" = "country"))
library(geosphere)
distance <- distHaversine(cbind(coordAgg$lon.x,coordAgg$lat.x), cbind(coordAgg$lon.y, coordAgg$lat.y))
coordAgg$dist <- distance

syrSplit <- coordAgg[order(coordAgg$Date),]
syrSplit$logFlow <- log(syrSplit$M_Size)
syrSplit[6:11] <- scale(syrSplit[6:11])
syrSplit$dist <- scale(syrSplit$dist)
uniqueMonth <- unique(syrSplit$NumMonths)


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

lagAgg$prevmedian <- NULL
lagAgg$prevmedian2 <- NULL
lagAgg$prevmedian3 <- NULL
lagAgg$month <- as.numeric(format(lagAgg$Date,"%m"))

seasons = function(x){
  if(x %in% 2:4) return("Spring")
  if(x %in% 5:7) return("Summer")
  if(x %in% 8:10) return("Fall")
  if(x %in% c(11,12,1)) return("Winter")
  
}

lagAgg$season = sapply(lagAgg$month, seasons)


library(caret)
library(doSNOW)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

syriaModelrf <- train(logFlow ~ NumMonths+Origin+Destination+OrigFood+OrigFuel+OrigWage+DestFood+DestFuel+
                        DestWage+conflict.x+deaths.x+civilianViolence.x+territoryChange.x+conflict.y+deaths.y+
                        civilianViolence.y+territoryChange.y+dist+flowLag1+month+season,data=lagAgg,
                      method= "rf",verbose=T,trControl=ctrl,importance=T)
#  24    1.477685  0.5968913  1.110777


syriaModelrf2 <- train(logFlow ~ NumMonths+Origin+Destination+OrigFood+OrigFuel+OrigWage+DestFood+DestFuel+
                        DestWage+conflict.x+conflict.y+
                        +dist+flowLag1+month+season,data=lagAgg,
                      method= "rf",verbose=T,trControl=ctrl,importance=T)

uniqueMonth <- unique(lagAgg$NumMonths)


## Random Forest
rfAggLoop <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 20:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths < i)
  testLoop <- subset(lagAgg,NumMonths == i)
  trainModel <- train(logFlow ~ NumMonths+Origin+Destination+OrigFood+OrigFuel+OrigWage+DestFood+DestFuel+
                          DestWage+conflict.x+deaths.x+civilianViolence.x+territoryChange.x+conflict.y+deaths.y+
                          civilianViolence.y+territoryChange.y+dist+flowLag1+month+season,data=trainLoop,
                        method= "rf",verbose=T,trControl=ctrl,importance=T)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop <- rbind(rfAggLoop,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop$logFlow,rfAggLoop$preds) #1.39983
cor(rfAggLoop$logFlow,rfAggLoop$preds) #0.8350899
mae(rfAggLoop$logFlow,rfAggLoop$preds) #1.06085



## Random Forest with fewer variables
rfAggLoop2 <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 20:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths < i)
  testLoop <- subset(lagAgg,NumMonths == i)
  trainModel <- train(logFlow ~ NumMonths+Origin+Destination+OrigFood+OrigFuel+OrigWage+DestFood+DestFuel+
                        DestWage+conflict.x+conflict.y+
                        dist+flowLag1+month+season,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl,importance=T)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop2 <- rbind(rfAggLoop2,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop2$logFlow,rfAggLoop2$preds) #1.421744
cor(rfAggLoop2$logFlow,rfAggLoop2$preds) #0.827467
mae(rfAggLoop2$logFlow,rfAggLoop2$preds) #1.081351


## Random Forest with even fewer variables
rfAggLoop3 <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 20:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths < i)
  testLoop <- subset(lagAgg,NumMonths == i)
  trainModel <- train(logFlow ~ NumMonths+Origin+Destination+OrigFood+OrigFuel+OrigWage+DestFood+DestFuel+
                        DestWage+conflict.x+conflict.y+
                        dist+flowLag1,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl,importance=T)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop3 <- rbind(rfAggLoop3,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop3$logFlow,rfAggLoop3$preds) #1.427933
cor(rfAggLoop3$logFlow,rfAggLoop3$preds) #0.8258995
mae(rfAggLoop3$logFlow,rfAggLoop3$preds) #1.088292




