library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(Metrics)

originDestinationDataCleaned <- read_delim(here("data","originDestinationDataCleaned.csv"),
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
WFPVAM_FoodPrices_05_12_2017 <- read_csv(here("data","WFPVAM_FoodPrices_05-12-2017.csv"))
syriaConflictsACLED <- read_csv(here("data","syriaConflictsACLED.csv"))
icewsConflicts2016 <- read_delim(here("data","dataverseConflicts2016.tab"), 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
icewsConflicts2015 <- read_delim(here("data","events.2015.20170206133646.tab"), 
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
syriaPricesNA <- byRegionFoodType %>%
  complete(Date,adm1_name)

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

#write_csv(odAgg,here("data","odAgg.csv"))

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
coordinates <- read_delim(here("data","coordinates.txt"), 
                          " ", escape_double = FALSE, trim_ws = TRUE)
coordinates$country <- factor(levels(conflictAgg$Origin))
coordAgg <- inner_join(conflictAgg,coordinates,by=c("Origin" = "country"))
colnames(coordAgg)[17:18] <- c("OriginLon","OriginLat")
coordAgg <- inner_join(coordAgg,coordinates,by=c("Destination" = "country"))
colnames(coordAgg)[19:20] <- c("DestLon","DestLat")
library(geosphere)
distance <- distHaversine(cbind(coordAgg$OriginLon,coordAgg$OriginLat), cbind(coordAgg$DestLon, coordAgg$DestLat))
coordAgg$dist <- distance

write.csv(coordAgg,here("data","coordagg.csv"),row.names=F)

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

write.csv(lagAgg,here("data","lagAgg.csv"),row.names=F)


