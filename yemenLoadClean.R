library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(reshape2)

yemenData <- read_csv(here("data","tfpm_16th_dataset_oct2017.csv"))
acledMiddleEast <- read_excel(here("data","acledMiddleEast.xlsx"))
icews16 <- read_delim(here("data","dataverseConflicts2016.tab"), 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
icews14 <- read_delim(here("data","events.2014.20160121105408.tab"), 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
icews15 <- read_delim(here("data","events.2015.20170206133646.tab"), 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
icews13 <- read_delim(here("data","events.2013.20150313084929.tab"), 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
WFPVAM_FoodPrices_05_12_2017 <- read_csv(here("data","WFPVAM_FoodPrices_05-12-2017.csv"))

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

yemenPricesNA <- yemenPrices %>%
  complete(Date,adm1_name) %>%
  filter(Date >= 2014)
#sum(is.na(yemenPricesNA))/(dim(yemenPricesNA)[1]*2) 46% missing values.

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

yemenCoordinates <- read_delim(here("data","yemenCoordinates.txt"), 
                               " ", escape_double = FALSE, trim_ws = TRUE)
yemenCoordinates$province <- unique(yemenAgg$Destination)

yemenAgg <- inner_join(yemenAgg,yemenCoordinates,by=c("Origin" = "province"))
yemenAgg <- inner_join(yemenAgg,yemenCoordinates,by=c("Destination" = "province"))
colnames(yemenAgg)[14:17] <- c("OrigLon","OrigLat","DestLon","DestLat")
library(geosphere)
yemenDistance <- distHaversine(cbind(yemenAgg$OrigLon,yemenAgg$OrigLat),cbind(yemenAgg$DestLon,yemenAgg$DestLat))
yemenAgg$dist <- yemenDistance

write.csv(yemenAgg,here("data","yemenAgg.csv"),row.names=F)


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

write.csv(ySplit,here("data","yemenAggData.csv"),row.names=F)

## build dataset with lags
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

write.csv(laggyAggy,here("data","laggyAggy.csv"),row.names = F)
