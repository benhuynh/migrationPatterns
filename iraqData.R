library(readr)
library(plyr)
library(dplyr)
library(tidyr)
iraqDF <- read_csv("~/Downloads/Round90_Master_List_IDP_2018-2-28_IOM_DTM.csv")

colnames(iraqDF) <- iraqDF[3,]
iraqDF <- iraqDF[-1,]
iraqDF <- iraqDF[-1,]
iraqDF <- iraqDF[-1,]
iraqDF <- as.data.frame(iraqDF)
iraqDF[,ncol(iraqDF)] <- NULL
iraqDF[3:9] <- NULL
iraqDF[21:30] <- NULL
iraqDF[29:31] <- NULL


cleanz <- iraqDF %>%
  filter(!is.na(`Pre-June14`)) %>%
  gather(destination,flow,Anbar:Wassit) #%>%
  #group_by(Governorate,destination) %>%
  #summarize(count = sum(as.numeric(`Pre-June14`),na.rm=T))
  


cleanIraq <- iraqDF %>%
  gather(Date,flow,`Pre-June14`:`July 17`) %>%
  gather(destination,flow2,Anbar:Wassit) %>%
  filter(!is.na(flow)) #%>%
  #group_by(Governorate,Date,destination) %>%
  #summarize(count = sum(as.numeric(flow),na.rm=T))
cleanIraq[3:22] <- NULL

cleanIraq_test <- cleanIraq %>%
  group_by(Governorate,Date,destination) %>%
  summarize(count = sum(as.numeric(flow),na.rm=T))


cleanIraq1 <- iraqDF %>%
  group_by(Governorate) %>%
  filter(!is.na(`Pre-June14`)) %>%
  summarize(Anbar = ifelse(sum(as.numeric(Anbar)),na.rm=T),
            Babylon = sum(as.numeric(Babylon),na.rm=T),
            Baghdad = sum(as.numeric(Baghdad),na.rm=T),
            Basrah = sum(as.numeric(Basrah),na.rm=T),
            Dahuk = sum(as.numeric(Dahuk),na.rm=T),
            Diyala = sum(as.numeric(Diyala),na.rm=T),
            Erbil = sum(as.numeric(Erbil),na.rm=T),
            Kerbala = sum(as.numeric(Kerbala),na.rm=T),
            Kirkuk = sum(as.numeric(Kirkuk),na.rm=T),
            Missan = sum(as.numeric(Missan),na.rm=T),
            Muthanna = sum(as.numeric(Muthanna),na.rm=T),
            Najaf = sum(as.numeric(Najaf),na.rm=T),
            Ninewa = sum(as.numeric(Ninewa),na.rm=T),
            Qadissiya = sum(as.numeric(Qadissiya),na.rm=T),
            `Salah al-Din` = sum(as.numeric(`Salah al-Din`),na.rm=T),
            Sulaymaniyah = sum(as.numeric(Sulaymaniyah),na.rm=T),
            `Thi-Qar` = sum(as.numeric(`Thi-Qar`),na.rm=T),
            Wassit = sum(as.numeric(Wassit),na.rm=T))
cleanIraq2 <- iraqDF %>%
  group_by(Governorate) %>%
  filter(!is.na(`Pre-June14`)) %>%
  summarize(Anbar = sum(as.numeric(Anbar),na.rm=T),
            Babylon = sum(as.numeric(Babylon),na.rm=T),
            Baghdad = sum(as.numeric(Baghdad),na.rm=T),
            Basrah = sum(as.numeric(Basrah),na.rm=T),
            Dahuk = sum(as.numeric(Dahuk),na.rm=T),
            Diyala = sum(as.numeric(Diyala),na.rm=T),
            Erbil = sum(as.numeric(Erbil),na.rm=T),
            Kerbala = sum(as.numeric(Kerbala),na.rm=T),
            Kirkuk = sum(as.numeric(Kirkuk),na.rm=T),
            Missan = sum(as.numeric(Missan),na.rm=T),
            Muthanna = sum(as.numeric(Muthanna),na.rm=T),
            Najaf = sum(as.numeric(Najaf),na.rm=T),
            Ninewa = sum(as.numeric(Ninewa),na.rm=T),
            Qadissiya = sum(as.numeric(Qadissiya),na.rm=T),
            `Salah al-Din` = sum(as.numeric(`Salah al-Din`),na.rm=T),
            Sulaymaniyah = sum(as.numeric(Sulaymaniyah),na.rm=T),
            `Thi-Qar` = sum(as.numeric(`Thi-Qar`),na.rm=T),
            Wassit = sum(as.numeric(Wassit),na.rm=T))

data_long <- gather(cleanIraq, governorate, flow, Anbar:Wassit, factor_key=TRUE)

cleanIraq2 <- iraqDF %>% filter(!is.na(Anbar))




df <- data.frame(
  id = 1:10,
  time = as.Date('2009-01-01') + 0:9,
  Q3.2.1. = rnorm(10, 0, 1),
  Q3.2.2. = rnorm(10, 0, 1),
  Q3.2.3. = rnorm(10, 0, 1),
  Q3.3.1. = rnorm(10, 0, 1),
  Q3.3.2. = rnorm(10, 0, 1),
  Q3.3.3. = rnorm(10, 0, 1)
)
