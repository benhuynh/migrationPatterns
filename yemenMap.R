library(rworldmap)
library(raster)
library(ggplot2)
library(readr)
library(dplyr)
library(rgeos)
library(here)
yemenAggData <- read_csv(here("data","yemenAggData.csv"))
yemenDestAgg <- yemenAggData %>%
  group_by(Destination,Date) %>%
  summarize(flowSum = sum(Flow))


yemenLevel1<- raster::getData("GADM", country = "Yemen", level = 1)

#fixing province names
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="`Adan"] <- "Aden"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Al Bayda'"] <- "Al Bayda"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Al Dali'"] <- "Al Dhale'e"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Al Mahrah"] <- "Al Maharah"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Hadramawt"] <- "Hadramaut"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Lahij"] <- "Lahj"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Ma'rib"] <- "Marib"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Sa`dah"] <- "Sa'ada"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="San`a'"] <- "Sana'a"
yemenLevel1$NAME_1[yemenLevel1$NAME_1=="Ta`izz"] <- "Taizz"

yemenMapAgg <- yemenDestAgg %>%
  group_by(Destination) %>%
  summarize(flowMean = mean(flowSum),
            flowSD = sd(flowSum))

y_map <- fortify(yemenLevel1)
y_map$id <- as.integer(y_map$id)

ydat <- data.frame(id = 1:(length(yemenLevel1@data$NAME_1)), state = yemenLevel1@data$NAME_1)
ymap_df <- inner_join(y_map, ydat, by = "id")
ymap_df <- left_join(ymap_df,yemenMapAgg,by = c("state" = "Destination"))

ycenters <- data.frame(gCentroid(yemenLevel1, byid = TRUE))
ycenters$state <- ydat$state

#ymap_df <- ymap_df[((ymap_df$piece!=2) & (ymap_df$piece!=3) & (ymap_df$piece!=4) & (ymap_df$piece!=5) & (ymap_df$piece!=6) &
#                      (ymap_df$piece!=7) & (ymap_df$piece!=8)),]

ymap_df <- ymap_df[(ymap_df$piece==1),]


ggplot() +
  geom_map(data = ymap_df, map = ymap_df, aes(x = long, y = lat, map_id = id, group = group,fill=flowSD)) +
  scale_fill_gradient(low="#fee0d2",high="#a50f15") +
  geom_polygon(data = ymap_df, aes(x = long, y = lat, group = group), color = "black", size = .05, fill = NA) +
  #scale_fill_gradient(trans="log10",low="#fee0d2",high="#a50f15",
  #                    breaks = scales::trans_breaks("log10", function(x) 10^x),
  #                    labels = scales::trans_format("log10", scales::math_format(10^.x))) + #set trans="log10" for log scale +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10,title="Flow SD")) +
  geom_text(data = ycenters, aes(label = state, x = x, y = y), size = 5) + theme_map()

## line plot data
yemenAgg <- read_csv("~/research/basu/migrationPatterns/yemenAgg.csv")
yemenAgg$Date <- as.yearmon(yemenAgg$Date)


#imputed foodprices
yemenImpAgg <- yemenAgg %>%
  group_by(Date) %>%
  summarize(flowsum=sum(Flow),
            food = median(OrigFood),
            fuel = median(OrigFuel)) %>%
  mutate(flowsum = flowsum/mean(flowsum),
         food = food/mean(food),
         fuel = fuel/mean(fuel)) %>%
  gather(type,measurement,c(flowsum,food,fuel),factor_key=T)


yemenImpAggRm <- yemenAgg %>%
  group_by(Date) %>%
  filter(Date >= 2015.167) %>%
  summarize(flowsum=sum(Flow),
            food = median(OrigFood),
            fuel = median(OrigFuel)) %>%
  mutate(flowsum = flowsum/mean(flowsum),
         food = food/mean(food),
         fuel = fuel/mean(fuel)) %>%
  gather(type,measurement,c(flowsum,food,fuel),factor_key=T) #remove first few months. doesn't look much better



#nonimputed food prices
yemenSumAgg <- yemenAgg %>%
  group_by(Date) %>%
  summarize(flowsum = sum(Flow)) %>%
  mutate(flowsum = flowsum/mean(flowsum)) %>%
  gather(type,measurement,c(flowsum),factor_key=T)

trueFoodz <- yemenFoodPrices %>%
  filter(cm_name=="Food") %>%
  filter(Date >= 2014) %>%
  group_by(Date) %>%
  summarize(median = median(mp_price)) %>%
  mutate(median = median/mean(median))

trueFoodz$cm_name <- "Food"

trueFuel <- yemenFoodPrices %>%
  filter(cm_name=="Fuel") %>%
  filter(Date >= 2014) %>%
  group_by(Date) %>%
  summarize(median = median(mp_price)) %>%
  mutate(median = median/mean(median))
trueFuel$cm_name <- "Fuel"


yemenSumAgg$type <- as.character(yemenSumAgg$type)
yemenSumAgg$Date <- as.yearmon(yemenSumAgg$Date)

yemenLineAgg <- full_join(yemenSumAgg,
                          trueFoodz,by=c("Date" = "Date","type"="cm_name","measurement"="median"))
yemenLineAgg <- full_join(yemenLineAgg,
                          trueFuel,by=c("Date" = "Date","type"="cm_name","measurement"="median"))




#aggregate flow
yemeniSpaghetti <- yemenDestAgg %>%
  group_by(Date) %>%
  summarize(sum = sum(flowSum))

##actual plotting

#aggregate flow plot
ggplot(data=yemeniSpaghetti, aes(x = as.factor(as.yearmon(Date)), y = sum, group = 1)) + geom_line(size=1) + geom_point(size=3,alpha=0.4) + 
  scale_y_log10(breaks = scales::pretty_breaks()) +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Date",y = "IDP Arrivals")

#imputed plot

ggplot(data = yemenImpAgg, aes(x=as.factor(Date),y=measurement,group=type,color=type)) + geom_line(size=1) +
  geom_point(size=3,alpha=0.4) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 18)) +
  labs(x="Date",y="Percentage of Average Value") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name="Type",
                         breaks=c("flowsum","food","fuel"),
                         labels=c("Arrivals","Food","Fuel"))
  #scale_y_log10(breaks = scales::pretty_breaks())

#nonimputed plot
ggplot(data = yemenLineAgg, aes(x=as.factor(Date),y=measurement,group=type,color=type)) + geom_line(size=1) +
  geom_point(size=3,alpha=0.4) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 18)) +
  labs(x="Date",y="Percentage of Average Value") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name="Type",
                       breaks=c("flowsum","Food","Fuel"),
                       labels=c("Arrivals","Food","Fuel"))

#scale_y_log10(breaks = scales::pretty_breaks())






