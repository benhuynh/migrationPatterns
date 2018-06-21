library(rworldmap)
library(raster)
library(ggplot2)
library(readr)
library(rgeos)
library(zoo)
library(here)
coordAgg <- read_csv(here("data","coordAgg.csv"))
lagAgg <- read.csv(here("data","lagAgg.csv"))
coordAgg$Date <- as.yearmon(coordAgg$Date)
lagAgg$Date <- as.yearmon(lagAgg$Date)

syriaLevel1<- raster::getData("GADM", country = "Syria", level = 1)


mapAgg <- lagAgg %>%
  group_by(Destination,Date) %>%
  summarize(flowsum = sum(M_Size)) %>%
  ungroup() %>%
  group_by(Destination) %>%
  summarize(flowMean = mean(flowsum),
            flowSD = sd(flowsum))

mapAgg$Destination <- as.character(mapAgg$Destination)


syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Al Ḥasakah"] <- "Hassakeh"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Ar Raqqah"] <- "Raqqa"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="As Suwayda'"] <- "As Suweida"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Dar`a"] <- "Daraa"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Dayr Az Zawr"] <- "Deir Ezzor"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Idlib"] <- "Edlib"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Hamah"] <- "Hama"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Hims"] <- "Homs"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Quneitra"] <- "Al Qunaytirah"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Rif Dimashq"] <- "Rural Damascus"
syriaLevel1$NAME_1[syriaLevel1$NAME_1=="Tartus"] <- "Tartous"
mapAgg$Destination[mapAgg$Destination=="Al_Qunaytirah"] <- "Al Qunaytirah"
mapAgg$Destination[mapAgg$Destination=="As_Suweida"] <- "As Suweida"





syriaLevel1 <- rworldmap::joinData2Map(mapAgg,nameMap="syriaLevel1",
                                       nameJoinIDMap = "NAME_1",nameJoinColumnData = "Destination")

map <- fortify(syriaLevel1)
map$id <- as.integer(map$id)

dat <- data.frame(id = 1:(length(syriaLevel1@data$NAME_1)), state = syriaLevel1@data$NAME_1)
map_df <- inner_join(map, dat, by = "id")
map_df <- left_join(map_df,mapAgg,by = c("state" = "Destination"))

theme_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.ticks.length=unit(0.3, "lines"),
      axis.ticks.margin=unit(0.5, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="white", colour=NA),
      legend.key=element_rect(colour="white"),
      legend.key.size=unit(1.5, "lines"),
      legend.position="right",
      legend.text=element_text(size=rel(1.2)),
      legend.title=element_text(size=rel(1.4), face="bold", hjust=0),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.margin=unit(0, "lines"),
      plot.background=element_blank(),
      plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
      plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5),
      strip.background=element_rect(fill="grey90", colour="grey50"),
      strip.text.x=element_text(size=rel(0.8)),
      strip.text.y=element_text(size=rel(0.8), angle=-90) 
    )   
}

centers <- data.frame(gCentroid(syriaLevel1, byid = TRUE))
centers$state <- dat$state
centers$x[centers$state=="Rural Damascus"] <- 37.53019
centers$y[centers$state=="Rural Damascus"] <- 33.26118




ggplot() +
  geom_map(data = map_df, map = map_df, aes(x = long, y = lat, map_id = id, group = group,fill=flowSD)) +
  scale_fill_gradient(low="#fee0d2",high="#a50f15") +
  geom_polygon(data = map_df, aes(x = long, y = lat, group = group), color = "black", size = .05, fill = NA) +
  #scale_fill_gradient(trans="log10",low="#fee0d2",high="#a50f15",
  #                    breaks = scales::trans_breaks("log10", function(x) 10^x),
  #                    labels = scales::trans_format("log10", scales::math_format(10^.x))) + #set trans="log10" for log scale +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10,title="Flow SD")) +
  geom_text(data = centers, aes(label = state, x = x, y = y), size = 5) + theme_map()


##aggregate lineplot
mapAgg2 <- lagAgg %>%
  group_by(Date) %>%
  summarize(flowsum = sum(M_Size))

lineAgg <- coordAgg %>%
  group_by(Date) %>%
  summarize(flowsum = sum(M_Size),
            food = median(OrigFood),
            fuel = median(OrigFuel),
            wage = median(OrigWage)) %>%
  mutate(flowsum = flowsum/mean(flowsum),
         food = food/mean(food),
         fuel = fuel/mean(fuel),
         wage = wage/mean(wage)) %>%
  gather(type,measurement,c(flowsum,food,fuel,wage),factor_key=T) #divide by food[1] for initial, divide by mean for avg
  
ggplot(data=mapAgg2,aes(x=as.factor(Date),y=flowsum,group=1)) + geom_line(size = 1) + geom_point(size=3,alpha=0.4) + 
  scale_y_log10(breaks = scales::pretty_breaks(n=4)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 18)) +
  labs(x="Date",y="IDP Arrivals")

ggplot(data = lineAgg, aes(x=as.factor(Date),y=measurement,group=type,color=type)) + geom_line(size = 1) +
  geom_point(size = 3,alpha=0.4) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 18)) +
  labs(x="Date",y="Percentage of Average Value") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(name="Type",
                       breaks=c("flowsum","food","fuel","wage"),
                       labels=c("Arrivals","Food","Fuel","Wages"))

  

