library(here)
library(randomForestSRC)
library(ggRandomForests)

lagAgg <- read.csv(here("data","lagAgg.csv"))

syriaModelrfsrc <- rfsrc(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                          OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=lagAgg)

gg_v <- gg_variable(syriaModelrfsrc)
gg_md <- gg_minimal_depth(syriaModelrfsrc)
xvar <- gg_md$topvars

#minimal depth plot

plot(gg_md) + theme_bw() + theme(text = element_text(size=18))

#variable importance
plot(gg_vimp(syriaModelrfsrc)) + theme_bw()


#plot(gg_v, xvar=xvar, panel=TRUE,se=.95, span=1.2, alpha=.4)

# dependence plots
plot(gg_v,xvar=c("flowLag1","dist","OriginConflict","OrigFood","DestFood","OrigFuel","DestFuel",
                 "OrigWage","DestWage"),alpha=.4,panel=T,aes(color=Origin))


#colored in w/ destinations
getPalette = colorRampPalette(brewer.pal(14, "Set1"))

ggplot(gg_v, aes(x=flowLag1,y=yhat,color=Destination)) +
  geom_point(alpha=.7) + 
  scale_color_manual(values = getPalette(14))# + scale_color_brewer()


ggplot(gg_v, aes(x=OrigFood,y=yhat)) +
  geom_point(alpha=.7) + 
  facet_wrap(~Destination,scales = "free")

##interaction plots
#interactionSyria <- find.interaction(syriaModelrfsrc)
#write.csv(interactionSyria,here("data","interactionSyria.csv"))
interactionSyria <- read.csv(here("data","interactionSyria.csv"))
plot(gg_interaction(interactionSyria), xvar=gg_md$topvars, panel=TRUE) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))
