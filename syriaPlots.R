library(caret)
library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)
library(here)
library(networkD3)
library(xtable)

### Data Reading

lagAgg <- read.csv(here("data","lagAgg.csv"))
coordAgg <- read.csv(here("data","coordAgg.csv"))
glmmAggLoop <- read.csv(here("data","glmmAggLoop.csv"))
rfAggLoop <- read.csv(here("data","rfAggLoop.csv"))
hmAggLoop <- read.csv(here("data","hmAggLoop.csv"))
locfAggLoop <- read.csv(here("data","locfAggLoop.csv"))
rfAggLoopnc <- read.csv(here("data","rfAggLoopnc.csv"))

hmAggLoopNonLog <- read.csv(here("data","hmAggLoopNonLog.csv"))
locfAggLoopNonLog <- read.csv(here("data","locfAggLoopNonLog.csv"))



syrImp <- read.csv(here("data","syrImp.csv"))

## Importance Plot

syrImp %>% 
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


## Sankey Chart
syrSankdf <- lagAgg %>%
  group_by(Destination,Origin) %>%
  summarize(flow = sum(M_Size)) %>%
  as.data.frame()
syrSankdf$Destination <- as.character(syrSankdf$Destination)
syrSankdf$Origin <- as.character(syrSankdf$Origin)
syrSankdf$Destination <- paste0(syrSankdf$Destination," ")

name_vec <- c(sort(unique(syrSankdf$Destination)),sort(unique(syrSankdf$Origin)))

nodes <- data.frame(name = name_vec, id = 0:27)

links <- syrSankdf %>%
  left_join(nodes, by = c('Origin' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('Destination' = 'name')) %>%
  rename(dest_id = id)

sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'flow', NodeID = 'name', fontSize = 16,
              fontFamily="sans-serif",iterations=0)

## Destination True Flow Plots
glmmAggLoop$predexp <- exp(glmmAggLoop$preds)

#glmm
glmmPlotExp <- glmmAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=glmmPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted"))

#rf
rfAggLoop$predexp <- exp(rfAggLoop$preds)

rfPlotExp <- rfAggLoop %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement))

ggplot(data=rfPlotExp,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted"))


#rf w/ quantiles
rfAggLoopnc$predexp <- exp(rfAggLoopnc$preds)
rfAggLoopnc$lowexp <- exp(rfAggLoopnc$five)
rfAggLoopnc$upexp <- exp(rfAggLoopnc$ninetyfive)
rfAggLoopnc$seexp <- rfAggLoopnc$se*rfAggLoopnc$predexp




rfPlotExpnc <- rfAggLoopnc %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(M_Size,predexp),factor_key=T) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement),
                   sumlow = sum(lowexp),
                   sumup = sum(upexp),
                   sumse = sum(seexp))

rfPlotExpnc$sumlow[rfPlotExpnc$type=="M_Size"] <- NA
rfPlotExpnc$sumup[rfPlotExpnc$type=="M_Size"] <- NA


ggplot(data=rfPlotExpnc,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  geom_ribbon(aes(ymin=sumlow,ymax=sumup),alpha=0.1,color=NA) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Syria)") +
  scale_color_discrete(name="Legend", breaks=c("M_Size", "predexp"),labels=c("Observed", "Predicted")) +
  theme_bw() + 
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size=22))






### Predicted vs Observed Values

#historical mean
ggplot(data=hmAggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

#locf
ggplot(data=locfAggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))


#rf
ggplot(data=rfAggLoop,aes(x=preds,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,12.5),ylim=c(0,12.5)) + geom_abline() +
  labs(x = "Predicted Values (log Flow)", y = "Observed Values (log Flow)", title = "Syria Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))

### predicted vs observed values (nonlog flow)

#rf actual
ggplot(data=rfAggLoop,aes(x=exp(preds),y=M_Size)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(ratio = 1,xlim=c(0,150000),ylim=c(0,150000)) + geom_abline() +
  labs(x = "Predicted Values (Flow)", y = "Observed Values (Flow)", title = "Syria Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18)) 

#rf zoomed in 50000
ggplot(data=rfAggLoop,aes(x=exp(preds),y=M_Size)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(ratio = 1,xlim=c(0,50000),ylim=c(0,50000)) + geom_abline() +
  labs(x = "Predicted Values (Flow)", y = "Observed Values (Flow)", title = "Syria Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))

#locf zoomed out
ggplot(data=locfAggLoopNonLog,aes(x=preds,y=M_Size)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(ratio = 1,xlim=c(0,150000),ylim=c(0,150000)) + geom_abline() +
  labs(x = "Predicted Values (Flow)", y = "Observed Values (Flow)", title = "Syria Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))

#hm zoomed out
ggplot(data=hmAggLoopNonLog,aes(x=preds,y=M_Size)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(ratio = 1,xlim=c(0,150000),ylim=c(0,150000)) + geom_abline() +
  labs(x = "Predicted Values (Flow)", y = "Observed Values (Flow)", title = "Syria Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))




## Log/normal histograms
ggplot(data = lagAgg, aes(M_Size)) + geom_histogram() +
  labs(x="Flow",y="Frequency",title="Syria IDP Flow Histogram") +
  theme(text = element_text(size = 20))

ggplot(data = lagAgg, aes(logFlow)) + geom_histogram() +
  labs(x="Log Flow",y="Frequency",title="Syria IDP Log Flow Histogram") +
  theme(text = element_text(size = 20))





## Descriptive Stats on Data
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

xtable(syrSummary) # we're mostly not using this. sticking with syrSummary if we are.

### Results Table
predictionTablesSyria <- read_csv(here("data","predictionTablesSyria.csv"))
xtable(predictionTablesSyria)


