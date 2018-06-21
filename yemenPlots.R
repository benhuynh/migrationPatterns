library(caret)
library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)
library(here)
library(networkD3)
library(xtable)

### Data Reading

laggyAggy <- read.csv(here("data","laggyAggy.csv"))
yemenAgg <- read.csv(here("data","yemenAgg.csv"))
aggLoop <- read.csv(here("data","laggLoop.csv"))
aggLoop2 <- read.csv(here("data","aggLoop2.csv"))
aggLoop3 <- read.csv(here("data","aggLoop3.csv"))
aggLooprf <- read.csv(here("data","aggLooprf.csv"))
aggLooprfNC <- read.csv(here("data","aggLooprfNC.csv"))

aggLoop2NonLog <- read.csv(here("data","aggLoop2NonLog.csv"))
aggLoop3NonLog <- read.csv(here("data","aggLoop3NonLog.csv"))



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


###results tables
predictionTablesYemen <- read_csv(here("data","predictionTablesYemen.csv"))
xtable(predictionTablesYemen)

### plots
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


ggplot(data=aggLoop2,aes(x=histMean,y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

ggplot(data=aggLoop3,aes(x=log(locfFlow+1),y=logFlow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,10),ylim=c(0,10)) + geom_abline() +
  labs(x="Predicted Values (log Flow)",y="Observed Values (log Flow)",title="Yemen Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))

### Observed vs predicted nonlog

ggplot(data=aggLooprf,aes(x=exp(preds),y=Flow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,27000),ylim=c(0,27000)) + geom_abline() +
  labs(x="Predicted Values (Flow)",y="Observed Values (Flow)",title="Yemen Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))

ggplot(data=aggLooprf2,aes(x=preds,y=Flow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,27000),ylim=c(0,27000)) + geom_abline() +
  labs(x="Predicted Values (Flow)",y="Observed Values (Flow)",title="Yemen Observed vs Predicted (RF)") +
  theme(text = element_text(size = 18))


ggplot(data=aggLoop2NonLog,aes(x=preds,y=Flow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,27000),ylim=c(0,27000)) + geom_abline() +
  labs(x="Predicted Values (Flow)",y="Observed Values (Flow)",title="Yemen Observed vs Predicted (HM)") +
  theme(text = element_text(size = 18))

ggplot(data=aggLoop3NonLog,aes(x=preds,y=Flow)) + geom_point(alpha=.5) +
  theme_bw() + coord_fixed(xlim=c(0,27000),ylim=c(0,27000)) + geom_abline() +
  labs(x="Predicted Values (Flow)",y="Observed Values (Flow)",title="Yemen Observed vs Predicted (LOCF)") +
  theme(text = element_text(size = 18))






### Destination true flow

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


# rf with bands
aggLooprfNC$predexp <- exp(aggLooprfNC$preds)
aggLooprfNC$lowexp <- exp(aggLooprfNC$five)
aggLooprfNC$upexp <- exp(aggLooprfNC$ninetyfive)


yemenDestPlotdfrfNC <- aggLooprfNC %>%
  group_by(Origin,Destination) %>%
  gather(type,measurement,c(Flow,predexp),factor_key=T) %>%
  ungroup() %>%
  group_by(Destination,NumMonths,type) %>%
  dplyr::summarize(sum = sum(measurement),
                   sumlow = sum(lowexp),
                   sumup = sum(upexp))

yemenDestPlotdfrfNC$sumlow[yemenDestPlotdfrfNC$type=="Flow"] <- NA
yemenDestPlotdfrfNC$sumup[yemenDestPlotdfrfNC$type=="Flow"] <- NA


ggplot(data=yemenDestPlotdfrf,aes(x=NumMonths,y=sum,color=type,group=type)) + geom_line() + geom_point(alpha=.5,size=1) +
  geom_ribbon(aes(ymin=sumlow,ymax=sumup),alpha=0.07,color=NA) +
  facet_wrap(~Destination,scales="free") + scale_y_log10() + labs(x = "Number of Months", y = "Arrivals", title = "Observed vs Forecasted by Destination Province (Yemen)") +
  scale_color_discrete(name="Legend", breaks=c("Flow", "predexp"), labels=c("Observed", "Predicted")) +
  theme_bw() + 
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.title = element_text(size=22))



