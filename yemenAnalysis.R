library(readr)
library(dplyr)
library(caret)
library(here)

laggLoop <- read.csv(here("data","laggLoop.csv"))
aggLoop2NonLog <- read.csv(here("data","aggLoop2NonLog.csv"))
aggLoop3 <- read.csv(here("data","aggLoop3.csv"))
aggLoop3NonLog <- read.csv(here("data","aggLoop3NonLog.csv"))
aggLoopxgb <- read.csv(here("data","aggLoopxgb.csv"))
aggLooprf <- read.csv(here("data","aggLooprf.csv"))
aggLoopsvm <- read.csv(here("data","aggLoopsvm.csv"))
aggLoopxgb2 <- read.csv(here("data","aggLoopxgb2.csv"))
aggLooprf2 <- read.csv(here("data","aggLooprf2.csv"))
aggLoopsvm2 <- read.csv(here("data","aggLoopsvm2.csv"))
aggLoopmlp2 <- read.csv(here("data","aggLoopmlp2.csv"))
aggLoopPois <- read.csv(here("data","aggLoopPois.csv"))


merfPredsY <- read_csv(here("data","yemenmerfpreds2.txt"),col_names = F)
merfPredsNonlog <- read_csv(here("data","yemenmerfpredsnonlog.txt"),col_names=F)
aggLoopMerf <- aggLooprf
aggLoopMerf$preds <- merfPredsY$X1
aggLoopMerf2 <- aggLooprf
aggLoopMerf2$preds <- merfPredsNonlog$X1
signCheck <- function(df) {
  df2 <- df %>%
    group_by(Destination,Origin) %>%
    mutate(prev = lag(Flow,order_by=NumMonths),
           exppreds = exp(preds)) %>%
    ungroup() %>%
    mutate(sign = ifelse(Flow >= prev,1,0),
           predsign = ifelse(exppreds >= prev,1,0)) %>%
    mutate(sign = ifelse(is.na(sign),0,sign),
           predsign = ifelse(is.na(predsign),0,predsign))
  confusionMatrix(factor(df2$sign),factor(df2$predsign))
}

signCheck(laggLoop) #0.7293
signCheck(aggLooprf) #0.7439
signCheck(aggLoopsvm) #0.7383
signCheck(aggLoopxgb) #0.7586
signCheck(aggLoopMerf) #0.7468
signCheck(aggLoopmlp) #0.7208

signCheckNonLog <- function(df) {
  df2 <- df %>%
    group_by(Destination,Origin) %>%
    mutate(prev = lag(Flow,order_by=NumMonths)) %>%
    ungroup() %>%
    mutate(sign = ifelse(Flow >= prev,1,0),
           predsign = ifelse(preds >= prev,1,0)) %>%
    mutate(sign = ifelse(is.na(sign),0,sign),
           predsign = ifelse(is.na(predsign),0,predsign))
  confusionMatrix(factor(df2$sign),factor(df2$predsign))
}




signCheckNonLog(aggLoop2NonLog) #0.6729
signCheckNonLog(aggLoop3NonLog) #0.6029
signCheckNonLog(aggLoopsvm2) #0.6255
signCheckNonLog(aggLooprf2) #0.6582
signCheckNonLog(aggLoopxgb2) #0.6396
signCheckNonLog(aggLoopmlp2) #0.6655
signCheckNonLog(aggLoopPois) #0.5708
signCheckNonLog(aggLoopMerf2) #0.6407


signCheck(aggLoop3)

df3 <- aggLoop3NonLog

