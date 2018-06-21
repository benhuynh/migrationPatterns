library(readr)
library(dplyr)
library(caret)
library(here)


glmmAggLoop <- read.csv(here("data","glmmAggLoop.csv"))
rfAggLoop <- read.csv(here("data","rfAggLoop.csv"))
svmAggLoop <- read.csv(here("data","svmAggLoop.csv"))
xgbAggLoop <- read.csv(here("data","xgbAggLoop.csv"))
mlpAggLoop <- read.csv(here("data","mlpAggLoop.csv"))
merfPreds <- read_csv(here("data","merfpreds6.txt"), col_names = FALSE)
merfPreds2 <- read_csv(here("data","syriamerfpredsnonlog.txt"), col_names = FALSE)
merfAggLoop <- glmmAggLoop
merfAggLoop$preds <- merfPreds$X1
merfAggLoop2 <- glmmAggLoop
merfAggLoop2$preds <- merfPreds2$X1
hmAggLoopNonLog <- read.csv(here("data","hmAggLoopNonLog.csv"))
locfAggLoopNonLog <- read.csv(here("data","locfAggLoopNonLog.csv"))
poisAggLoop <- read.csv(here("data","poisAggLoop.csv"))


rfAggLoop2 <- read.csv(here("data","rfAggLoop2.csv"))
svmAggLoop2 <- read.csv(here("data","svmAggLoop2.csv"))
xgbAggLoop2 <- read.csv(here("data","xgbAggLoop2.csv"))
mlpAggLoop2 <- read.csv(here("data","mlpAggLoop2.csv"))


signCheck <- function(df) {
  df2 <- df %>%
    group_by(Destination,Origin) %>%
    mutate(prev = lag(M_Size,order_by=NumMonths),
           exppreds = exp(preds)) %>%
    ungroup() %>%
    mutate(sign = ifelse(M_Size >= prev,1,0),
           predsign = ifelse(exppreds >= prev,1,0)) %>%
    mutate(sign = ifelse(is.na(sign),0,sign),
           predsign = ifelse(is.na(predsign),0,predsign))
  confusionMatrix(factor(df2$sign),factor(df2$predsign))
}

signCheckNonLog <- function(df) {
  df2 <- df %>%
    group_by(Destination,Origin) %>%
    mutate(prev = lag(M_Size,order_by=NumMonths)) %>%
    mutate(sign = ifelse(M_Size >= prev,1,0),
           predsign = ifelse(preds >= prev,1,0)) %>%
    mutate(sign = ifelse(is.na(sign),0,sign),
           predsign = ifelse(is.na(predsign),0,predsign)) %>%
    ungroup()
  confusionMatrix(factor(df2$sign),factor(df2$predsign))
}



signCheck(glmmAggLoop) #0.7052
signCheck(rfAggLoop) #0.7013
signCheck(svmAggLoop) #0.7021
signCheck(xgbAggLoop) #0.6794
signCheck(merfAggLoop) #0.6966
signCheck(mlpAggLoop) #0.6763
signCheckNonLog(hmAggLoopNonLog) #0.6341
signCheckNonLog(locfAggLoopNonLog) #0.5895 We know this is the NIR from the previous signCheck. 
signCheckNonLog(rfAggLoop2) #0.5895
signCheckNonLog(svmAggLoop2) #0.6059
signCheckNonLog(xgbAggLoop2) #0.5903
signCheckNonLog(mlpAggLoop2) #0.6145
signCheckNonLog(merfAggLoop2) #0.5856
signCheckNonLog(poisAggLoop) #0.5708
