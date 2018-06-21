library(caret)
library(readr)
library(here)
library(Metrics)
library(lme4)

laggyAggy <- read.csv(here("data","laggyAggy.csv"))
uniqueMonths <- unique(laggyAggy$NumMonths)


### Model Evaluation on logFlow

## Support Vector Machine
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

aggLoopsvm <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopsvm <- rbind(aggLoopsvm,testLoop)
}
registerDoSEQ()
rmse(aggLoopsvm$logFlow,aggLoopsvm$preds) 
cor(aggLoopsvm$logFlow,aggLoopsvm$preds) 
mae(aggLoopsvm$logFlow,aggLoopsvm$preds) 

write.csv(aggLoopsvm,here("data","aggLoopsvm.csv"),row.names=F)

## mlp
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

aggLoopmlp <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "brnn",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopmlp <- rbind(aggLoopmlp,testLoop)
}
registerDoSEQ()
rmse(aggLoopmlp$logFlow,aggLoopmlp$preds) 
cor(aggLoopmlp$logFlow,aggLoopmlp$preds) 
mae(aggLoopmlp$logFlow,aggLoopmlp$preds) 

write.csv(aggLoopmlp,here("data","aggLoopmlp.csv"),row.names=F)

## Random Forest
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
uniqueMonths <- unique(laggyAggy$NumMonths)
aggLooprf <- data_frame()

for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)#,tuneGrid=rfGrid)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLooprf <- rbind(aggLooprf,testLoop)
}
registerDoSEQ()
rmse(aggLooprf$logFlow,aggLooprf$preds) 
cor(aggLooprf$logFlow,aggLooprf$preds) 
mae(aggLooprf$logFlow,aggLooprf$preds) 

write.csv(aggLooprf,here("data","aggLooprf.csv"),row.names=F)

## Random Forest w/ all predictions

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLooprfNC <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- randomForest(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,mtry=26)
  testpreds <- predict(trainModel,testLoop,predict.all=T)
  rq <- rowQuantiles(testpreds$individual,probs=c(0.05,0.95))
  testLoop$five <- rq[,1]
  testLoop$ninetyfive <- rq[,2]
  testLoop$preds <- testpreds$aggregate
  aggLooprfNC <- rbind(aggLooprfNC,testLoop)
}
registerDoSEQ()
rmse(aggLooprfNC$logFlow,aggLooprfNC$preds) 
cor(aggLooprfNC$logFlow,aggLooprfNC$preds) 
mae(aggLooprfNC$logFlow,aggLooprfNC$preds) 

write.csv(aggLooprfNC,here("data","aggLooprfNC.csv"),row.names=F)

## xgboost
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLoopxgb <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(logFlow ~ .-Flow-Date-DataDate,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopxgb <- rbind(aggLoopxgb,testLoop)
}
registerDoSEQ()
rmse(aggLoopxgb$logFlow,aggLoopxgb$preds) 
cor(aggLoopxgb$logFlow,aggLoopxgb$preds) 
mae(aggLoopxgb$logFlow,aggLoopxgb$preds) 

write.csv(aggLoopxgb,here("data","aggLoopxgb.csv"),row.names=F)

## Linear Mixed Effects Model
uniqueMonths <- unique(laggyAggy$NumMonths)
laggLoop <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+DestFood+DestFuel+OrigConflict*DestConflict+OrigFood*DestFood+DestConflict3+OrigConflict3+
                       flowLag1+OrigConflict+DestConflict+dist+NumMonths+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  laggLoop <- rbind(laggLoop,testLoop)
}
rmse(laggLoop$logFlow,laggLoop$preds) 
cor(laggLoop$logFlow,laggLoop$preds) 
mae(laggLoop$logFlow,laggLoop$preds) 

write.csv(laggLoop,here("data","laggLoop.csv"),row.names=F)


## Historical Mean
aggLoop2 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(preds = log(mean(Flow)))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$preds[is.na(testLoopHM$preds)] <- 0
  aggLoop2 <- rbind(aggLoop2,testLoopHM)
}

rmse(aggLoop2$logFlow,aggLoop2$preds) 
cor(aggLoop2$logFlow,aggLoop2$preds) 
mae(aggLoop2$logFlow,aggLoop2$preds) 

write.csv(aggLoop2,here("data","aggLoop2.csv"),row.names=F)

## Last Observation Carried Forward
aggLoop3 <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,logFlow)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "preds"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$preds[is.na(testLooplocf$preds)] <- 0
  aggLoop3 <- rbind(aggLoop3,testLooplocf)
}

rmse(aggLoop3$logFlow,aggLoop3$preds) 
cor(aggLoop3$logFlow,aggLoop3$preds) 
mae(aggLoop3$logFlow,aggLoop3$preds) 

write.csv(aggLoop3,here("data","aggLoop3.csv"),row.names=F)

### Model Evaluation on nonLog Flow

#svm
rmse(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) 
cor(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) 
mae(aggLoopsvm$Flow,exp(aggLoopsvm$preds)) 

#rf
rmse(aggLooprf$Flow,exp(aggLooprf$preds)) 
cor(aggLooprf$Flow,exp(aggLooprf$preds)) 
mae(aggLooprf$Flow,exp(aggLooprf$preds)) 

#xgb
rmse(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) 
cor(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) 
mae(aggLoopxgb$Flow,exp(aggLoopxgb$preds)) 

#linear mixed effects model
rmse(laggLoop$Flow,exp(laggLoop$preds)) 
cor(laggLoop$Flow,exp(laggLoop$preds)) 
mae(laggLoop$Flow,exp(laggLoop$preds)) 

#mlp
rmse(aggLoopmlp$Flow,exp(aggLoopmlp$preds)) 
cor(aggLoopmlp$Flow,exp(aggLoopmlp$preds)) 
mae(aggLoopmlp$Flow,exp(aggLoopmlp$preds)) 


#historical mean
aggLoop2NonLog <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(preds = mean(Flow))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$preds[is.na(testLoopHM$preds)] <- 0
  aggLoop2NonLog <- rbind(aggLoop2,testLoopHM)
}

rmse(aggLoop2NonLog$Flow,aggLoop2NonLog$preds) 
cor(aggLoop2NonLog$Flow,aggLoop2NonLog$preds) 
mae(aggLoop2NonLog$Flow,aggLoop2NonLog$preds) 

write.csv(aggLoop2NonLog,here("data","aggLoop2NonLog.csv"),row.names=F)

#locf
aggLoop3NonLog <- data_frame()
for (i in 20:41) {
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,Flow)
  locfLoop <- locfLoop[c("Destination","Origin","Flow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  aggLoop3NonLog <- rbind(aggLoop3NonLog,testLooplocf)
}
colnames(aggLoop3NonLog)[18] <- "preds"

rmse(aggLoop3NonLog$Flow,aggLoop3NonLog$preds) 
cor(aggLoop3NonLog$Flow,aggLoop3NonLog$preds) 
mae(aggLoop3NonLog$Flow,aggLoop3NonLog$preds) 

write.csv(aggLoop3NonLog,here("data","aggLoop3NonLog.csv"),row.names=F)


## Support Vector Machine nonlogflow
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

aggLoopsvm2 <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(Flow ~ .-logFlow-Date-DataDate,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopsvm2 <- rbind(aggLoopsvm2,testLoop)
}
registerDoSEQ()
rmse(aggLoopsvm2$Flow,aggLoopsvm2$preds) 
cor(aggLoopsvm2$Flow,aggLoopsvm2$preds) 
mae(aggLoopsvm2$Flow,aggLoopsvm2$preds) 

write.csv(aggLoopsvm2,here("data","aggLoopsvm2.csv"),row.names=F)


## mlp nonlog flow
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

aggLoopmlp2 <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(Flow ~ .-logFlow-Date-DataDate,data=trainLoop,
                      method= "brnn",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopmlp2 <- rbind(aggLoopmlp2,testLoop)
}
registerDoSEQ()
rmse(aggLoopmlp2$Flow,aggLoopmlp2$preds) 
cor(aggLoopmlp2$Flow,aggLoopmlp2$preds) 
mae(aggLoopmlp2$Flow,aggLoopmlp2$preds) 

write.csv(aggLoopmlp2,here("data","aggLoopmlp2.csv"),row.names=F)

## Random Forest
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="none",verboseIter = TRUE)
rfGrid = expand.grid(mtry=26)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLooprf2 <- data_frame()

for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(Flow ~ .-logFlow-Date-DataDate,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)#,tuneGrid=rfGrid)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLooprf2 <- rbind(aggLooprf2,testLoop)
}
registerDoSEQ()
rmse(aggLooprf2$Flow,aggLooprf2$preds) 
cor(aggLooprf2$Flow,aggLooprf2$preds) 
mae(aggLooprf2$Flow,aggLooprf2$preds) 

write.csv(aggLooprf2,here("data","aggLooprf2.csv"),row.names=F)


## MERF nonlog flow
merfPredsNonlog <- read_csv(here("data","yemenmerfpredsnonlog.txt"),col_names=F)
aggLoopMerf2 <- aggLooprf
aggLoopMerf2$preds <- merfPredsNonlog$X1
rmse(aggLoopMerf2$preds,aggLoopMerf2$Flow) 
cor(aggLoopMerf2$preds,aggLoopMerf2$Flow) 
mae(aggLoopMerf2$preds,aggLoopMerf2$Flow) 

## xgboost nonlog flow
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
ctrl=trainControl(method="cv",number=2,verboseIter = TRUE)

uniqueMonths <- unique(laggyAggy$NumMonths)
aggLoopxgb2 <- data_frame()
for (i in 20:41) {
  print(i)
  trainLoop <- subset(laggyAggy,NumMonths <= uniqueMonths[i])
  testLoop <- subset(laggyAggy,NumMonths == uniqueMonths[i+1])
  trainModel <- train(Flow ~ .-logFlow-Date-DataDate,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  aggLoopxgb2 <- rbind(aggLoopxgb2,testLoop)
}
registerDoSEQ()
rmse(aggLoopxgb2$Flow,aggLoopxgb2$preds) 
cor(aggLoopxgb2$Flow,aggLoopxgb2$preds) 
mae(aggLoopxgb2$Flow,aggLoopxgb2$preds) 

write.csv(aggLoopxgb2,here("data","aggLoopxgb2.csv"),row.names=F)



