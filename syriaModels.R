library(caret)
library(readr)
library(lme4)
library(lmerTest)
library(dplyr)
library(here)

lagAgg <- read.csv(here("data","lagAgg.csv"))
uniqueMonth <- unique(lagAgg$NumMonths)

### Modeling ###

## Mixed Effects Model
glmmAggLoop <- data_frame()
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- lmer(logFlow~OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+OriginConflict*DestConflict+OrigFood*DestFood+flowLag1+
                       OriginConflict+DestConflict+dist+NumMonths+OriginConflict3+DestConflict3+(NumMonths|Origin/Destination),data=trainLoop,REML = F)
  testLoop$preds <- predict(trainModel,testLoop,allow.new.levels = T)
  glmmAggLoop <- rbind(glmmAggLoop,testLoop)
}
rmse(glmmAggLoop$logFlow,glmmAggLoop$preds) 
cor(glmmAggLoop$logFlow,glmmAggLoop$preds) 
mae(glmmAggLoop$logFlow,glmmAggLoop$preds) 

write.csv(glmmAggLoop,here("data","glmmAggLoop.csv"),row.names=F)

## Random Forest
rfAggLoop <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel$finalModel,testLoop,predict.all = TRUE)
  rfAggLoop <- rbind(rfAggLoop,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop$logFlow,rfAggLoop$preds) 
cor(rfAggLoop$logFlow,rfAggLoop$preds) 
mae(rfAggLoop$logFlow,rfAggLoop$preds) 

write.csv(rfAggLoop,here("data","rfAggLoop.csv"),row.names=F)

## random forest non-caret
rfAggLoopnc <- data_frame()

for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- randomForest(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                               OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,mtry=20,ntree = 1000)
  testPreds <- predict(trainModel,testLoop,predict.all = TRUE)
  se <- rowSds(testPreds$individual)/sqrt(1000)
  rq <- rowQuantiles(testPreds$individual,probs=c(0.05,0.95))
  testLoop$se <- se
  testLoop$five <- rq[,1]
  testLoop$ninetyfive <- rq[,2]
  testLoop$preds <- testPreds$aggregate
  rfAggLoopnc <- rbind(rfAggLoopnc,testLoop)
}
rmse(rfAggLoopnc$logFlow,rfAggLoopnc$preds) 
cor(rfAggLoop$logFlow,rfAggLoop$preds) 
mae(rfAggLoop$logFlow,rfAggLoop$preds) 

write.csv(rfAggLoopnc,here("data","rfAggLoopnc.csv"),row.names=F)


## Support Vector Machine
svmAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  svmAggLoop <- rbind(svmAggLoop,testLoop)
}
registerDoSEQ()
rmse(svmAggLoop$logFlow,svmAggLoop$preds) 
cor(svmAggLoop$logFlow,svmAggLoop$preds) 
mae(svmAggLoop$logFlow,svmAggLoop$preds) 

write.csv(svmAggLoop,here("data","svmAggLoop.csv"),row.names=F)


## xgBoost
xgbAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  xgbAggLoop <- rbind(xgbAggLoop,testLoop)
}
registerDoSEQ()
rmse(xgbAggLoop$logFlow,xgbAggLoop$preds) 
cor(xgbAggLoop$logFlow,xgbAggLoop$preds) 
mae(xgbAggLoop$logFlow,xgbAggLoop$preds) 

write.csv(xgbAggLoop,here("data","xgbAggLoop.csv"),row.names=F)

## mlp 
mlpAggLoop <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- caret::train(logFlow ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "brnn",trControl=ctrl)#,tuneGrid=tune.grid)
  testLoop$preds <- predict(trainModel,testLoop)
  mlpAggLoop <- rbind(mlpAggLoop,testLoop)
}
registerDoSEQ()
rmse(mlpAggLoop$logFlow,mlpAggLoop$preds) 
cor(mlpAggLoop$logFlow,mlpAggLoop$preds) 
mae(mlpAggLoop$logFlow,mlpAggLoop$preds) 

write.csv(mlpAggLoop,here("data","mlpAggLoop.csv"),row.names=F)

## Historical Mean

hmAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(preds = log(mean(M_Size)))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$preds[is.na(testLoopHM$preds)] <- 0
  hmAggLoop <- rbind(hmAggLoop,testLoopHM)
}

rmse(hmAggLoop$logFlow,hmAggLoop$preds) 
cor(hmAggLoop$logFlow,hmAggLoop$preds) 
mae(hmAggLoop$logFlow,hmAggLoop$preds)

write.csv(hmAggLoop,here("data","hmAggLoop.csv"),row.names=F)

## Last observation carried forward
locfAggLoop <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,logFlow)
  locfLoop <- locfLoop[c("Destination","Origin","logFlow")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  locfAggLoop <- rbind(locfAggLoop,testLooplocf)
}
colnames(locfAggLoop)[24] <- "preds"
rmse(locfAggLoop$logFlow,locfAggLoop$preds)
cor(locfAggLoop$logFlow,locfAggLoop$preds) 
mae(locfAggLoop$logFlow,locfAggLoop$preds) 

write.csv(locfAggLoop,here("data","locfAggLoop.csv"),row.names=F)

### Non Log Flow Error

#linear mixed effects model
rmse(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) 
cor(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) 
mae(glmmAggLoop$M_Size,exp(glmmAggLoop$preds)) 

#svm
rmse(svmAggLoop$M_Size,exp(svmAggLoop$preds)) 
cor(svmAggLoop$M_Size,exp(svmAggLoop$preds)) 
mae(svmAggLoop$M_Size,exp(svmAggLoop$preds)) 

## Support Vector Machine nonlogflow
svmAggLoop2 <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(M_Size ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "svmPoly",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  svmAggLoop2 <- rbind(svmAggLoop2,testLoop)
}
registerDoSEQ()
rmse(svmAggLoop2$M_Size,svmAggLoop2$preds) 
cor(svmAggLoop2$M_Size,svmAggLoop2$preds) 
mae(svmAggLoop2$M_Size,svmAggLoop2$preds) 

write.csv(svmAggLoop2,here("data","svmAggLoop2.csv"),row.names=F)

#rf
rmse(rfAggLoop$M_Size,exp(rfAggLoop$preds)) 
cor(rfAggLoop$M_Size,exp(rfAggLoop$preds)) 
mae(rfAggLoop$M_Size,exp(rfAggLoop$preds)) 


## Random Forest
rfAggLoop2 <- data_frame()
library(doSNOW)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

ctrl=trainControl(method="cv",number=5,verboseIter = TRUE)

for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(M_Size ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "rf",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  rfAggLoop2 <- rbind(rfAggLoop2,testLoop)
}
registerDoSEQ()
rmse(rfAggLoop2$M_Size,rfAggLoop2$preds) 
cor(rfAggLoop2$M_Size,rfAggLoop2$preds) 
mae(rfAggLoop2$M_Size,rfAggLoop2$preds) 

write.csv(rfAggLoop2,here("data","rfAggLoop2.csv"),row.names=F)


#xgb
rmse(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) 
cor(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) 
mae(xgbAggLoop$M_Size,exp(xgbAggLoop$preds)) 

## xgBoost nonlogflow
xgbAggLoop2 <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- train(M_Size ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                        OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                      method= "xgbTree",verbose=T,trControl=ctrl)
  testLoop$preds <- predict(trainModel,testLoop)
  xgbAggLoop2 <- rbind(xgbAggLoop2,testLoop)
}
registerDoSEQ()
rmse(xgbAggLoop2$M_Size,xgbAggLoop2$preds) 
cor(xgbAggLoop2$M_Size,xgbAggLoop2$preds) 
mae(xgbAggLoop2$M_Size,xgbAggLoop2$preds) 

write.csv(xgbAggLoop2,here("data","xgbAggLoop2.csv"),row.names=F)


#mlp

rmse(mlpAggLoop$M_Size,exp(mlpAggLoop$preds)) 
cor(mlpAggLoop$M_Size,exp(mlpAggLoop$preds)) 
mae(mlpAggLoop$M_Size,exp(mlpAggLoop$preds)) 


## mlp nonlogflow
mlpAggLoop2 <- data_frame()
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)
for (i in 5:23) {
  print(i)
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  trainModel <- caret::train(M_Size ~ OrigFood+OrigFuel+OrigWage+DestWage+DestFood+DestFuel+flowLag1+OriginConflict3+DestConflict3+
                               OriginConflict+DestConflict+dist+NumMonths+Origin+Destination,data=trainLoop,
                             method= "brnn",trControl=ctrl)#,tuneGrid=tune.grid)
  testLoop$preds <- predict(trainModel,testLoop)
  mlpAggLoop2 <- rbind(mlpAggLoop2,testLoop)
}
registerDoSEQ()
rmse(mlpAggLoop2$M_Size,mlpAggLoop2$preds) 
cor(mlpAggLoop2$M_Size,mlpAggLoop2$preds) 
mae(mlpAggLoop2$M_Size,mlpAggLoop2$preds) 

write.csv(mlpAggLoop2,here("data","mlpAggLoop2.csv"),row.names=F)


##Historical Mean

hmAggLoopNonLog <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  hmLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    summarize(preds = mean(M_Size))
  testLoopHM <- left_join(testLoop,hmLoop)
  testLoopHM$preds[is.na(testLoopHM$preds)] <- 0
  hmAggLoopNonLog <- rbind(hmAggLoopNonLog,testLoopHM)
}

rmse(hmAggLoopNonLog$M_Size,hmAggLoopNonLog$preds) 
cor(hmAggLoopNonLog$M_Size,hmAggLoopNonLog$preds) 
mae(hmAggLoopNonLog$M_Size,hmAggLoopNonLog$preds) 

write.csv(hmAggLoopNonLog,here("data","hmAggLoopNonLog.csv"),row.names=F)

##LOCF
locfAggLoopNonLog <- data_frame()
for (i in 5:23) {
  trainLoop <- subset(lagAgg,NumMonths <= uniqueMonth[i])
  testLoop <- subset(lagAgg,NumMonths == uniqueMonth[i+1])
  locfLoop <- trainLoop %>%
    group_by(Destination,Origin) %>%
    filter(NumMonths==max(NumMonths)) %>%
    arrange(Destination,Origin,M_Size)
  locfLoop <- locfLoop[c("Destination","Origin","M_Size")]
  colnames(locfLoop)[3] <- "locfFlow"
  testLooplocf <- left_join(testLoop,locfLoop)
  testLooplocf$locfFlow[is.na(testLooplocf$locfFlow)] <- 0
  locfAggLoopNonLog <- rbind(locfAggLoopNonLog,testLooplocf)
}
colnames(locfAggLoopNonLog)[24] <- "preds"
rmse(locfAggLoopNonLog$M_Size,locfAggLoopNonLog$preds) 
cor(locfAggLoopNonLog$M_Size,locfAggLoopNonLog$preds) 
mae(locfAggLoopNonLog$M_Size,locfAggLoopNonLog$preds) 

write.csv(locfAggLoopNonLog,here("data","locfAggLoopNonLog.csv"),row.names=F)

syriamerfpredsnonlog <- read_csv("~/research/basu/migrationPatterns/data/syriamerfpredsnonlog.txt",col_names = FALSE)
rmse(rfAggLoop$M_Size,syriamerfpredsnonlog$X1) 
cor(rfAggLoop$M_Size,syriamerfpredsnonlog$X1) 
mae(rfAggLoop$M_Size,syriamerfpredsnonlog$X1) 
