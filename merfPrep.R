library(readr)
syriaAgg <- read_csv("~/research/basu/migrationPatterns/syriaAgg.csv")
syriaAgg[4:5] <- NULL
syriaAgg[15:18] <- NULL
syriaAgg$id <- paste0(syriaAgg$Destination,syriaAgg$Origin)
syriaAgg$id <- as.numeric(factor(syriaAgg$id))
syriaAgg$Destination <- as.numeric(factor(syriaAgg$Destination))
syriaAgg$Origin <- as.numeric(factor(syriaAgg$Origin))
write.csv(syriaAgg,"~/research/basu/migrationPatterns/syriaAggMerf.csv",row.names=F)

yemenAggLag <- read_csv("~/research/basu/migrationPatterns/yemenAggLag.csv")
yemenAggLag[4:5] <- NULL
yemenAggLag$id <- paste0(yemenAggLag$Destination,yemenAggLag$Origin)
yemenAggLag$id <- as.numeric(factor(yemenAggLag$id))
yemenAggLag$Destination <- as.numeric(factor(yemenAggLag$Destination))
yemenAggLag$Origin <- as.numeric(factor(yemenAggLag$Origin))
write.csv(yemenAggLag,"~/research/basu/migrationPatterns/yemenAggMerf.csv",row.names=F)
