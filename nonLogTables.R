library(here)
library(xtable)
syriaNonLogTable <- read_delim(here("data","syriaNonLogTable.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
syriaLogTable <- read_delim(here("data","syriaLogTable.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
yemenNonLogTable <- read_delim(here("data","yemenNonLogTable.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
yemenLogTable <- read_delim(here("data","yemenLogTable.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)

syriaLogTable <- syriaLogTable[-(1:2),]
syriaDiffTable <- syriaNonLogTable[-1]-syriaLogTable[-1]
syriaDiffTable$Model <- syriaLogTable$Model
syriaDiffTable <- syriaDiffTable[,c(5,1,2,3,4)]

yemenLogTable <- yemenLogTable[-(1:2),]
yemenDiffTable <- yemenNonLogTable[-1]-yemenLogTable[-1]
yemenDiffTable$Model <- yemenLogTable$Model
yemenDiffTable <- yemenDiffTable[,c(5,1,2,3,4)]

xtable(syriaDiffTable)

xtable(yemenDiffTable)
