setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
MyData <- read.csv(file="./data/raw/csv/1.csv", header=TRUE, sep=",")
head(MyData)
tail(MyData)
plot(MyData)
mean(MyData$V6)
summary(MyData)

MyData2 <- read.csv(file="./data/raw/csv/2.csv", header=TRUE, sep=",")
head(MyData2)
tail(MyData2)
plot(MyData2)
summary(MyData2)
