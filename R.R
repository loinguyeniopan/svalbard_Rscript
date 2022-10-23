
install.packages( "ggplot2")

install.packages( "vegan")

install.packages( "randomForest")

install.packages( "corrplot")

install.packages( "reshape2")

install.packages( "plyr")

lake=read.csv("lake.csv")

head(lake)
tail(lake)


lake[2,4]
head(lake[,1:4])
head(lake[1:4,])
head (lake$BOD)
install.packages("ggplot2")
lake = read.csv("lake.csv")
table(lake$Location)

head(lake)
lake.w = subset(lake, lake$Location=="Wonju")
table(lake.w$Location)
head (lake.w)

tapply(lake$chl, lake$Location, sd)
sapply(lake,mean)

library (reshape2)
install.packages("reshape2")

library(reshape2)
head(itis_melt)
install.packages("phyr")

