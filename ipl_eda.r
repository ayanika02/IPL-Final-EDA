library(ggplot2)
library(dplyr)

data <- read.csv("IPL final.csv", header= TRUE)
summary(data)

head(data)
tail(data)

colSums(is.na(data))
