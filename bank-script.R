
library(dplyr)
library(FSelector)
library(ggplot2)

raw_data <- read.csv(
    file = "~/bank-marketing/bank-additional/bank-additional.csv",
    header = TRUE, 
    sep=";")

summary(raw_data$duration)

ig <- information.gain(y~.,raw_data)

igg <- data.frame(row.names(ig), ig$attr_importance)
names(igg) <- c("attr", "ig_attr")
igg <- igg[order(igg$ig_attr, decreasing = TRUE),]
igg$attr <- factor(igg$attr, levels= igg$attr)
ggplot(igg, aes(x=attr, y=ig_attr) ) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) 

