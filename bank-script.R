
library(dplyr)
library(data.table)
library(FSelector)
library(ggplot2)
library(hash)

raw_data <- fread(
    "~/bank-marketing/bank-additional/bank-additional-full.csv",
    header = TRUE, 
    stringsAsFactors = TRUE,
    na.strings = "unknown",
    sep=";")
summary(raw_data)

#Dataframe dimensions
dim(raw_data)
#Total number of missing values
sum(is.na(raw_data))
#percentage of missing values
sum(is.na(raw_data)) / (dim(raw_data)[1] * dim(raw_data)[2])

# estimate the "Null Model". Assuming that always say 'no' will for sure have a high accuracy
#> summary(raw_data$y)
#no  yes 
#3668  451 
#> dim(raw_data)
#[1] 4119   21
#> 3668/4119
#[1] 0.8905074
#>

#The target will be to build a model that has more than 89% of accuracy


ig <- information.gain(y~.,raw_data)

igg <- data.table(row.names(ig), ig$attr_importance)
names(igg) <- c("attr", "ig_attr")
#igg <- arrange(igg, desc(ig_attr))
igg <- igg[order(-ig_attr)]
igg$attr <- factor(igg$attr, levels= igg$attr)
row.names(igg) <- 1:20
 

# http://www.gapminder.org/data/

social <- "social/economic context"
personal <- "personal"
other <- "other"
lastContact <- "last contact"
lastCampaign <- "last campaign"


attr_cat <- new.env()
attr_cat[["duration"]] <- "duration!!"
attr_cat[["cons.conf.idx"]] <- social
attr_cat[["pdays"]] <- other
attr_cat[["previous"]] <- other
attr_cat[["job"]] <- personal
attr_cat[["education"]] <- personal
attr_cat[["housing"]] <-  personal
attr_cat[["euribor3m"]] <- social
attr_cat[["nr.employed"]] <- social
attr_cat[["poutcome"]] <- other
attr_cat[["age"]] <- personal
attr_cat[["default"]] <- personal
attr_cat[["marital"]] <- personal
attr_cat[["loan"]] <- personal
attr_cat[["cons.price.idx"]] <- social
attr_cat[["emp.var.rate"]] <- social
attr_cat[["month"]] <- lastCampaign
attr_cat[["contact"]] <- lastCampaign
attr_cat[["campaign"]] <- other
attr_cat[["day_of_week"]] <- lastCampaign


udf.cat <- function(attr) { attr_cat[[attr]] }

#igg$attr_category <- sapply(igg$attr, udf.cat)
igg$attr_category <- unlist(mget(as.character(igg$attr), envir = attr_cat))


ggplot(igg, aes(x=attr, y=ig_attr, fill=attr_category) ) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

df <- select(raw_data, -duration)

#Split into training set (80%) and test set (20%)

set.seed(564)
train_ind <- sample(seq_len(nrow(raw_data)), 
                    size = floor(0.8 * nrow(raw_data)))

train_data <- raw_data[train_ind,]
test_data <- raw_data[-train_ind,]

