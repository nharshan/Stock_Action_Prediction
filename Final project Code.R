#To read the data.
amz.df<-read.csv("AMZN.csv")

#View(amz.df)

#if we have to trim any data not necessary for prediction.
df<-amz.df [,- c (7)]### to remove quantity of the stocks.

#To replace na values with mediun of respective columns to avoid further data operation errors.
df[is.na(df$Open), "Open"] <- mean(na.omit(df$Open))
df[is.na(df$High), "High"] <- mean(na.omit(df$High))
df[is.na(df$Low), "Low"] <- mean(na.omit(df$Low))
df[is.na(df$Close), "Close"] <- mean(na.omit(df$Close))
df[is.na(df$Adj.Close), "Adj.Close"] <- mean(na.omit(df$Adj.Close))

#To round off the values to 3 decimels so that data is uniform and comparable.
library(dplyr)

df <- df %>% mutate_if(is.numeric, round, 3)

#View(df)

#To divide the data into training and validation 
selected.var <- c(1,2,3,4,5,6)

set.seed(1)  # set seed for reproducing the partition
# select first 600 rows's index of data for sample
train.index <- sample(c(1:600), 600)  

# save selected 600 rows and variables in training.df data fram
train.df <- df[train.index, selected.var]
# save selected the rest 400 rows and variables in valid.df data fram
valid.df <- df[-train.index, selected.var]

#View(train.df)

#to sort all the data as we need all the stock in the oreder of date or intial index.

rownames(train.df) <- 1 : length(rownames(train.df))
rownames(valid.df) <- 1 : length(rownames(valid.df))

#View(train.df)
#View(validate.df)

#To generate a colunm to define positive/equal/negative based on the open and close price of the stock as defined in the paper.
sample.df <- within(train.df, {
  Decision <- ifelse(train.df$Open > train.df$Close, "Positive", ifelse(train.df$Open < train.df$Close, "Negative", "Equal"))
  rm()
})

View(sample.df)

sample1.df <- within(sample.df, {
  Action <- ifelse(sample.df$Decision == "Positive", "Buy", ifelse(sample.df$Decision == "Negative", "Sell", "Sell"))
  rm()
})

View(sample1.df)

#To load the required library for classification tree. 
library(rpart)
library(rpart.plot)
default.ct <- rpart(sample1.df$Action ~ sample1.df$Decision, data = sample1.df, method = "class", control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

deeper.ct <- rpart(sample1.df$Action ~ sample1.df$Decision, data = sample1.df, method = "class", control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))# check rpart.control API
deeper.ct$frame
# deeper.ct$frame$var contains each node in the tree
deeper.ct$frame$var

# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree if it is a leaf, show color as gray, otherwise white
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  


