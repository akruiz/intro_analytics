# Hw 1
# Andrea Ruiz
# May 24, 2023
    
# 2.1
# I love to online shop, one of my favorite tools is using poshmark (online marketplace where users can buy and sell new and secondhand fashion).
#  -> An important question to answer could be whether the seller will accept an offer for an item.
#   Predictors: 1. difference of offer and sale price, 2. amount of time item has been on sale, 3. how many repeated items are already being sold, 4. If the item is new with tags or gently used, 5. If an offer has already been made on the item

#2.2

# load package recommended to get SVM package
library(kernlab)

# read data & name variable df
df <- read.table("credit_card_data.txt")
View(df)

# 1.
# Use ksvm, "x" = last column aka binary response variable to train support vector machine with scaling
ksvmo <- ksvm(V11~.,data=df, C = 120, scaled=TRUE, kernel = "vanilladot", type= "C-svc")
ksvmo

# predicting using the test data
classify <- predict(ksvmo,df[,1:10])
classify

# get accuracy of model
ans <- sum(classify==df[,11])/nrow(df)
ans

# 2.3
#load k nearest n. class
library(kknn)

for (i in 1:nrow(df)){
# Use kknn package for model with scaling
# for sake of time we will do k=30
kknnm <- kknn(V11~~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,df[-i,],df[i,],k=30, scale = TRUE) # use scaled data

# predicting using the test data
predi2 <- predict(kknm,df[,1:10])

# get accuracy of kknn model
ans2 <- sum(kknnm == df[,11])/nrow(df)}
