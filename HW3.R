
## Q7.1
# A situation applied for exponential smoothing could be determining the money I will spend next year.
# The data needed would be the amount of money I spend each day (excluding money transferred to savings/investments) for the past 4 years at least
# since this is when i became indepedent and spend a lot more..
# The alpha I would intially try a = 0.5 and I would probably decrease it if i needed to emphasize a longer term trend.
# However due to the fact that i get salary increases yearly, my spending also increases so the latest data is more relevant to me.

## Q7.2
# clear
rm(list = ls())
# read data & name variable df
df <- read.table("temps.txt", header=TRUE)
head(df)

# format df to get columns in date & temps
library(reshape)
library(tidyr)
df <- melt(df, id.vars = c("DAY"), value.name=("year"), variable.name = ("temps"))
df_res = df %>% unite(date, DAY, variable, sep = "-", remove = FALSE)
keeps <- c("date", "value")
head(df_res[keeps],10)

# get data into time series
ts_df <- ts(df_res[keeps][, 2], start = 1996, frequency = 123)
ts_df

# use holt winters to fit data as recommended, we will use alpha as 0.5 to focus on recent data and beta & gamma as 0.2 as recommended from documentation.
df_fit <- HoltWinters(ts_df, seasonal="multiplicative", optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1))

# plot fitted model over original time series data
plot(df_fit)

summary(df_fit)

head(df_fit$fitted)
# then we use the data to determine the seasonal factor which measures the percentage amount that on average, monthly production is above or below normal.


## 8.1
# Linear regression would be appropriate to forecast the amount of debt accumulated in a year
# some predictors include money spent on a credit card, money borrowed from family, taking a spontaneous loan for a masters degree,
# purchasing a brand new car, purchasing a home.

## 8.2 
# clear
rm(list = ls())
# read data & name variable df
df <- read.table("uscrime.txt", header=TRUE)
head(df)

# input data given for testing
df_test <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)
df_test

# lm function used for regression modeling
regress <- lm(Crime ~. ,data = df)
summary(regress)

# predict crime rate in a city with the model
pred <- predict(regress, df_test)
pred

# The result is much lower than the data given, as stated in the question this model was probably over fitted. 