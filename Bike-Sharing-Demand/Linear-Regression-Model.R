# Linear regression for bike sharing project on Kaggle
# https://www.kaggle.com/c/bike-sharing-demand
# My github for Bike share
# https://github.com/Mixidiot/Kaggle-playground/tree/master/Bike-Sharing-Demand 

# 
rm(list = ls()); gc()

# Libraries
library(RCurl)
library(dplyr)
library(ggplot2)

# Reading data
data.url <- "https://raw.githubusercontent.com/Mixidiot/Kaggle-playground/master/Bike-Sharing-Demand/data/train.csv"
bike.df <- read.csv(text = getURL(data.url),
                    header=T)

# Check out data
head(bike.df)

# Data cleaning and feature engineering
bike.df$datetime <- as.POSIXct(bike.df$datetime, format = "%Y-%m-%d %H:%M:%S")
bike.df$date <- as.POSIXct(bike.df$datetime)

bike.df$hour <- sapply(as.POSIXct(bike.df$datetime), function(x) {format(x, "%H")})
bike.df$hour <- as.numeric(bike.df$hour)
# bike.df$hour <- sapply(bike.df$hour, asnumeric)

# Exploratory Data Analysis

# Creating scatter plot
temp.count.pl <- ggplot(bike.df, aes(temp, count)) + geom_point(alpha = 0.3, aes(colour = temp)) 
date.count.pl <- ggplot(bike.df, aes(date, count)) + geom_point(alpha = 0.5, aes(colour = temp))

date.count.pl + scale_colour_continuous(low = "#55D8CE", high = "#FF6E2E") + theme_bw()

# Correlations
cor(bike.df$temp, bike.df$count)
# cor(bike.df[, c("temp", "count")])

# Season as factor
bike.df$season <- as.factor(bike.df$season)

# Further customised scatter plot
ggplot(bike.df, aes(season, count)) + geom_boxplot(aes(colour = season))

# Scatterplot
hour.count.pl.d1 <- (ggplot(filter(bike.df, workingday == 1), 
                           aes(hour, count)) 
                     + geom_point(position = position_jitter(w = 1,
                                                             h = 0),
                                  (aes(colour = temp)),
                                  alpha = 0.5))

hour.count.pl.d1 + scale_colour_gradientn(colours = c("dark blue",
                                                      "blue",
                                                      "light blue",
                                                      "light green", 
                                                      "yellow", 
                                                      "orange",
                                                      "red")) + theme_bw()

# Non-working days
hour.count.pl.d0 <- (ggplot(filter(bike.df, workingday == 0), 
                            aes(hour, count)) 
                     + geom_point(position = position_jitter(w = 1,
                                                             h = 0),
                                  (aes(colour = temp)),
                                  alpha = 0.5))

hour.count.pl.d0 + scale_colour_gradientn(colours = c("dark blue",
                                                      "blue",
                                                      "light blue",
                                                      "light green", 
                                                       "yellow", 
                                                      "orange",
                                                      "red")) + theme_bw()


# Model building
temp.model <- lm(count ~ temp, bike.df)

summary(temp.model)

# How many bike rental counts at 25 degree C?
6.0462 + 9.17*25 # 235.2962
temp.test <- data.frame(temp = 25)
predict(temp.model, temp.test)

# model minus certain features
model <- lm(count ~ . - casual - registered - datetime - atemp, bike.df)
summary(model)

# this model does not account for seasonality
# not a good fit for this question