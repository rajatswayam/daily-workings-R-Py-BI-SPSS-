install.packages("quantmod")
library(quantmod)
# data Call out
Data_zee = getSymbols("ZEEL.NS", src = "yahoo", from = "2018-07-01", to = Sys.Date(), auto.assign = FALSE)
Data_zee

# remove all rows with any NA values from the data object
na.omit(Data_zee)

# Summary of the data
summary(Data_zee)

# Simming the data to read quickly
install.packages("skimr")
library(skimr)
skim(Data_zee)

# Structure data
str(Data_zee)
head(Data_zee)
tail(Data_zee)

# Visulatizing data
install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(xts)

# daily price
ggplot(Data_zee, aes(x = as.Date(index(Data_zee)), y = Data_zee[,6])) +
  geom_line(color = "orange") +
  xlab("Date") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "9 months") +
  ggtitle("ZEE LTD daily prices series")

# time series
y_TM <- Data_zee[, 6]
x_TM <- index(Data_zee)

plot(x = x_TM, y = y_TM, type = "l", col = "red",
     xlab = "Date", ylab = "Stock-Price", main = "Time Series Plot")

# Moving Average
TM_mm <- subset(Data_zee, index(Data_zee) >= "2018-07-01")

TM_mm10 <- rollmean(TM_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
TM_mm30 <- rollmean(TM_mm[,6], 30, fill = list(NA , NULL, NA), align = "right")

TM_mm$mm10 <- coredata(TM_mm10)
TM_mm$mm30 <- coredata(TM_mm30)

plot(index(TM_mm), TM_mm[, 6], type = "l", col = "violet",
     xlab = "Date", ylab = "Price", main = "ZEE LTD Price Series")
lines(index(TM_mm), TM_mm$mm10, col = "black")
lines(index(TM_mm), TM_mm$mm30, col = "gold")
lines(index(TM_mm), TM_mm$mm100, col = "slategrey")

ggplot(TM_mm,aes(x = index(TM_mm10))) +
  geom_line(aes(y = TM_mm[,6], color = "ZEE")) + ggtitle("ZEE LTD Moving Averages") +
  geom_line(aes(y = TM_mm$mm10, color = "MM10")) +
  geom_line(aes(y = TM_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5),panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y" ,  date_breaks = "12 months") +
  scale_color_manual("series", values = c("ZEE" = "yellow", "MM10" = "firebrick4", "MM30" = "darkcyan"))

TM_ret <- diff(log(Data_zee[,6]))
TM_ret <- TM_ret[-1,]
summary(TM_ret) 

ggplot(TM_ret, aes(x = index(TM_ret), y = TM_ret)) +
  geom_line(color = "deepskyblue4") +
  ggtitle(" ZEE LTD RETURN SERIES") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "12 months")

TM_ret17 <- subset(TM_ret, index(TM_ret) > "2023-07-01")

ggplot(TM_ret17, aes(x = index(TM_ret17), y = TM_ret17)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("ZEE LTD Returns Series 2024") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "1 months")

library(quantmod)
stocks <- c("ZEEL.NS")
stockEnv <- new.env()
symbols <- getSymbols(stocks, src='yahoo', env=stockEnv)

for (stock in ls(stockEnv)){
  chartSeries(stockEnv[[stock]], theme="white", name=stock,
              TA="addVo();addBBands();addCCI();addSMA(20, col='blue');
        addSMA(5, col='red');addSMA(50, col='black')", subset='last 30 days')    
}


