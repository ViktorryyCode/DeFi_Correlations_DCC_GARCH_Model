getwd() 

library(zoo)
library(PortfolioAnalytics)
library(xts)
library(rugarch)
library(rmgarch)
library(quantmod)

# import data from csv
SP500PriceDataUNIX <- read.csv("SP500PriceDataUNIX.csv")
US10YPriceDataUNIX <- read.csv("US10YPriceDataUNIX.csv")
GoldPriceDataUNIX <- read.csv("GoldPriceDataUNIX.csv")
OilPriceDataUNIX <- read.csv("OilPriceDataUNIX.csv")
DXYPriceDataUNIX <- read.csv("DXYPriceDataUNIX.csv")
BTCPriceDataUNIX <- read.csv("BTCPriceDataUNIX.csv")
ETHPriceDataUNIX <- read.csv("ETHPriceDataUNIX.csv")
DeFiIndexPriceDataUNIX <- read.csv("DeFiIndexPriceDataUNIX.csv")
USTPriceDataUNIX <- read.csv("USTPriceDataUNIX.csv")
LUNAPriceDataUNIX <- read.csv("LUNAPriceDataUNIX.csv")


# convert UNIX to POSIXct date-time object
SP500PriceDataUNIX[, 1] <- as.POSIXct(SP500PriceDataUNIX[, 1],
                                      origin = "1970-01-01",
                                      tz = "UTC")
US10YPriceDataUNIX[, 1] <- as.POSIXct(US10YPriceDataUNIX[, 1],
                                      origin = "1970-01-01",
                                      tz = "UTC")
GoldPriceDataUNIX[, 1] <- as.POSIXct(GoldPriceDataUNIX[, 1],
                                     origin = "1970-01-01",
                                     tz = "UTC")
OilPriceDataUNIX[, 1] <- as.POSIXct(OilPriceDataUNIX[, 1],
                                    origin = "1970-01-01",
                                    tz = "UTC")
DXYPriceDataUNIX[, 1] <- as.POSIXct(DXYPriceDataUNIX[, 1],
                                    origin = "1970-01-01",
                                    tz = "UTC")
BTCPriceDataUNIX[, 1] <- as.POSIXct(BTCPriceDataUNIX[, 1],
                                    origin = "1970-01-01",
                                    tz = "UTC")
ETHPriceDataUNIX[, 1] <- as.POSIXct(ETHPriceDataUNIX[, 1],
                                    origin = "1970-01-01",
                                    tz = "UTC")
DeFiIndexPriceDataUNIX[, 1] <- as.POSIXct(DeFiIndexPriceDataUNIX[, 1],
                                          origin = "1970-01-01",
                                          tz = "UTC")
USTPriceDataUNIX[, 1] <- as.POSIXct(USTPriceDataUNIX[, 1],
                                    origin = "1970-01-01",
                                    tz = "UTC")
LUNAPriceDataUNIX[, 1] <- as.POSIXct(LUNAPriceDataUNIX[, 1],
                                     origin = "1970-01-01",
                                     tz = "UTC")

# reformat date-time object to "YYYY-MM-DD HH:MM"
SP500PriceDataUNIX[, 1] <- format(SP500PriceDataUNIX[, 1],
                                  format = "%d-%m-%Y %H:%M")
US10YPriceDataUNIX[, 1] <- format(US10YPriceDataUNIX[, 1],
                                  format = "%d-%m-%Y %H:%M")
GoldPriceDataUNIX[, 1] <- format(GoldPriceDataUNIX[, 1],
                                 format = "%d-%m-%Y %H:%M")
OilPriceDataUNIX[, 1] <- format(OilPriceDataUNIX[, 1],
                                format = "%d-%m-%Y %H:%M")
DXYPriceDataUNIX[, 1] <- format(DXYPriceDataUNIX[, 1],
                                format = "%d-%m-%Y %H:%M")
BTCPriceDataUNIX[, 1] <- format(BTCPriceDataUNIX[, 1],
                                format = "%d-%m-%Y %H:%M")
ETHPriceDataUNIX[, 1] <- format(ETHPriceDataUNIX[, 1],
                                format = "%d-%m-%Y %H:%M")
DeFiIndexPriceDataUNIX[, 1] <- format(DeFiIndexPriceDataUNIX[, 1],
                                      format = "%d-%m-%Y %H:%M")
USTPriceDataUNIX[, 1] <- format(USTPriceDataUNIX[, 1],
                                format = "%d-%m-%Y %H:%M")
LUNAPriceDataUNIX[, 1] <- format(LUNAPriceDataUNIX[, 1],
                                 format = "%d-%m-%Y %H:%M")

# convert first column to a POSIXct date-time object
SP500PriceDataUNIX[, 1] <- as.POSIXct(SP500PriceDataUNIX[, 1],
                                      format = "%d-%m-%Y %H:%M")
US10YPriceDataUNIX[, 1] <- as.POSIXct(US10YPriceDataUNIX[, 1],
                                      format = "%d-%m-%Y %H:%M")
GoldPriceDataUNIX[, 1] <- as.POSIXct(GoldPriceDataUNIX[, 1],
                                     format = "%d-%m-%Y %H:%M")
OilPriceDataUNIX[, 1] <- as.POSIXct(OilPriceDataUNIX[, 1],
                                    format = "%d-%m-%Y %H:%M")
DXYPriceDataUNIX[, 1] <- as.POSIXct(DXYPriceDataUNIX[, 1],
                                    format = "%d-%m-%Y %H:%M")
BTCPriceDataUNIX[, 1] <- as.POSIXct(BTCPriceDataUNIX[, 1],
                                    format = "%d-%m-%Y %H:%M")
ETHPriceDataUNIX[, 1] <- as.POSIXct(ETHPriceDataUNIX[, 1],
                                    format = "%d-%m-%Y %H:%M")
DeFiIndexPriceDataUNIX[, 1] <- as.POSIXct(DeFiIndexPriceDataUNIX[, 1],
                                          format = "%d-%m-%Y %H:%M")
USTPriceDataUNIX[, 1] <- as.POSIXct(USTPriceDataUNIX[, 1],
                                    format = "%d-%m-%Y %H:%M")
LUNAPriceDataUNIX[, 1] <- as.POSIXct(LUNAPriceDataUNIX[, 1],
                                     format = "%d-%m-%Y %H:%M")

# convert data frame to xts object
xtsSP500PriceData <- as.xts(SP500PriceDataUNIX[,-1],
                            order.by = SP500PriceDataUNIX[, 1])
xtsUS10YPriceData <- as.xts(US10YPriceDataUNIX[,-1],
                            order.by = US10YPriceDataUNIX[, 1])
xtsGoldPriceData <- as.xts(GoldPriceDataUNIX[,-1],
                           order.by = GoldPriceDataUNIX[, 1])
xtsOilPriceData <- as.xts(OilPriceDataUNIX[,-1],
                          order.by = OilPriceDataUNIX[, 1])
xtsDXYPriceData <- as.xts(DXYPriceDataUNIX[,-1],
                          order.by = DXYPriceDataUNIX[, 1])
xtsBTCPriceData <- as.xts(BTCPriceDataUNIX[,-1],
                          order.by = BTCPriceDataUNIX[, 1])
xtsETHPriceData <- as.xts(ETHPriceDataUNIX[,-1],
                          order.by = ETHPriceDataUNIX[, 1])
xtsDeFiIndexPriceData <- as.xts(DeFiIndexPriceDataUNIX[,-1],
                                order.by = DeFiIndexPriceDataUNIX[, 1])
xtsUSTPriceData <- as.xts(USTPriceDataUNIX[,-1],
                          order.by = USTPriceDataUNIX[, 1])
xtsLUNAPriceData <- as.xts(LUNAPriceDataUNIX[,-1],
                           order.by = LUNAPriceDataUNIX[, 1])

# re name first column of time series price data
colnames(xtsSP500PriceData)[1] <- "SP500p"
colnames(xtsUS10YPriceData)[1] <- "US10Yp"
colnames(xtsGoldPriceData)[1] <- "Goldp"
colnames(xtsOilPriceData)[1] <- "Oilp"
colnames(xtsDXYPriceData)[1] <- "DXYp"
colnames(xtsBTCPriceData)[1] <- "BTCp"
colnames(xtsETHPriceData)[1] <- "ETHp"
colnames(xtsDeFiIndexPriceData)[1] <- "DeFiIndexp"
colnames(xtsUSTPriceData)[1] <- "USTp"
colnames(xtsLUNAPriceData)[1] <- "LUNAp"


# calculate returns
SP500returns = CalculateReturns(xtsSP500PriceData)
US10Yreturns = CalculateReturns(xtsUS10YPriceData)
Goldreturns = CalculateReturns(xtsGoldPriceData)
Oilreturns = CalculateReturns(xtsOilPriceData)
DXYreturns = CalculateReturns(xtsDXYPriceData)
BTCreturns = CalculateReturns(xtsBTCPriceData)
ETHreturns = CalculateReturns(xtsETHPriceData)
DeFiIndexreturns = CalculateReturns(xtsDeFiIndexPriceData)
USTreturns = CalculateReturns(xtsUSTPriceData)
LUNAreturns = CalculateReturns(xtsLUNAPriceData)

# rename col from price to returns
colnames(SP500returns)[1] <- "SP500r"
colnames(US10Yreturns)[1] <- "US10Yr"
colnames(Goldreturns)[1] <- "Goldr"
colnames(Oilreturns)[1] <- "Oilr"
colnames(DXYreturns)[1] <- "DXYr"
colnames(BTCreturns)[1] <- "BTCr"
colnames(ETHreturns)[1] <- "ETHr"
colnames(DeFiIndexreturns)[1] <- "DeFiIndexr"
colnames(USTreturns)[1] <- "USTr"
colnames(LUNAreturns)[1] <- "LUNAr"

# replace blanks in first column of returns 
SP500returns$SP500r[1] <- ifelse(is.na(SP500returns$SP500r[1]),
                                 0.00, SP500returns$SP500r[1])
US10Yreturns$US10Yr[1] <- ifelse(is.na(US10Yreturns$US10Yr[1]),
                                 0.00, US10Yreturns$US10Yr[1])
Goldreturns$Goldr[1] <- ifelse(is.na(Goldreturns$Goldr[1]),
                               0.00, Goldreturns$Goldr[1])
Oilreturns$Oilr[1] <- ifelse(is.na(Oilreturns$Oilr[1]),
                             0.00, Oilreturns$Oilr[1])
DXYreturns$DXYr[1] <- ifelse(is.na(DXYreturns$DXYr[1]),
                             0.00, DXYreturns$DXYr[1])
BTCreturns$BTCr[1] <- ifelse(is.na(BTCreturns$BTCr[1]),
                             0.00, BTCreturns$BTCr[1])
ETHreturns$ETHr[1] <- ifelse(is.na(ETHreturns$ETHr[1]),
                             0.00, ETHreturns$ETHr[1])
DeFiIndexreturns$DeFiIndexr[1] <- ifelse(is.na(DeFiIndexreturns$DeFiIndexr[1]),
                                         0.00, DeFiIndexreturns$DeFiIndexr[1])
USTreturns$USTr[1] <- ifelse(is.na(USTreturns$USTr[1]),
                             0.00, USTreturns$USTr[1])
LUNAreturns$LUNAr[1] <- ifelse(is.na(LUNAreturns$LUNAr[1]),
                               0.00, LUNAreturns$LUNAr[1])

# plot returns
plot(SP500returns)
plot(US10Yreturns)
plot(Goldreturns)
plot(Oilreturns)
plot(DXYreturns)
plot(BTCreturns)
plot(ETHreturns)
plot(DeFiIndexreturns)
plot(USTreturns)
plot(LUNAreturns)

str(LUNAreturns)

# DCC MGARCH Model 
# Consolidate data into data frame for multivariate model

rX <- data.frame(SP500returns, US10Yreturns,
                 Goldreturns, Oilreturns,
                 DXYreturns, 
                 BTCreturns, ETHreturns,
                 DeFiIndexreturns, USTreturns,
                 LUNAreturns)

names (rX) [1] <- "SP500r"
names (rX) [2] <- "US10Yr"
names (rX) [3] <- "GOLDr"
names (rX) [4] <- "OILr"
names (rX) [5] <- "DXYr"
names (rX) [6] <- "BTCr"
names (rX) [7] <- "ETHr"
names (rX) [8] <- "DEFIINDEXr"
names (rX) [9] <- "USTr"
names (rX) [10] <- "LUNAr"

# Model Specification 

ug_spec = ugarchspec()

# Review Overview

ug_spec

# Change Mean Model to AR(1) Model

ug_spec <- ugarchspec(mean.model = list (armaOrder = c(1,0)))

# Base Univariate GARCH Model Estimation for SP500 to be replicated

ugfit = ugarchfit(spec = ug_spec, data = SP500returns)

ugfit

names(ugfit@model)

names(ugfit@fit)

ugfit@fit$coef

ug_var <- ugfit@fit$var # to save estimated conditional variances
ug_res2 <- (ugfit@fit$residuals)^2 # to save estimated square residuals


# DCC Model Preparation 

uspec.n = multispec(replicate(10, ugarchspec(mean.model = list(armaOrder = c(1,0)))))

# Results of estimated parameters for all 10 models are saved in multf, prior to DCC specification
multf = multifit(uspec.n, rX)

# enter multf to consol to view estimated alpha and beta parameters per asset

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')


# Multiariate DCC GARCH Model Estimation

# fit1 contains DCC MGARCH results of estimated parameters
fit1 = dccfit(spec1, data = rX, fit.control = list(eval.se = TRUE), fit = multf)

# Covariance and Correlation Matrices

# Extract model based time varying covariance (arrays) and correlation matrices
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix

dim(cor1) # output is 10 10 1442 meaning we have a 10 by 10 matrix for each of 1442 observation periods

# format as time series of dynamic conditional correlation for selected assets 

DCC <- "Conditional Correlation"

# SP500
# SP500 and UST
cor_SP500UST <- cor1[1,9,]   # first row, ninth column, blank last dimension implies we want all observation periods
cor_SP500UST <- as.xts(cor_SP500UST)  # redefine as time series for plotting
colnames(cor_SP500UST)[1] <- DCC

# SP500 and LUNA
cor_SP500LUNA <- cor1[1,10,]   # row, column, all observation periods
cor_SP500LUNA <- as.xts(cor_SP500LUNA)  # redefine as time series
colnames(cor_SP500LUNA)[1] <- DCC
  
# SP500 and BTC
cor_SP500BTC <- cor1[1,6,]   # row, column, all observation periods
cor_SP500BTC <- as.xts(cor_SP500BTC)  # redefine as time series
colnames(cor_SP500BTC)[1] <- DCC

# SP500 and ETH
cor_SP500ETH <- cor1[1,7,]   # row, column, all observation periods
cor_SP500ETH <- as.xts(cor_SP500ETH)  # redefine as time series
colnames(cor_SP500ETH)[1] <- DCC


# US10Y
# US10Y and UST
cor_US10YUST <- cor1[2,9,]   # row, column, all observation periods
cor_US10YUST <- as.xts(cor_US10YUST)  # redefine as time series
colnames(cor_US10YUST)[1] <- DCC

# US10Y and LUNA
cor_US10YLUNA <- cor1[2,10,]   # row, column, all observation periods
cor_US10YLUNA <- as.xts(cor_US10YLUNA)  # redefine as time series
colnames(cor_US10YLUNA)[1] <- DCC

# US10Y and BTC
cor_US10YBTC <- cor1[2,6,]   # row, column, all observation periods
cor_US10YBTC <- as.xts(cor_US10YBTC)  # redefine as time series
colnames(cor_US10YBTC)[1] <- DCC

# US10Y and ETH
cor_US10YETH <- cor1[2,7,]   # row, column, all observation periods
cor_US10YETH <- as.xts(cor_US10YETH)  # redefine as time series
colnames(cor_US10YETH)[1] <- DCC


# Gold
# Gold and UST
cor_GoldUST <- cor1[3,9,]   # row, column, all observation periods
cor_GoldUST <- as.xts(cor_GoldUST)  # redefine as time series
colnames(cor_GoldUST)[1] <- DCC

# Gold and LUNA
cor_GoldLUNA <- cor1[3,10,]   # row, column, all observation periods
cor_GoldLUNA <- as.xts(cor_GoldLUNA)  # redefine as time series
colnames(cor_GoldLUNA)[1] <- DCC

# Gold and BTC
cor_GoldBTC <- cor1[3,6,]   # row, column, all observation periods
cor_GoldBTC <- as.xts(cor_GoldBTC)  # redefine as time series
colnames(cor_GoldBTC)[1] <- DCC

# Gold and ETH
cor_GoldETH <- cor1[3,7,]   # row, column, all observation periods
cor_GoldETH <- as.xts(cor_GoldETH)  # redefine as time series
colnames(cor_GoldETH)[1] <- DCC


# Oil
# Oil and UST
cor_OilUST <- cor1[4,9,]   # row, column, all observation periods
cor_OilUST <- as.xts(cor_OilUST)  # redefine as time series
colnames(cor_OilUST)[1] <- DCC

# Oil and LUNA
cor_OilLUNA <- cor1[4,10,]   # row, column, all observation periods
cor_OilLUNA <- as.xts(cor_OilLUNA)  # redefine as time series
colnames(cor_OilLUNA)[1] <- DCC

# Oil and BTC
cor_OilBTC <- cor1[4,6,]   # row, column, all observation periods
cor_OilBTC <- as.xts(cor_OilBTC)  # redefine as time series
colnames(cor_OilBTC)[1] <- DCC

# Oil and ETH
cor_OilETH <- cor1[4,7,]   # row, column, all observation periods
cor_OilETH <- as.xts(cor_OilETH)  # redefine as time series
colnames(cor_OilETH)[1] <- DCC


# DXY
# DXY and UST
cor_DXYUST <- cor1[5,9,]   # row, column, all observation periods
cor_DXYUST <- as.xts(cor_DXYUST)  # redefine as time series
colnames(cor_DXYUST)[1] <- DCC

# DXY and LUNA
cor_DXYLUNA <- cor1[5,10,]   # row, column, all observation periods
cor_DXYLUNA <- as.xts(cor_DXYLUNA)  # redefine as time series
colnames(cor_DXYLUNA)[1] <- DCC

# DXY and BTC
cor_DXYBTC <- cor1[5,6,]   # row, column, all observation periods
cor_DXYBTC <- as.xts(cor_DXYBTC)  # redefine as time series
colnames(cor_DXYBTC)[1] <- DCC

# DXY and ETH
cor_DXYETH <- cor1[5,7,]   # row, column, all observation periods
cor_DXYETH <- as.xts(cor_DXYETH)  # redefine as time series
colnames(cor_DXYETH)[1] <- DCC


# DeFiIndex
# DeFiIndex and UST
cor_DeFiIndexUST <- cor1[8,9,]   # row, column, all observation periods
cor_DeFiIndexUST <- as.xts(cor_DeFiIndexUST)  # redefine as time series
colnames(cor_DeFiIndexUST)[1] <- DCC

# DeFiIndex and LUNA
cor_DeFiIndexLUNA <- cor1[8,10,]   # row, column, all observation periods
cor_DeFiIndexLUNA <- as.xts(cor_DeFiIndexLUNA)  # redefine as time series
colnames(cor_DeFiIndexLUNA)[1] <- DCC

# DeFiIndex and BTC
cor_DeFiIndexBTC <- cor1[8,6,]   # row, column, all observation periods
cor_DeFiIndexBTC <- as.xts(cor_DeFiIndexBTC)  # redefine as time series
colnames(cor_DeFiIndexBTC)[1] <- DCC

# DeFiIndex and ETH
cor_DeFiIndexETH <- cor1[8,7,]   # row, column, all observation periods
cor_DeFiIndexETH <- as.xts(cor_DeFiIndexETH)  # redefine as time series
colnames(cor_DeFiIndexETH)[1] <- DCC


# BTC
# BTC and UST
cor_BTCUST <- cor1[6,9,]   # row, column, all observation periods
cor_BTCUST <- as.xts(cor_BTCUST)  # redefine as time series
colnames(cor_BTCUST)[1] <- DCC

# BTC and LUNA
cor_BTCLUNA <- cor1[6,10,]   # row, column, all observation periods
cor_BTCLUNA <- as.xts(cor_BTCLUNA)  # redefine as time series
colnames(cor_BTCLUNA)[1] <- DCC

# BTC and ETH
cor_BTCETH <- cor1[6,7,]   # row, column, all observation periods
cor_BTCETH <- as.xts(cor_BTCETH)  # redefine as time series
colnames(cor_BTCETH)[1] <- DCC


# ETH
# ETH and UST
cor_ETHUST <- cor1[7,9,]   # row, column, all observation periods
cor_ETHUST <- as.xts(cor_ETHUST)  # redefine as time series
colnames(cor_ETHUST)[1] <- DCC

# ETH and LUNA
cor_ETHLUNA <- cor1[7,10,]   # row, column, all observation periods
cor_ETHLUNA <- as.xts(cor_ETHLUNA)  # redefine as time series
colnames(cor_ETHLUNA)[1] <- DCC

# crash variables
# LUNA and UST
cor_LUNAUST <- cor1[10,9,]   # row, column, all observation periods
cor_LUNAUST <- as.xts(cor_LUNAUST)  # redefine as time series
colnames(cor_LUNAUST)[1] <- DCC
