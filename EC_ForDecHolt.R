#### Project: Energy Consumption 
#### Autor: Juliana Perlingeiro 
#### Date: 04/03/19 


## Forecasting + Decompising + Holt Winters  ----------------------------------------------------------------


## Sub-meter 1,2,3,4 E GAP ---------------------------------------------------
## Make subset grouping by Year and Month ------------------------------------

AvgMonthAllTS <- newDF1 %>%
  filter(Year != 2010) %>%
  group_by(Year, Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1)*(24*(365/12)),
            meanSub_2 = mean(Sub_metering_2)*(24*(365/12)),
            meanSub_3 = mean(Sub_metering_3)*(24*(365/12)),
            meanSub_4 = mean(Sub_metering_4)*(24*(365/12)),
            meanGAP = mean(Global_active_power)* (1000/60)*(24*(365/12)))

summary(AvgMonthAllTS)

## Create TS object with sub-meter 1,2,3,4 e GAP

TSAvgMonthSM1 <- ts(AvgMonthAllTS$meanSub_1, frequency = 12, start = c(2007,1))

TSAvgMonthSM2 <- ts(AvgMonthAllTS$meanSub_2, frequency = 12, start = c(2007,1))

TSAvgMonthSM3 <- ts(AvgMonthAllTS$meanSub_3, frequency = 12, start = c(2007,1))

TSAvgMonthSM4 <- ts(AvgMonthAllTS$meanSub_4, frequency = 12, start = c(2007,1))

TSAvgMonthGAP <- ts(AvgMonthAllTS$meanGAP, frequency = 12, start = c(2007,1))



## Plot sub-meter 1,2,3,4 e GAP with autoplot ---------------------------------------------

autoplot(TSAvgMonthSM1, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 1")

autoplot(TSAvgMonthSM2, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 2")

autoplot(TSAvgMonthSM3, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 3")

autoplot(TSAvgMonthSM4, xlab = "Time", ylab = "Watt Hours", 
         main = "Sub-meter 4")

autoplot(TSAvgMonthGAP, xlab = "Time", ylab = "Watt Hours", 
         main = "GAP")



## Plot sub-meter 1,2,3,4 e GAP with plot.tsn ---------------------------------------------

plot.ts(TSAvgMonthSM1)

plot.ts(TSAvgMonthSM2)

plot.ts(TSAvgMonthSM3)

plot.ts(TSAvgMonthSM4)

plot.ts(TSAvgMonthGAP)


## Apply time series linear regression to the sub-meter 1,2,3,4 e GAP -------------------------

FitSM1 <- tslm(TSAvgMonthSM1 ~ trend + season)

FitSM2 <- tslm(TSAvgMonthSM2 ~ trend + season)

FitSM3 <- tslm(TSAvgMonthSM3 ~ trend + season)

FitSM4 <- tslm(TSAvgMonthSM4 ~ trend + season)

FitGAP <- tslm(TSAvgMonthGAP ~ trend + season)


summary(FitSM1)

summary(FitSM2)

summary(FitSM3)

summary(FitSM4)

summary(FitGAP)

## Create sub-meter 1,2,3,4 e GAP forecast ahead 36 time periods -----------------------------
## with confidence levels 80 and 90 

forecastFitSM1 <- forecast(FitSM1, h=36, level=c(80,90))
forecastFITSM2 <- forecast(FitSM2, h=36, level = c(80,90))
forecastFITSM3 <- forecast(FitSM3, h=36, level = c(80,90))
forecastFITSM4 <- forecast(FitSM4, h=36, level = c(80,90))
forecastFITGAP <- forecast(FitGAP, h=36, level = c(80,90))



## Plot sub-meter 1,2,3,4 e GAP forecast, limit y and add labels ------------------------------

plot(forecastFitSM1, ylab= "Watt-Hours", xlab="Year")
plot(forecastFITSM2, ylab = "watt-Hours" , xlab = "Year")
plot(forecastFITSM3, ylab = "watt-Hours" , xlab = "Year")
plot(forecastFITSM4, ylab = "watt-Hours" , xlab = "Year")
plot(forecastFITGAP, ylab = "watt-Hours" , xlab = "Year")



## Decompose sub-meter 1,2,3,4 e GAP into trend, seasonal and remainder ------------------------------------

componentsTSAvgMonthSM1 <- decompose(TSAvgMonthSM1)

componentsTSAvgMonthSM2 <- decompose(TSAvgMonthSM2)

componentsTSAvgMonthSM3 <- decompose(TSAvgMonthSM3)

componentsTSAvgMonthSM4 <- decompose(TSAvgMonthSM4)

componentsTSAvgMonthGAP <- decompose(TSAvgMonthGAP)



## Plot decomposed sub-meter 1,2,3,4 e GAP ------------------------------------------------------

plot(componentsTSAvgMonthSM1)

plot(componentsTSAvgMonthSM2)

plot(componentsTSAvgMonthSM3)

plot(componentsTSAvgMonthSM4)

plot(componentsTSAvgMonthGAP)


## Check summary statistics for decomposed sub-meter 1,2,3,4 e GAP ----------------------------- 

summary(componentsTSAvgMonthSM1)

summary(componentsTSAvgMonthSM2)

summary(componentsTSAvgMonthSM3)

summary(componentsTSAvgMonthSM4)

summary(componentsTSAvgMonthGAP)


## Seasonal adjusting sub-meter 1,2,3,4 e GAP
## by REMOVING the seasonal component & plot

TSAvgMonthSM1Adj <- TSAvgMonthSM1 - componentsTSAvgMonthSM1$seasonal

TSAvgMonthSM2Adj <- TSAvgMonthSM2 - componentsTSAvgMonthSM2$seasonal

TSAvgMonthSM3Adj <- TSAvgMonthSM3 - componentsTSAvgMonthSM3$seasonal

TSAvgMonthSM4Adj <- TSAvgMonthSM4 - componentsTSAvgMonthSM4$seasonal

TSAvgMonthGAPAdj <- TSAvgMonthGAP - componentsTSAvgMonthGAP$seasonal


autoplot(TSAvgMonthSM1Adj)

autoplot(TSAvgMonthSM2Adj)

autoplot(TSAvgMonthSM3Adj)

autoplot(TSAvgMonthSM4Adj)

autoplot(TSAvgMonthGAPAdj)



## Test Seasonal Adjustment by running Decompose again --------------------------------------------

plot(decompose(TSAvgMonthSM1Adj))

plot(decompose(TSAvgMonthSM2Adj))

plot(decompose(TSAvgMonthSM3Adj))

plot(decompose(TSAvgMonthSM4Adj))

plot(decompose(TSAvgMonthGAPAdj))


## Holt Winters Exponential Smoothing & Plot-----------------------------------------------------

# Create ts object that contains exponentially smoothed data 
## with no seasonality ----------------------

TSAvgMonthSM1HW <- HoltWinters(TSAvgMonthSM1Adj, beta=0.5, gamma=0.02, seasonal = "additive")

TSAvgMonthSM2HW <- HoltWinters(TSAvgMonthSM1Adj, beta=0.5, gamma=0.02, seasonal = "additive")

TSAvgMonthSM3HW <- HoltWinters(TSAvgMonthSM3Adj, beta =0.5, gamma = 0.02, seasonal = "additive")

TSAvgMonthSM4HW <- HoltWinters(TSAvgMonthSM4Adj, beta =0.5, gamma = 0.02, seasonal = "additive")

TSAvgMonthGAPHW <- HoltWinters(TSAvgMonthGAPAdj, beta =0.5, gamma = 0.02, seasonal = "additive")


plot(TSAvgMonthSM1HW)

plot(TSAvgMonthSM2HW)

plot(TSAvgMonthSM3HW)

plot(TSAvgMonthSM4HW)

plot(TSAvgMonthGAPHW)

## HoltWinters forecast & plot ----------------------------------------------------------------

TSAvgMonthSM1HWForecast <- forecast(TSAvgMonthSM1HW, h=25)

TSAvgMonthSM2HWForecast <- forecast(TSAvgMonthSM2HW, h=25)

TSAvgMonthSM3HWForecast <- forecast(TSAvgMonthSM3HW, h=25)

TSAvgMonthSM4HWForecast <- forecast(TSAvgMonthSM4HW, h=25)

TSAvgMonthGAPHWForecast <- forecast(TSAvgMonthGAPHW, h=25)




plot(TSAvgMonthSM1HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

plot(TSAvgMonthSM1HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-Meters")

plot(TSAvgMonthSM2HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

plot(TSAvgMonthSM3HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

plot(TSAvgMonthSM4HWForecast, ylab= "Watt-Hours", xlab="Time - Sub-meter 4")

plot(TSAvgMonthGAPHWForecast, ylab= "Watt-Hours", xlab="Time - GAP")


# Forecast HoltWinters with diminished confidence levels

TSAvgMonthSM1HWForecastC <- forecast(TSAvgMonthSM1HW, h=25, level=c(10,25))

TSAvgMonthSM2HWForecastC <- forecast(TSAvgMonthSM2HW, h=25, level=c(10,25))

TSAvgMonthSM3HWForecastC <- forecast(TSAvgMonthSM3HW, h=25, level=c(10,25))

TSAvgMonthSM4HWForecastC <- forecast(TSAvgMonthSM4HW, h=25, level=c(10,25))

TSAvgMonthGAPHWForecastC <- forecast(TSAvgMonthGAPHW, h=25, level=c(10,25))

# Plot only the forecasted area

plot(TSAvgMonthSM1HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

plot(TSAvgMonthSM2HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

plot(TSAvgMonthSM3HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

plot(TSAvgMonthSM4HWForecastC, ylab= "Watt-Hours", xlab="Time - Sub-meter 4", start(2010))

plot(TSAvgMonthGAPHWForecastC, ylab = "Watt-Hours", xlab="Time - GAP", start(2010))




