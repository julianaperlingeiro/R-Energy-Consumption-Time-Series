#### Project: Energy Consumption 
#### Autor: Juliana Perlingeiro 
#### Date: 04/03/19 

## Packages --------------------------------------------------------

## install.packages("RMySQL")
## install.packages("DBI")
## install.packages("lubridate")
## install.packages("plotly")
## install.packages ("ggfortify")
## install.packages("forecast")

## Libraries -------------------------------------------------------

library(RMySQL)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(anytime)
library(plotly)
library(ggfortify)
library(forecast)

## Create a DB connection -----------------------------------------

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='35.239.91.216')

## List of the tables contained in the DB -------------------------

dbListTables(con)

## TRAINING with IRIS ---------------------------------------------

## List of the attributes contained in the DB ---------------------

dbListFields(con,'iris')

## Specifying all attributes to my new DB
## Use asterisk to specify all attributes for download

irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## specifying select attributes to my new DB
## Use attribute names to specify specific attributes for download

irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## End training with IRIS -----------------------------------------------

## Checking attributes in the table yr_2006 to 2010 ----------------------------

dbListFields(con,'yr_2006')
dbListFields(con,'yr_2007')
dbListFields(con,'yr_2008')
dbListFields(con,'yr_2009')
dbListFields(con,'yr_2010')

## specifying select attributes to my new DB by year -----------------------------

yr_2006 <- dbGetQuery(con, "SELECT Date, 
                                   Time, 
                                   Sub_metering_1, 
                                   Sub_metering_2, 
                                   Sub_metering_3,
                                   Global_active_power FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, 
                                   Time, 
                                   Sub_metering_1, 
                                   Sub_metering_2, 
                                   Sub_metering_3, 
                                   Global_active_power FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, 
                                   Time, 
                                   Sub_metering_1, 
                                   Sub_metering_2, 
                                   Sub_metering_3, 
                                   Global_active_power FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, 
                                   Time, 
                                   Sub_metering_1, 
                                   Sub_metering_2, 
                                   Sub_metering_3, 
                                   Global_active_power FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, 
                                   Time, 
                                   Sub_metering_1, 
                                   Sub_metering_2, 
                                   Sub_metering_3, 
                                   Global_active_power FROM yr_2010")

## Str, Summary, Head and Tail 2006 -------------------------------------------------

str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

## Str, Summary, Head and Tail 2007 -------------------------------------------------

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

## Str, Summary, Head and Tail 2008 -------------------------------------------------

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

## Str, Summary, Head and Tail 2009 -------------------------------------------------

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

## Str, Summary, Head and Tail 2010 -------------------------------------------------

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

## Create New Data Frame --------------------------------------------------------

newDF <- bind_rows(yr_2007, yr_2008, yr_2009)

## Str, Summary, Head and Tail New Data Frame -----------------------------------

str(newDF)
summary(newDF)
head(newDF)
tail(newDF)

## Pre Prossessing --------------------------------------------------------------

## Transform Data in Data file 

newDF$Date <- as.Date(newDF$Date)

## Combine Date and Time attribute values in a new attribute column -------------

newDF$DateTime <-paste(newDF$Date,newDF$Time)

## Convert DateTime from POSIXlt to POSIXct ------------------------------------

newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
head(newDF)

## Add the time zone -----------------------------------------------------------

attr(newDF$DateTime, "tzone") <- "Europe/Paris"

## # Inspect the data types

str(newDF)

## Creating attributes for different time periods ------------------------------------------

newDF$Year <- year(newDF$DateTime) 
newDF$Quarter <- quarter(newDF$DateTime) 
newDF$Month <- month(newDF$DateTime)
newDF$Week <- week(newDF$DateTime) 
newDF$Weekdays <- weekdays(newDF$DateTime) 
newDF$Day <- day(newDF$DateTime) 
newDF$Hour <- hour(newDF$DateTime)
newDF$Minute <- minute(newDF$DateTime)


## Change year, quarter, month, week, weekday and day into factors ----------------------------- 

newDF$Year <- as.factor(newDF$Year)
newDF$Quarter <- as.factor(newDF$Quarter)
newDF$Month <- as.factor(newDF$Month)
newDF$Week <- as.factor(newDF$Week)
newDF$Weekdays <- as.factor(newDF$Weekdays)
newDF$Day <- as.factor(newDF$Day)

## Change Time, Sub_metering 1, sub_metering 2 and Global Active Power as numeric -----------------

newDF$Sub_metering_1<-as.numeric(newDF$Sub_metering_1)

newDF$Sub_metering_2<-as.numeric(newDF$Sub_metering_2)

newDF$Global_active_power<-as.numeric(newDF$Global_active_power)

## Delete "Date" and "Time" ------------------------------------------------

newDF$Date <- NULL

newDF$Time <- NULL

summary(newDF)

## Energy not measured by the submeters Sub metering 4 + GAP ---------------

newDF <- newDF %>%
  mutate(Sub_metering_4 = Global_active_power*(1000/60) -
           Sub_metering_1 -
           Sub_metering_2 -
           Sub_metering_3) 

summary(newDF)

newDF1 <- 
  newDF %>%
  na.omit(newDF)

summary(newDF1)


## Plot Sub-Metering 1 ----------------------------------------

plot(newDF1$Sub_metering_1)

## Subset the second week of 2008 - All Observations ------------------------

houseWeek <- filter(newDF1, Year == 2008 & Week == 2)

## Plot subset houseWeek ---------------------------------------------------

plot(houseWeek$Sub_metering_1)


## Subset the 9th day of January 2008 - All observations --------------------

houseDay <- filter(yourData, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1 09/01/08 ------------------------------------------------

plot_ly(houseDay, 
        x = ~houseDay$DateTime, 
        y = ~houseDay$Sub_metering_1, 
        type = 'scatter', 
        mode = 'lines')


## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations ------------- 

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency ------------------------------

houseDay10 <- filter(newDF1, 
                     Year == 2008 & 
                     Month == 1 & 
                     Day == 9 & 
                     (Minute == 0  | 
                      Minute == 10 | 
                      Minute == 20 | 
                      Minute == 30 | 
                      Minute == 40 | 
                      Minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency ------------

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Create a visualization with plotly for a Week of your choosing 
## Use all three sub-meters and make sure to label 
## Experiment with granularity 

## Subset the 34 week of 2008 - All Observations ------------------------

houseWeek_1 <- filter(newDF1, Year == 2008 & Week == 34 )

## Plot sub-meter 1, 2 and 3 with title, legend and labels - week 34 year 2008 -----------

plot_ly(houseWeek_1, x = ~houseWeek_1$DateTime, y = ~houseWeek_1$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek_1$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek_1$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 34 Week, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset 1o Quarter 2009 ---------------------------------------------------------------

housequarter <-filter(newDF1, Year == 2009 & Quarter == 1)

## Plot sub-meter 1, 2 and 3 - 1o quarter 2009 ------------------------------------------

plot_ly(housequarter, x = ~housequarter$DateTime, y = ~housequarter$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~housequarter$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~housequarter$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 1o quarter, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset to one observation per week on Mondays ------------------------------------------
## at 8:00pm for 2007, 2008 and 2009

house070809weekly <- filter(newDF1, 
                            Weekdays == 'Monday' & 
                            Hour == 20 & 
                            Minute == 1)

## Create TS object with SubMeter3 - Frequency 52 -----------------------------------------

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, 
                         frequency=52, 
                         start=c(2007,1))


## Plot sub-meter 3 with autoplot --------------------------------------------------------

autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color ------------------------------------

autoplot(tsSM3_070809weekly, 
         ts.colour = 'red', 
         xlab = "Time", 
         ylab = "Watt Hours", 
         main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts ---------------------------------------------------------

plot.ts(tsSM3_070809weekly)

## Apply time series linear regression to the sub-meter 3 
## Ts object and use summary to obtain R2 and RMSE from the model you built

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3 ----------------------------------------------------
## Forecast ahead 20 time periods 

forecastfitSM3 <- forecast(fitSM3, h=20)

## Plot the forecast for sub-meter 3 ------------------------------------------------------

plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90 ---------------------------

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
summary(forecastfitSM3c)

## Plot sub-meter 3 forecast, limit y and add labels --------------------------------------

plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

## Decompose Sub-meter 3 into trend, seasonal and remainder -------------------------------

components070809SM3weekly <- decompose(tsSM3_070809weekly)

## Plot decomposed sub-meter 3 ------------------------------------------------------------

plot(components070809SM3weekly)

## Check summary statistics for decomposed sub-meter 3 ------------------------------------

summary(components070809SM3weekly)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot -------------

tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again ----------------------------------- 
## Note the very, very small scale for Seasonal

plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot ---------------------------------------------

tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)

plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot -----------------------------------------------------------

tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)

plot(tsSM3_HW070809for, ylim = c(0, 20), 
                        ylab= "Watt-Hours", 
                        xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels ---------------------------------

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))

## Plot only the forecasted area -----------------------------------------------------------

plot(tsSM3_HW070809forC, ylim = c(0, 20), 
                         ylab= "Watt-Hours", 
                         xlab="Time - Sub-meter 3", 
                         start(2010))

