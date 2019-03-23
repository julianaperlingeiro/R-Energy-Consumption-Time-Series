#### Project: Energy Consumption 
#### Autor: Juliana Perlingeiro 
#### Date: 04/03/19 


## Dataset -------------------------------------------------------------------

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


## Average per Year ----------------------------------------

AvgYearAll <- newDF1 %>%
  group_by(Year) %>% 
  filter(Year != 2010) %>% 
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power) * (1000/60))


## Average per Month --------------------------------------

AvgMonthAll <- newDF1 %>%
  filter(Year != 2010) %>% 
  group_by(Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))


## Subsets monthly averages per YEAR ---------------------------------------
## Calculating the average per MONTH ---------------------------------------

AvgYearMonthAll <- newDF1 %>%
  filter(Year != 2010) %>% 
  group_by(Year, Month) %>%
  summarise(meanSub_1 = mean(Sub_metering_1),
            meanSub_2 = mean(Sub_metering_2),
            meanSub_3 = mean(Sub_metering_3),
            meanSub_4 = mean(Sub_metering_4),
            meanGAP = mean(Global_active_power)* (1000/60))

# 2007
AvgMonth2007 <- AvgYearMonthAll %>%
  filter(Year == 2007)

# 2008
AvgMonth2008 <- AvgYearMonthAll %>%
  filter(Year == 2008) 

# 2009
AvgMonth2009 <- AvgYearMonthAll %>%
  filter(Year == 2009) 

## Plot sub meters 1,2,3 e 4 + GAP "Years" --------------------------------------------------------


plot_ly(AvgYearAll, x = ~AvgYearAll$Year, y = ~AvgYearAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanSub_2, name = 'Laundry Room', mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanSub_4, name = 'Unspecified Energy', mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanGAP, name = 'GAP', mode = 'lines + markers') %>%
  layout(title = "Power Consumption by Year",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (watt-hours)"))


## Plot sub meters 1,2,3 e 4 + GAP "Month" --------------------------------------------------------


plot_ly(AvgMonthAll, x = ~AvgMonthAll$Month, y = ~AvgMonthAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines + markers') %>%
  add_trace(y = ~AvgMonthAll$meanSub_2, name = 'Laundry Room', mode = 'lines + markers') %>%
  add_trace(y = ~AvgMonthAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanSub_4, name = 'Unspecified Energy', mode = 'lines + markers') %>%
  add_trace(y = ~AvgYearAll$meanGAP, name = 'GAP', mode = 'lines + markers') %>%
  layout(title = "Power Consumption by Month",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (watt-hours)"))


## Plot sub meters 1,2,3,4 + GAP "Week" -----------------------------------------------------------------


plot_ly(AvgWeekAll, x = ~AvgWeekAll$Week, y = ~AvgWeekAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines+markers') %>%
  add_trace(y = ~AvgWeekAll$meanSub_2, name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~AvgWeekAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines+markers') %>%
  add_trace(y = ~AvgWeekAll$meanSub_4, name = 'Unspecified Energy', mode = 'lines+markers') %>%
  add_trace(y = ~AvgWeekAll$meanGAP, name = 'GAP', mode = 'lines+ markers') %>%
  layout(title = "Power Consumption by Week",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Power (watt-hours)"))


## Plot sub meters 1,2,3,4 + GAP "Day" -----------------------------------------------------

plot_ly(AvgDayAll, x = ~AvgDayAll$Weekdays, y = ~AvgDayAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines+markers') %>%
  add_trace(y = ~AvgDayAll$meanSub_2, name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~AvgDayAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines+markers') %>%
  add_trace(y = ~AvgDayAll$meanSub_4, name = 'Unspecified Energy', mode = 'lines+markers') %>%
  add_trace(y = ~AvgDayAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Day",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Power (watt-hours)"))


## Plot sub meters 1,2,3,4 + GAP "Hour" -----------------------------------------------------

plot_ly(AvgHourAll, x = ~AvgHourAll$Hour, y = ~AvgHourAll$meanSub_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines+markers') %>%
  add_trace(y = ~AvgHourAll$meanSub_2, name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~AvgHourAll$meanSub_3, name = 'Water Heater & AC', mode = 'lines+markers') %>%
  add_trace(y = ~AvgHourAll$meanSub_4, name = 'Other Appliances', mode = 'lines+markers') %>%
  add_trace(y = ~AvgHourAll$meanGAP, name = 'GAP', mode = 'lines') %>%
  layout(title = "Power Consumption by Hour",
         xaxis = list(title = "Hour"),
         yaxis = list (title = "Power (watt-hours)"))





                 

