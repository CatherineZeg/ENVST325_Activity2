#install necessary packages
#install.packages(c("dplyr", "lubridate"))

#load in libraries
library(dplyr)
library(lubridate)

#in class ----
#load in csv files
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv", )
streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")

#add column of stream datetime column as time series data
streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")
#extracts the year from the date
streamH$year <- year(streamH$dateF)

#create peaceH data frame of only peace river data from streamH  
peaceH <- streamH %>% 
  filter(siteID == 2295637)
# %>% pipes the date, allows you to chain a data set to a function

#plate date vs height
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

# create floods data from by joining streamH and siteInfo data frames
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common column

#mean of heights
mean(floods$gheight.ft)

#using group by 
height.ave <- floods %>% 
  group_by(names) %>% 
  summarise(mean.height = mean(gheight.ft)) 
height.ave

floods$doy <- yday(floods$dateF)

height.doy <- floods %>%
  group_by(names, doy) %>% 
  summarise(mean.height = mean(gheight.ft))

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n())

#Prompt 1

#left join:
flood_left_join <- left_join(streamH, # left table
                            siteInfo, # right table
                            by="siteID")

#right join:
flood_right_join <- right_join(streamH, # left table
                             siteInfo, # right table
                             by="siteID")

#for this data, it does not make a difference

#Prompt 2 : Parse the date for the Floods data frame.
#done

#Prompt 3: What was the earliest date that each river reached the flood stage?
earliest.flood <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(earliest = min(dateF))
earliest.flood


#HW ----

help(plot)

#Question 1: Make a separate plot of the stream stage data for each river.
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

floods %>%
  group_by(names) %>%
  plot(dateF, gheight.ft)

#Question 2:
earliest.date <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(earliest = min(dateF))
earliest.date

