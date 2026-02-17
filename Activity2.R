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

#Question 1: 

#creates a list of each river name
flood_names <- unique(floods$names)

#iterates through each river to create a plot of its stream stage data
for(x in 1:4) {
  sub_df <- floods %>%
    filter(names == flood_names[x])
  
  plot(sub_df$dateF, sub_df$gheight.ft, type="b", pch=19, xlab="Date",
       ylab = "Stage height (ft)", main = flood_names[x], )  
}


#Question 2:

#Creates df of each rivers earliest action reporting
earliest.action <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft - action.ft >= 0) %>%
  summarise(earliest_action = min(dateF)) 
earliest.action

#Creates df of each rivers earliest flood reporting
earliest.flood <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft - flood.ft >= 0) %>%
  summarise(earliest_flood = min(dateF)) 
earliest.flood

#Creates df of each rivers earliest moderate reporting
earliest.moderate <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft - moderate.ft >= 0) %>%
  summarise(earliest_moderate = min(dateF))
earliest.moderate

#Creates df of each rivers earliest major reporting
earliest.major <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft - major.ft >= 0) %>%
  summarise(earliest_major = min(dateF))
earliest.major

#joins all the earliest dfs into one
earliest_df <- left_join(earliest.action,
                          earliest.flood, 
                          by="names") %>%
  left_join(., 
            earliest.moderate, 
            by="names") %>%
  left_join(.,
            earliest.major,
            by="names")

earliest_df

#Question 3:

#finds the greatest difference above its major category for each river 
heighest <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > major.ft) %>%
  summarize(greatest_diff = max(gheight.ft - major.ft))

#sorts the data frame in descending order, putting the river with the greatest difference at the top
heighest <- heighest[order(heighest$greatest_diff, decreasing = TRUE),]

#prints the first observation, the river with the greatest difference
print(cat("THe river with the highest stage above its major level is ", 
          heighest$names[1], "with ", heighest$greatest_diff[1], "ft."))

#Question 4:

#example use of select function: selecting the name column and columns ending with .ft
select_df <- select(floods, names, ends_with(".ft"))

#example use of if else function: adding column that labels the observation as dangerous
#if the gheight exceeds the major value, and not dangerous otherwise
select_df$classification <- ifelse(select_df$gheight.ft>=select_df$major.ft, "dangerous", "not dangerous")

#example use of the hist function: histogram of gheights
hist(select_df$gheight.ft, col = "orange", main = "Histogram", 
     xlab = "gheight in ft")
