# ANA-515-Assignment-3

#1
getwd()

library("dplyr")
library("tidyverse")
library("readr")
library("stringr")
library("ggplot2")

# import csv files
my_data <- read.csv("/Users/chenzhiying/Downloads/StormEvents_details-ftp_v1.0_d1996_c20220425.csv")
storm_events <- my_data

#2 - new dataset
storm_96 <- storm_events[c(18,20,7,8,9,10,16,14,15,13,17,45,46,47,48)]
#3
storm_96 <- arrange(storm_96, STATE, .by_group = FALSE)
#4
storm_96$STATE <- str_to_title(storm_96$STATE)
#5 
storm_96 <- filter(storm_96, CZ_TYPE=="C", .preserve = FALSE)
storm_96 <- subset(storm_96, select = -c(8) )
#6
storm_96$CZ_FIPS <- str_pad(storm_96$CZ_FIPS, width=3, side='left', pad='0')
storm_96$STATE_FIPS <-  str_pad(storm_96$STATE_FIPS, width=3, side='left', pad='0')
storm_96$FIP <- str_c(storm_96$STATE_FIPS, storm_96$CZ_FIPS)
#7
storm_96 <- rename_all(storm_96, tolower)
view(storm_96)
#8
data("state")
us_state <- data.frame(STATE=state.name, REGION=state.region, AREA=state.area)
#9
storm_freq <- data.frame(table(storm_96$state))
storm_freq <- rename(storm_freq,c("state"="Var1"))
merged <- merge(x=storm_freq,y=us_state,by.x="state", by.y="state")

storm_96_freq <- storm_96%>%group_by(state)%>%summarise(events = n())
view(us_state)
joined <- left_join(us_state, storm_96_freq, by = c("STATE"="state"))
view(joined)
#10
storm_plot <- ggplot(joined,aes(x=AREA,y=events))+
  geom_point(aes(color=REGION))+
  labs(x = "Land area (square miles)",
       y = "# of storm events in 1996")
storm_plot

