# UBER CASE STUDY

### Set Working Directory to appropriate folder

# setwd("F:/IIIT - B/Module 2/Uber Case Study") # setting the working directory

getwd() #checking current working directory
rm(list = ls()) # clearing the environment

### Downloading the data

# checking for files in working directory and downloading if not present

if (!file.exists("Uber Request Data.csv")){
        uber_url = "https://cdn.upgrad.com/UpGrad/temp/76b3b6a4-d87d-4e82-b1c3-3f6e10b9c076/Uber%20Request%20Data.csv"
        download.file(uber_url,"Uber Request Data.csv",mode = "wb")
        rm(uber_url)
}

### libraries used

# Assumption : All required packages are pre-installed in your R environment else use install.packages() to install these packages before running the code
# Load required libraries

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

### Loading the dataset into R

uber = fread("Uber Request Data.csv") # fread() data.table is used here.

### Initial insights of the data given

# structure of data. We can see Status, Pickup point are characters. 
str(uber) # Should convert Status, Pickup point as factors
head(uber) 

# column names of uber are with spaces. I am not so comfortable with that.
names(uber) # so, I am goint to change the column names.

# missing values in dataset

sum(is.na(uber)) # Total NAs in given data
sapply(uber,function(x){ sum(is.na(x))}) # no of missing values in each row
sapply(uber,function(x){ sum(is.na(x))/length(x)*100}) # percentage of NAs in each row

# NOTE: Logically, if there is a trip/request cancellation or no cars were available
# There is no data to fill in. So NAs are placed (assumption). So, no need to take of NAs


### DATA CLEANING

# changing columns names (just for my comfort)

names(uber)
col_names = c("Request_id", "Pickup_point", "Driver_id", "Status", 
              "Request_timestamp", "Drop_timestamp")
colnames(uber) = col_names

# Changing required variables to factors

uber$Status = as.factor(uber$Status)
uber$Pickup_point = as.factor(uber$Pickup_point)
#uber$Driver_id = as.factor(uber$Driver_id)
str(uber)


# Request_timestamp, Drop_timestamp are in different formats (with different seperators).
# So, changing them to uniform format using gsub()

uber$Request_timestamp = gsub("/","-", uber$Request_timestamp)
uber$Drop_timestamp = gsub("/","-", uber$Drop_timestamp)

# Request_timestamp, Drop_timestamp are in character format. As we know they date and times,
# we need to convert them to proper date - time format.

# There are some rows without seconds in the data point. Due which, I was not able to
# convert them to date - time columns. So, I pasted seconds as 00 in every data point
# with seconds missing.

# Seconds won't affect analysis much, so assumed it as 00. (assumption)

i = 1
while( i < length(uber$Request_timestamp)+1){
        if (nchar(uber$Request_timestamp[i]) == 14 | nchar(uber$Request_timestamp[i]) == 15){
                uber$Request_timestamp[i] = paste(uber$Request_timestamp[i], ":00", sep = "")
        }
        i = i + 1
}

uber$Request_timestamp # we can see :00 seconds are added to data points which were missing them initially

i = 1
while( i < length(uber$Drop_timestamp)+1){
        if( is.na(uber$Drop_timestamp[i])){
                #print(uber$Drop_timestamp[i])
                uber$Drop_timestamp[i] = uber$Drop_timestamp[i]
        }else if(nchar(uber$Drop_timestamp[i]) == 14 | nchar(uber$Drop_timestamp[i]) == 15){
                uber$Drop_timestamp[i] = paste(uber$Drop_timestamp[i], ":00", sep = "")
        }
        i = i + 1
}

uber$Drop_timestamp # we can see :00 seconds are added to data points which were missing them initially

# Using libridate library, I parsed the Request_timestamp, Drop_timestamp to date - time columns

uber$Request_timestamp = parse_date_time(uber$Request_timestamp, "%d-%m-%y %H:%M:%S")
uber$Drop_timestamp = parse_date_time(uber$Drop_timestamp, "%d-%m-%y %H:%M:%S")


### DERIVING METRICS

# The given data is from only one year(2016) and one month (July). So, it is of no use to derive years and months
# And they gave us data of 5 weekdays(11th july - 15 july). Instead of dates of day, lets see day names makes any sense in patterns

table(year(uber$Request_timestamp))
table(month(uber$Request_timestamp))
table(day(uber$Request_timestamp))

# Weekday of the request from the Request_timestamp
uber$req_day = wday(uber$Request_timestamp, label = TRUE)
#uber$drop_day = wday(uber$Drop_timestamp, label = TRUE)

# Hour of request from Request_timestamp
uber$req_hour = hour(uber$Request_timestamp)
#uber$drop_hour = hour(uber$Drop_timestamp)

# Trip time for the trips completed from Request_timestamp and Drop_timestamp
uber$triptime_mins = round(difftime(uber$Drop_timestamp,uber$Request_timestamp),0)

# I am segmenting the hour of request into 6 segments (4 hours timeframe each)
# Thes segments are useful to analyse the demand - supply timeslot wise.

levels = c(-1,3,7,11,15,19,23)
labels = c("Late Night (00:00 - 3:59)", "Early Morning (4:00 - 7:59)", "Morning (8:00 - 11:59)", 
           "AfterNoon (12:00 - 15:59)", "Evening (16:00 - 19:59)", "Night (20:00 - 23:59)")
uber = uber %>% mutate(req_time_slot = cut(req_hour, levels, labels = labels))
#uber = uber %>% mutate(drop_time_slot = cut(drop_hour, levels, labels = labels))

View(uber)

### Variable Analysis to understand the data and find the problems

# Pickup_point 

table(uber$Pickup_point) # the demand for cabs is more for City to Airport
ggplot(uber, aes(Pickup_point)) + geom_bar(fill = "darkgrey") + ggtitle("Demand distribution by Pickup point") +
        xlab("Pickup Point") + ylab("Count of Requests")

# Status

table(uber$Status) # Trips completed is highest between the 3. But the other two categories represent trips Failed. 
ggplot(uber, aes(Status)) + geom_bar(fill = "darkgrey") + ggtitle("Distribution of trip status") +
        xlab("Trip Status") + ylab("Count of Requests")

# Request hour

table(factor(uber$req_hour))
ggplot(uber, aes(factor(req_hour))) + geom_bar(fill = "darkgrey") # No of requests by hour

# Request timeslot

table(uber$req_time_slot)
ggplot(uber, aes(factor(req_time_slot))) + geom_bar(fill = "darkgrey") # No of requests by timeslots

# Status w.r.t Pickup_point

pickup_point = ggplot(data = uber, aes(Pickup_point, fill = Status))
pickup_point + geom_bar(position = "fill") + theme_gray() + ggtitle("Proportion of trip status by Pickup point") +
             xlab("Pickup Point") + ylab("Proportion of trip status") # + ggsave("Trip Status w.r.t pickup point.png")
# Airport to city : No Cars are available is more than Cancellation of requests
# City to Airport : Cancellation of requests is more than No cars Availabel.
pickup_point + geom_bar(position = "dodge")

# Status w.r.t Request hour 

Status_by_hour = ggplot(data = uber, aes(req_hour, fill = Status))
Status_by_hour + geom_bar(position = "fill") + ggtitle("Proportion of trip status by Request hour") +
        xlab("Hour of Request") + ylab("Proportion of trip status")
# We can observe that there is significant gap between trips completed and trips failed (Cancelled and No cars Available)


# Status w.r.t Weekday of request (req_day) and Pickup_poinr

req_day_analysis = ggplot(data = uber, aes(Pickup_point))
req_day_analysis + geom_bar(aes(fill = Status)) + facet_grid(.~req_day) +
        ggtitle("Trips Analysis by day and pickup point")
# we can see there is more demond for city to airport on thursday and friday.
#ggsave("Analysis of trip status by weekday and pickup point.png")

# Failed trips (Status = No Cars Available or Cancelled) w.r.t timeslot and pickup_point

# I am filtering Status for only failed trips
time_slot_analysis = uber %>% filter(Status == "No Cars Available" | Status == "Cancelled") %>% 
        ggplot(aes(Status, fill = Pickup_point))

# frequency of not completed requests by timeslots 
time_slot_analysis + geom_bar(stat = "count", position = "dodge") + 
        facet_grid(.~req_time_slot) + xlab("Trip's Status") + ylab("Frequency of Requests") +
        ggtitle("Requests - Not Completed, Analysis by Timeslot, Pickup point") #+ ggsave("Requests - Not Completed, Analysis by Timeslot, Pickup point.png")

# failed reuests analysis by hour
time_analysis = uber %>% filter(Status == "No Cars Available" | Status == "Cancelled") %>% 
        ggplot(aes(Status, fill = req_time_slot))

# frequency of not completed requests by hour
time_analysis + geom_histogram(stat = "count", position = "dodge", aes(req_hour)) +
        facet_grid(.~Status + Pickup_point) + xlab("Hour of Request") + ylab("Frequency of Request") +
        ggtitle("Requests - Not Completed by Hour, Pickup point") #+ ggsave("Requests - Not Completed by Hour, Pickup point.png")

# Requests not completed : Cancellation is high - during early morining to morning - from city to airport
# Requests not completed : No cars available is high - during evening to night - from airport to city

# It is clearly evident that in early morning to morning business is impacted from city to airport 
# and in evening to night it is impacted from airport to City

rm(list=setdiff(ls(), "uber"))

# Demand - Supply and gap analysis

# Demand = Trips Completed + No cars available + Cancelled 
# Supply = Trips completed
# Gap = No cars available + Cancelled

# I am grouping by timeslots, then by pickup point and counting the all the trip statuses for each group
uber_grouped_by_time_slot = uber %>% group_by(req_time_slot,Pickup_point) %>% count(Status)
colnames(uber_grouped_by_time_slot)[4] = "No_of_requests" # re-naming the count column

# Summing the count by grouped timeslot and pickup point for demand
uber_demand = uber_grouped_by_time_slot %>% group_by(req_time_slot,Pickup_point) %>% summarise(demand = sum(No_of_requests))
# Summing the count by grouped timeslot and pickup point for supply
uber_supply = uber_grouped_by_time_slot %>% filter(Status == "Trip Completed") %>% group_by(req_time_slot,Pickup_point) %>% 
        summarise(supply = sum(No_of_requests))

# joining demand and supply to form a new dataset
uber_demand_supply = inner_join(uber_demand,uber_supply, by = c("req_time_slot", "Pickup_point"))
# deriving the gap
uber_demand_supply$gap = uber_demand_supply$demand - uber_demand_supply$supply

rm(uber_demand,uber_grouped_by_time_slot,uber_supply)

# Demand Vs Supply graph
ggplot(uber_demand_supply, aes(supply, demand, group = Pickup_point, color = Pickup_point)) + 
        geom_line() + coord_fixed() + ggtitle("Supply Vs Demand") + xlab("Supply")+
        ylab("Demand") #+ ggsave("Supply Vs Demand.png")


# Gap Analysis

ggplot(uber_demand_supply, aes(Pickup_point, fill = Pickup_point)) + geom_bar(aes(y = gap), stat = "identity") +
        facet_wrap( ~ req_time_slot) + ggtitle("Gap Analysis of Supply - Demand") + xlab("Pickup Point") +
        ylab("Supply - Demand Gap") #+ ggsave("Gap Analysis.png")

# Supply - Demand - Gap Visualization

# I am grouping by request hour, then by pickup point and counting the all the trip statuses for each group
uber_grouped_by_hour = uber %>% group_by(req_hour,Pickup_point) %>% count(Status)
colnames(uber_grouped_by_hour)[4] = "No_of_requests"

# Summing the count by grouped timeslot and pickup point for demand
uber_demand_by_hour = uber_grouped_by_hour %>% group_by(req_hour, Pickup_point) %>% summarise(demand = sum(No_of_requests))
# Summing the count by grouped timeslot and pickup point for supply
uber_supply_by_hour = uber_grouped_by_hour %>% filter(Status == "Trip Completed") %>% group_by(req_hour,Pickup_point) %>% 
        summarise(supply = sum(No_of_requests))

# joining demand and supply to form a new dataset
uber_demand_supply_by_hour = inner_join(uber_demand_by_hour,uber_supply_by_hour, 
                                        by = c("req_hour", "Pickup_point"))

# Gap analysis by request hour

uber_demand_supply_by_hour$gap = uber_demand_supply_by_hour$demand - uber_demand_supply_by_hour$supply
ggplot(uber_demand_supply_by_hour, aes(req_hour))+ geom_bar(aes(y = gap), stat = "identity", fill = "white")+
        geom_line(aes(y = demand,color = "demand")) +
        geom_line(aes(y = supply,color = "supply")) + ylab("No of Requests") + xlab("Hour of Request") +
        ggtitle("Gap - Demand - Supply") #+ ggsave("Gap - Demand - Supply.png")














































