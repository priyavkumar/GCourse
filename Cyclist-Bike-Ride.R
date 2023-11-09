# Install Library



#Load Library
library("ggplot2")
library("lubridate")
library("tidyverse")
library("knitr")
library("dplyr")
options(dplyr.summarise.inform = FALSE)

#Read datasets
df1 <- read_csv("202204-divvy-tripdata.csv")
df2 <- read_csv("202205-divvy-tripdata.csv")
df3 <- read_csv("202206-divvy-tripdata.csv")
df4 <- read_csv("202207-divvy-tripdata.csv")
df5 <- read_csv("202208-divvy-tripdata.csv")
df6 <- read_csv("202209-divvy-tripdata.csv")
df7 <- read_csv("202210-divvy-tripdata.csv")
df8 <- read_csv("202211-divvy-tripdata.csv")
df9 <- read_csv("202212-divvy-tripdata.csv")
df10 <- read_csv("202301-divvy-tripdata.csv")
df11 <- read_csv("202302-divvy-tripdata.csv")
df12 <- read_csv("202303-divvy-tripdata.csv")

#check column names before merging
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)
colnames(df9)
colnames(df10)
colnames(df11)
colnames(df12)

#Merge Datasets
df_bike <- bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
colnames(df_bike)

#datatype of the DataFrame
sapply(df_bike,class)

#summary of the Dataset
summary(df_bike)
fileConn<-file("output.txt")
writeLines(summary(df_bike), fileConn)
close(fileConn)



############ CELAN DATASET #########################

#Check for missing values
colSums(is.na(df_bike))

#Drop empty cells
bike_share_clean <- drop_na(df_bike)

#Formate Column
bike_share_clean$date <- as.Date(bike_share_clean$started_at)
bike_share_clean$month <- format(as.Date(bike_share_clean$started_at), "%m")
bike_share_clean$day <-format(as.Date(bike_share_clean$started_at), "%d")
bike_share_clean$year <-format(as.Date(bike_share_clean$started_at), "%y")
bike_share_clean$day_of_week <-format(as.Date(bike_share_clean$started_at), "%A")

#Check Datatypes of the column
sapply(bike_share_clean,class)

#Calculate Ride Duration
bike_share_clean$ride_duration <-difftime(bike_share_clean$ended_at,bike_share_clean$started_at)/60

#check datatype of ride_duration column
str(bike_share_clean$ride_duration)

#Convert the ride_duration column to numeric
bike_share_clean$ride_length <-as.numeric(bike_share_clean$ride_duration)

str(bike_share_clean$ride_length) #check datatype of ride_length column



########### ANALYZE ######################

#Summarize the Data
mean(bike_share_clean$ride_length)
max(bike_share_clean$ride_length)
min(bike_share_clean$ride_length)
median(bike_share_clean$ride_length)

summary(bike_share_clean$ride_length)

#Aggregating members_casual and ride_length(in seconds)
aggregate(bike_share_clean$ride_length ~ bike_share_clean$member_casual, FUN = mean)

#Average ride length per day of the week of casual and member
bike_share_clean %>%
  group_by(member_casual, day_of_week) %>%
  aggregate(bike_share_clean$ride_length ~ bike_share_clean$member_casual+ day_of_week, FUN = mean)

bike_share_clean %>%
  aggregate(bike_share_clean$ride_length ~ bike_share_clean$member_casual+ day_of_week, FUN = max)

bike_share_clean %>%
  aggregate(bike_share_clean$ride_length ~ bike_share_clean$member_casual+ day_of_week, FUN = min)

#Summarize no of rides by weekdays
bike_share_clean %>% 
  group_by(day_of_week,member_casual) %>% 
  summarise(number_of_ride = n()) %>% 
  arrange(day_of_week)

  

#Summarize no of rides by month
bike_share_clean %>%
  group_by(month) %>%
  summarise(ride_len = sum(ride_length)) %>% arrange(month)

##Summarize no of rides by membership
bike_share_clean %>%
  group_by(member_casual) %>%
  summarise(no_of_rides = length(ride_length)) %>%
  arrange(member_casual)

#***********************************************************************************************
#                           VISUALIZE DATASET 
#***********************************************************************************************


#***********************************************************************************************
#                 AVERAGE RIDE DURATION OF MEMBER AND CASUAL RIDER PER MONTH
#***********************************************************************************************

bike_share_clean %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual )) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  labs(title = "Average duration of ride per month") + facet_wrap(~member_casual)+
  scale_fill_manual(values = c("#6495ED", "#33A02C"))


#***********************************************************************************************
#             AVERAGE RIDE DURATION OF MEMBER_CASUAL RIDER PER DAY OF MONTH"
#***********************************************************************************************


bike_share_clean %>% group_by(member_casual, day) %>% 
  summarise(Average_duration = mean(ride_duration), .groups = 'drop') %>% 
  ggplot(aes(x = day, y =Average_duration, fill = member_casual )) + 
  geom_col(position = "dodge") + labs(title = "AVERAGE DURATION OF RIDER PER DAY OF MONTH")+ 
  facet_wrap(~member_casual) + scale_fill_manual(values = c("#6495ED", "#33A02C")) +
  scale_x_discrete(guide = guide_axis(angle = 90))

#***********************************************************************************************
#                                   RIDER TYPE IN PERCENTAGE
#***********************************************************************************************

bike_share_clean %>% group_by(member_casual) %>% summarize(number_of_rides = n()) %>% 
  mutate(percentage = round(number_of_rides*100/ sum(number_of_rides))) %>% 
  ggplot(aes(x = "", y = number_of_rides, fill = member_casual)) + 
  geom_bar(width = 1, stat="identity", color = "white", show.legend = FALSE) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste(member_casual,paste(percentage,"%"), sep = "\n")), position = position_stack(vjust = 0.5),color ="white") + 
  labs(title = "Ride Percentage of Rider type ") + theme_void() 


#***********************************************************************************************
#                       RIDE COUNT OF MEMBER AND CASUAL RIDER PER MONTH        
#***********************************************************************************************

bike_share_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x = month,y = ride_count,fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Total rides taken (ride_count) by Members and Casual riders")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#***********************************************************************************************
#                       RIDE COUNT OF MEMBER AND CASUAL RIDER PER MONTH        
#***********************************************************************************************

bike_share_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x = day_of_week,y = ride_count,fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "RIDE COUNT OF MEMBER AND CASUAL RIDER PER WEEKDAYS")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#***********************************************************************************************
#                       RIDE DISTANCE COVERED BY MEMBER AND CASUAL RIDER PER MONTH        
#***********************************************************************************************
bike_share_clean %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_length,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean distance traveled by Members and Casual riders") +
  ggsave("/Users/priya/personal/Capstone/Bike-Share/Dataset/programming/results/mean_distance.jpeg")


#***********************************************************************************************
#                       RIDE COUNT COVERED BY MEMBER AND CASUAL RIDER PER DAY OF THE WEEK        
#***********************************************************************************************

week_day_ride <- as.data.frame(bike_share_clean %>% 
                                 group_by(day_of_week,member_casual) %>%     #create a dataframe
                                 summarise(number_of_ride = n()))
week_day_ride$number_of_ride <-as.numeric(week_day_ride$number_of_ride) #convert number of ride column to numberic value.
sapply(week_day_ride,class) # check the datatype of the columns

#create a visualization 
ggplot(week_day_ride, aes(x = day_of_week, y = number_of_ride, fill = member_casual)) +
  geom_bar(stat = "identity",position="dodge")


