library(dplyr)
library(ggplot2)

#Setting working directory
setwd("/Users/kareena_610/https:/github.com/k-610z/RepData_PeerAssessment1")

# the pathname of zipfile
zipfile<-"/Users/kareena_610/https:/github.com/k-610z/RepData_PeerAssessment1/activity.zip"

#Unzipping the file:
unzip(zipfile,exdir=getwd())

# Converting the file into a dataframe called activity
if(file.exists("activity.csv"))
    activity<-read.csv(file="activity.csv")

#prepprocessing the data before ach plot 

head(activity) #Seeing the overview of data

# Plotting the total daily steps per day in histogram plot
dailytotal<-activity %>% 
    group_by(date) %>% 
    summarise(tot_dailystps=sum(steps))

ggplot(dailytotal, aes(x=tot_dailystps)) +
    geom_histogram(fill = "seagreen", binwidth = 1000) +
    labs(title = "Steps/Day", x = "Steps", y = "Frequency")
mean(dailytotal$tot_dailystps, na.rm=TRUE)
median(dailytotal$tot_dailystps, na.rm=TRUE)

# Plotting time-interval graph of average steps during each interval

averages<-activity %>% 
          select(interval,steps) %>% 
          filter(complete.cases(.)) %>% 
          group_by(interval) %>% 
          summarise(avg_intervalstps = mean(steps))
averages

ggplot(data=averages, aes(x=interval, y=avg_intervalstps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")

# Filling Missing values and summarising the new dataset

missing_count<- activity %>% 
                summarise(missing_count=sum(is.na(steps)))
missing_count #prints the number of NA values in steps

#Lets fill the missing values 
date_na<- activity %>% 
            filter(!complete.cases(.)) %>% 
            select(date)

      
unique(date_na) #mostly data is missing from these dates

#since data is missing from most days, lets put the interval average in each
fill.value <- function(steps, interval) {
    filled <- NA #initialising the filled value
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "avg_intervalstps"])
   
     return(filled)
}
filled.activity <- activity
filled.activity$steps <- mapply(fill.value, filled.activity$steps, filled.activity$interval)

filled.activity$steps<-as.numeric(filled.activity$steps)

#plotting the new histogram:
dailytotal2<-filled.activity %>% 
              group_by(date) %>% 
              summarise(tot_dailystps2=sum(steps))

dailytotal2$tot_dailystps2<-as.numeric(dailytotal2$tot_dailystps2)

ggplot(dailytotal2, aes(x=tot_dailystps2))+
     geom_histogram(fill = "blue", binwidth = 1000)+
     labs(title = "Steps/Day", x = "Steps", y = "Frequency")

mean(dailytotal2$tot_dailystps2)
median(dailytotal2$tot_dailystps2)

#Comparing it to previous findings
class(filled.activity$date)

#converting date to class date
filled.activity$date<-as.Date(filled.activity$date, format="%Y-%m-%d")


#making a function to assign values as weekend or weekday

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.activity$day <- sapply(filled.activity$date, FUN=weekday.or.weekend)

#Now we plot the 5 min interval of total steps segregated by weekend/weekday

averages2 <- aggregate(steps ~ interval + day, data=filled.activity, mean)

ggplot(averages2, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")



