
library(lubridate)
library(ggplot2)

setwd("~/Coursera/ForkedRepos/RepData_PeerAssessment1")

df <- read.csv("activity.csv")
df$date <- ymd(df$date)


#
g <- ggplot(data = df, aes(x = date, y = steps))
g + geom_bar(stat = "identity",fill = "steelblue") + 
    labs(title= "Histogram of the total number of steps taken each day")

meanPerDay <- aggregate(steps ~ date, df, mean)
names(meanPerDay) <- c("date", "meanSteps")
medianPerDay <- aggregate(steps ~ date, df, median)
names(medianPerDay) <- c("date", "medianSteps")
infoPerDay <- merge(meanPerDay,medianPerDay, by = "date")

totalPerDay <- aggregate(steps~date,df, sum)
meanPerDay <- aggregate(steps ~ date, df, mean)
medianPerDay <- aggregate(steps ~ date, df, median)
infoAllDays <- merge(totalPerDay,merge(meanPerDay,medianPerDay, by = "date"), by = "date")
names(infoAllDays) <- c("date","totalSteps","meanSteps","medianSteps")
infoAllDays


# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
df2 <- merge(df, meanPerDay, by = "date")
df2 <- merge(df2, medianPerDay, by = "date")
df2 <- df2[with(df2,order(date,interval)),]


df2 <- aggregate(steps ~ interval,df, mean)

maxPoint <- df2[which.max(df2$steps),]

g <- ggplot(data = df2, aes(x = interval, y = steps))
g + geom_line(col = "steelblue", size = 1) + 
    labs(x = "Interval", y = "Average Steps",title = "Average Steps for each interval in a day") + 
    geom_point(data = maxPoint,aes(x=interval, y = steps), shape = 21, size = 3, color = "red", stroke = 2) + 
    geom_vline(xintercept = maxPoint$interval, linetype="dotted")


# 
totalNA <- sum(is.na(df$steps))
# Filling in the data
fill_NA_with_mean <- function(df1,df2 = df2){
    for(i in 1:nrow(df1)){
        if(is.na(df1[i,"steps"])){
            tmpInterval <- df1[i,"interval"]
            df1[i,"steps"] <- df2[df2$interval == tmpInterval, "steps"]
        }
    }
    return(df1)
}

df_full_values <- fill_NA_with_mean(df, df2)



# 
g <- ggplot(data = df_full_values, aes(x = date, y = steps))
g + geom_bar(stat = "identity",fill = "steelblue") + 
    labs(title= "Histogram of the total number of steps taken each day (NAs imputed with mean values)")

library(lubridate)

df_full_values$wday <- wday(df_full_values$date) 
for(i in 1:nrow(df_full_values)){
    if(df_full_values[i,"wday"] %in% c(1,7)){
        df_full_values[i,"wday"] <- "weekend day"
    } else {
        df_full_values[i,"wday"] <- "weekday"
    }
}
df_full_values$wday <- as.factor(df_full_values$wday)

df3 <- aggregate(steps ~ interval + wday,df_full_values, mean)
g <- ggplot(data = df3, aes(x = interval, y = steps))
g + geom_line(col = "steelblue", size = 1) + 
    labs(x = "Interval", y = "Average Steps",title = "Average Steps for each interval in a day") + 
    facet_grid(wday~.)

g <- ggplot(data = df3, aes(x = interval, y = steps, group = wday, color = wday))
g + geom_line(size = 2) + 
    labs(x = "Interval", y = "Average Steps",title = "Overlap of weekday and weekend")
