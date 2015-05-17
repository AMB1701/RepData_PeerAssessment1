require("dplyr")
require("ggplot2")
original <- read.csv("activity.csv", stringsAsFactors=FALSE, na.string="NA")
original$date <- as.Date(original$date)

#Task 1: Calculate mean and median number of steps by day.
totalsPerDay <- as.data.frame(as.Date(unique(original$date)))
colnames(totalsPerDay) <- c("date")

for(x in totalsPerDay$date)
{
  subset <- filter(original, date==x)
  subset$steps[is.na(subset$steps)] <- 0
  totalsPerDay[totalsPerDay$date==x,2] <- sum(subset$steps)
}
colnames(totalsPerDay)[2] <- "steps"

png("hist1.png", width=800, height=600)
hist(totalsPerDay$steps, breaks=nrow(totalsPerDay), col="red", main="Frequency of Steps",
     xlab="# of Steps", ylab="Frequency", xaxt="n")
maxX <- 1000 * (max(totalsPerDay$steps)%/%1000 + as.logical(max(totalsPerDay$steps)%%1000))
axis(1, at=seq(0, as.integer(maxX), by=maxX/20))
dev.off()

print(paste("Mean number of steps per day:", 
            as.integer(mean(totalsPerDay$steps))))
print(paste("Median number of steps per day:", 
            as.integer(median(totalsPerDay$steps))))
print("Please see hist1.png for histogram.")

#Task 2: Explore average daily activity pattern.
totalsPerInterval <- as.data.frame(unique(original$interval))
colnames(totalsPerInterval) <- c("interval")

for(x in totalsPerInterval$interval)
{
  subset <- filter(original, interval==x)
  subset$steps[is.na(subset$steps)] <- 0
  totalsPerInterval[totalsPerInterval$interval==x,2] <- sum(subset$steps)
}
colnames(totalsPerInterval)[2] <- "steps"

png("line1.png", width=1000, height=600)
plot(x=totalsPerInterval$interval, y=totalsPerInterval$steps, type="l", xaxt="n",
  col="red", main="Number of Steps per Interval", xlab="Interval", lwd=2,
  ylab="# of Steps", xaxt="n")
maxX <- 1000 * (max(totalsPerInterval$interval)%/%1000 + as.logical(max(totalsPerInterval$interval)%%1000))
axis(1, at=seq(0, as.integer(maxX), by=maxX/30))
dev.off()

maxSteps <- max(totalsPerInterval$steps)
print(paste("Max number of steps occurs at interval:",
            totalsPerInterval[totalsPerInterval$steps==maxSteps,1]))
print("Please see line1.png for time series plot.")

#Task 3: Imputing missing values.  
#Replacement strategy is the mean for each 5 minute interval.
print(paste("Total NA's:",sum(is.na(original$steps))))

totalsPerInterval[,3] <- totalsPerInterval[,2]/length(unique(original$date))
colnames(totalsPerInterval)[3] <- "average"
cleaner <- original
 
for(x in 1:nrow(cleaner))
{
  if(is.na(cleaner[x,1]))
    cleaner[x,1] <- ceiling(totalsPerInterval[totalsPerInterval$interval == cleaner[x,3],3])
}

totalsPerDay <- as.data.frame(as.Date(unique(cleaner$date)))
colnames(totalsPerDay) <- c("date")

for(x in totalsPerDay$date)
{
  subset <- filter(cleaner, date==x)
  subset$steps[is.na(subset$steps)] <- 0
  totalsPerDay[totalsPerDay$date==x,2] <- sum(subset$steps)
}
colnames(totalsPerDay)[2] <- "steps"

png("hist2.png", width=800, height=600)
hist(totalsPerDay$steps, breaks=nrow(totalsPerDay), col="blue", main="Frequency of Steps",
     xlab="# of Steps", ylab="Frequency", xaxt="n")
maxX <- 1000 * (max(totalsPerDay$steps)%/%1000 + as.logical(max(totalsPerDay$steps)%%1000))
axis(1, at=seq(0, as.integer(maxX), by=maxX/20))
dev.off()

print(paste("Mean number of steps per day (with NA's removed):", 
            as.integer(mean(totalsPerDay$steps))))
print(paste("Median number of steps per day (with NA's removed):", 
            as.integer(median(totalsPerDay$steps))))
print("Please see hist2.png for histogram with NA's removed.")


#Task 4: Are there differences in activity patterns between weekdays and weekends?
cleaner$isWeekday <- ifelse(weekdays(cleaner$date)=="Sunday" | weekdays(cleaner$date)=="Saturday",
                            "weekend","weekday")

weekdaySet <- filter(cleaner, isWeekday=="weekday")
weekendSet <- filter(cleaner, isWeekday=="weekend")
weekdayTotal <- as.data.frame(unique(weekdaySet$interval))
colnames(weekdayTotal) <- c("interval")

for(x in weekdayTotal$interval)
{
  subset <- filter(weekdaySet, interval==x)
  weekdayTotal[weekdayTotal$interval==x,2] <- sum(subset$steps)
}
colnames(weekdayTotal)[2] <- "steps"
weekdayTotal[,3] <- rep("weekday", times=nrow(weekdayTotal))
colnames(weekdayTotal)[3] <- "isWeekday"

weekendTotal <- as.data.frame(unique(weekendSet$interval))
colnames(weekendTotal) <- c("interval")

for(x in weekendTotal$interval)
{
  subset <- filter(weekendSet, interval==x)
  weekendTotal[weekendTotal$interval==x,2] <- sum(subset$steps)
}
colnames(weekendTotal)[2] <- "steps"
weekendTotal[,3] <- rep("weekend", times=nrow(weekendTotal))
colnames(weekendTotal)[3] <- "isWeekday"

weekdaySummary <- rbind(weekdayTotal, weekendTotal)

png("weekdays.png", width=800, height=600)
p <- ggplot(weekdaySummary, aes(x=interval, y=steps, color=isWeekday)) + facet_grid(.~isWeekday) + xlab("Interval") +
  ylab("Steps") + ggtitle("Total steps by interval: Weekdays vs. Weekends") +
  geom_line()

print(p)
dev.off()

rm(p, maxX, x, maxSteps, totalsPerDay, subset, totalsPerInterval, weekdaySet, weekdayTotal, weekendSet, weekendTotal, weekdaySummary, original, cleaner)
