library(ggplot2)
library(plyr)

Sys.setlocale("LC_TIME", "English")

data.dir <- "E:\\R & Courses\\My Projects\\Reproducible Research\\Peer Assessment\\Data"

setwd(data.dir)

raw.data <- read.csv(list.files(data.dir))
data <- na.omit(raw.data) # Exclude NAs from dataframe
data$date <- as.Date(data$date, "%Y-%m-%d")

daily.steps <- aggregate(steps ~ date, data = data, sum)

hist <- qplot(steps, data = daily.steps, geom = "histogram", binwidth = 3000)
daily.steps.mean <- mean(daily.steps$steps)
daily.steps.median <- median(daily.steps$steps)
#daily.steps.mean <- aggregate(steps ~ date, data = data, mean)
#daily.steps.median <- aggregate(steps ~ date, data = data, median)

# Time series plot
plot <- qplot(date, steps, data = daily.steps.mean, geom = "line")
plot

# Find interval with max average steps
interval.steps.mean <- aggregate(steps ~ interval, data = data, mean)
max.interval <- interval.steps.mean[max(interval.steps.mean$steps), ]

# Calculate number of missing values from raw data
count(raw.data, vars = "steps")
length(raw.data$steps[is.na(raw.data$steps)])

# Replace NA with equivalent 5-minute interval mean steps
new.data <- raw.data
new.data$steps <- ifelse(is.na(raw.data$steps),interval.steps.mean$steps, raw.data$steps)

# Re-analyse data using new dataframe
new.daily.steps <- aggregate(steps ~ date, data = new.data, sum)

hist2 <- qplot(steps, data = new.daily.steps, geom = "histogram", binwidth = 3000)
new.daily.steps.mean <- mean(new.daily.steps$steps)
new.daily.steps.median <- median(new.daily.steps$steps)

# Add new column - day - either "weekend" or "weekday"
new.data$date <- as.Date(new.data$date, "%Y-%m-%d")
day <- as.character(weekdays(new.data$date))
new.data$day <- ifelse(day == "Saturday" | day == "Sunday", "Weekend", "Weekday")

# Plot average number of steps by interval grouped by weekend and weekday
plot.data <- aggregate(steps ~ interval + day, data = new.data, mean)
plot2 <- ggplot(data = plot.data, aes(x = interval, y = steps))
plot2 <- plot2 + geom_line()
plot2 <- plot2 + facet_grid(day ~.)
plot2

write.csv(new.data, "New Data.csv")
