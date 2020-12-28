library(dplyr)
library(ggplot2)

if(!file.exists("activity.csv")){
    unzip("activity.zip")
}

## Loading and preprocessing the data
data <- read.csv("activity.csv")

steps_by_day <- data %>% group_by(date) %>% summarise(total.steps = sum(steps)) %>% na.omit()
hist(steps_by_day$total.steps, xlab = "Daily Steps", main = "Total Steps per Day", breaks = 30)

mean_median <- steps_by_day %>% summarise(mean = mean(total.steps), median = median(total.steps))
print(mean_meaidan)

## What is the average daily activity pattern?
steps_interval <- data %>% group_by(interval) %>% na.omit() %>% summarise(msteps = mean(steps))
g <- ggplot(steps_interval, aes(x = interval, y = msteps))
g + geom_line() + labs(x = "Intervals" , y = "Average Steps")

indice <- which.max(steps_interval$msteps)
steps_interval$interval[indice]

### Imputing missing values
## Calculate and report the total number of missing values in the dataset
## (i.e. the total number of rows with NAs)
sum(is.na(data))

## Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sophisticated. For example, you could use
## the mean/median for that day, or the mean for that 5-minute interval, etc.
my_replace <- function(x){
    replace(x, is.na(x), mean(x, na.rm = TRUE))
}

new_data <- data %>% group_by(interval) %>% mutate(steps = my_replace(steps))

new_steps_by_day <- new_data %>% group_by(date) %>% summarise(total.steps = sum(steps))
hist(new_steps_by_day$total.steps, xlab = "Daily Steps", main = "Total Steps per Day", breaks = 30)

new_mean_median <- new_steps_by_day %>% summarise(mean = mean(total.steps), median = median(total.steps))

print(paste0("Mean with na's = ", round(mean_median[1]), ". Mean with no na's = ", round(new_mean_median[1])))
print(paste0("Median with na's = ", mean_median[2], ". Median with no na's = ", round(new_mean_median[2])))

## Are there differences in activity patterns between weekdays and weekends?
new_data$date <- as.Date(as.character(new_data$date), format = "%Y-%m-%d")

new_data$week <- weekdays(new_data$date)
new_data$week <- ifelse(new_data$week == "sábado" | new_data$week == "domingo", "weekdend", "weekday")
new_data$week <- as.factor(new_data$week)

weekday_comparison <- new_data %>% group_by(interval, week) %>% summarise(msteps = mean(steps))
g <- ggplot(weekday_comparison, aes(x = interval, y = msteps, color = week))
g + geom_line() + facet_grid(week ~ .) + labs(x = "Interval", y = "Average steps")