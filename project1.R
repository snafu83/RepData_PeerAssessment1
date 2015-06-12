## reproducible research assigment 1
## christian jaehnichen

library(dplyr)
library(data.table)

## read data
getwd()
setwd("D:/coursera/coursera reproducible research/project1/")
dat <- read.csv("activity.csv", header = TRUE, sep = ",")
summary(dat)
str(dat)

## convert date & interval to date format

dat$day <- as.POSIXct(dat$date, tz = "UTC")

n <- dim(dat)[1]

for (i in 1:n){
  if (nchar(dat$interval[i]) == 1){
    dat$time[i] = paste("0", as.character(dat$interval[i]), sep = ":")
  }
  if (nchar(dat$interval[i]) == 2){
    dat$time[i] = paste("0", as.character(dat$interval[i]), sep = ":")
  }
  if (nchar(dat$interval[i]) == 3){
    dat$time[i] = paste(substr(dat$interval[i], 1, 1), substr(dat$interval[i], 2, 3), sep = ":")
  }
  if (nchar(dat$interval[i]) == 4){
    dat$time[i] = paste(substr(dat$interval[i], 1, 2), substr(dat$interval[i], 3, 4), sep = ":")
  }
}
dat$date <- as.POSIXct(paste(dat$day, dat$time), tz = "UTC", format = "%Y-%m-%d %H:%M")
dat$weekday <- weekdays(dat$date)
dat$week <- week(dat$date)

summary(dat)

## mean steps per day

steps <- dat %>%
  select(steps, day) %>%
  group_by(day) %>%
  summarize(steps = sum(steps, na.rm = TRUE))

par(mfrow= c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
hist(steps$steps, breaks = 5, xlab = "Steps", main = "5 breaks")
hist(steps$steps, breaks = 10, xlab = "Steps", main = "10 breaks")
hist(steps$steps, breaks = 20, xlab = "Steps", main = "20 breaks")
mtext("Histogram for sum of steps per day using 5, 10 and 20 breaks", outer = TRUE)

mnsteps <- format(mean(steps$steps, na.rm = TRUE), digits = 4, scientific = FALSE)
mdsteps <- format(median(steps$steps, na.rm = TRUE), digits = 5, scientific = FALSE)
steps.day.null <- sum(steps$steps == 0, na.rm = TRUE)

## average daily activity pattern

steps.mean <- round(mean(dat$steps, na.rm = TRUE), 0)
steps.median <- (median(dat$steps, na.rm = TRUE))
steps.max <- max(dat$steps, na.rm = TRUE)
steps.max.date <- dat[which(dat$steps == steps.max), ]
steps.max.date2 <- subset(dat, steps == steps.max)
steps.null <- sum(dat$steps == 0, na.rm = TRUE)

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
with(dat,plot(steps~date, type = "l", ylim = c(0, 1000), xlab = "Date", ylab = "# of steps per 5 min interval", main = "Number of steps per 5 min interval between 01/10/2012 and 30/11/2012"))
abline(h = steps.mean, col = "red", lty = 3)
abline(h = steps.median, col = "blue", lty = 3)
abline(h = steps.max, col = "green", lty = 3)
legend("topright", lty = 3, bty = "n", cex = .75, col = c("red", "blue", "green"), legend = c("Mean", "Median", "Max"))

## inputting missing values

steps.na <- sum(is.na(dat$steps))
dat.new1 <- dat
dat.new2 <- dat

steps.tod <- dat %>%
  select(steps, time) %>%
  group_by(time) %>%
  summarize(steps = round(mean(steps, na.rm = TRUE), 0))

for (i in 1:n){
  if (is.na(dat.new1$steps[i]) == TRUE){
    dat.new1$steps[i] <- steps.tod$steps[steps.tod$time == dat$time[i]]
    dat.new2$steps[i] <- 0
  }
}

steps1 <- dat.new1 %>%
  select(steps, day) %>%
  group_by(day) %>%
  summarize(steps = sum(steps, na.rm = TRUE))

steps2 <- dat.new2 %>%
  select(steps, day) %>%
  group_by(day) %>%
  summarize(steps = sum(steps, na.rm = TRUE))

mnsteps1 <- format(mean(steps1$steps, na.rm = TRUE), digits = 6, scientific = FALSE)
mdsteps1 <- format(median(steps1$steps, na.rm = TRUE), digits = 6, scientific = FALSE)
mnsteps2 <- format(mean(steps2$steps, na.rm = TRUE), digits = 5, scientific = FALSE)
mdsteps2 <- format(median(steps2$steps, na.rm = TRUE), digits = 5, scientific = FALSE)

overview <- data.frame(original = c(mnsteps, mdsteps, sum(dat$steps, na.rm = TRUE)), way1 = c(mnsteps1, mdsteps1, sum(dat.new1$steps)), way2 = c(mnsteps2, mdsteps2, sum(dat.new2$steps)), row.names = c("mean", "median", "total"))

par(mfrow= c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
hist(steps$steps, breaks = 10, xlab = "Steps", main = "Original dataset")
hist(steps1$steps, breaks = 10, xlab = "Steps", main = "Approach 1")
hist(steps2$steps, breaks = 10, xlab = "Steps", main = "Approach 2")
mtext("Histogram for sum of steps per day comparing the original dataset with the two different approaches", outer = TRUE)

steps1.mean <- round(mean(dat.new1$steps, na.rm = TRUE), 0)
steps1.median <- (median(dat.new1$steps, na.rm = TRUE))
steps1.max <- max(dat.new1$steps, na.rm = TRUE)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
with(dat,plot(steps~date, type = "l", ylim = c(0, 1000), xlab = "Date", ylab = "# of steps per 5 min interval", main = "Original dataset", cex.main = .75))
abline(h = steps.mean, col = "red", lty = 3)
abline(h = steps.median, col = "blue", lty = 3)
abline(h = steps.max, col = "green", lty = 3)
legend("topright", lty = 3, bty = "n", cex = .75, col = c("red", "blue", "green"), legend = c("Mean", "Median", "Max"))
with(dat.new1,plot(steps~date, type = "l", ylim = c(0, 1000), xlab = "Date", ylab = "# of steps per 5 min interval", main = "Modified dataset", cex.main = .75))
abline(h = steps1.mean, col = "red", lty = 3)
abline(h = steps1.median, col = "blue", lty = 3)
abline(h = steps1.max, col = "green", lty = 3)
legend("topright", lty = 3, bty = "n", cex = .75, col = c("red", "blue", "green"), legend = c("Mean", "Median", "Max"))
mtext("Number of steps per 5 min interval between 01/10/2012 and 30/11/2012", outer = TRUE)

## activity pattern between weekdays and weekends

for (i in 1:n){
  if (dat$weekday[i] == "Sunnday" | dat$weekday[i] == "Saturday"){
    dat$tow[i] <- "weekend"
    dat.new1$tow[i] <- "weekend"
  }else{
    dat$tow[i] <- "weekday"
    dat.new1$tow[i] <- "weekday"
  }
}

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1), oma = c(2, 1, 2, 1))
with(dat,plot(steps[tow == "weekend"]~date[tow == "weekend"],
              type = "l", ylim = c(0, 1000), xlim = c(as.POSIXct("2012-09-30"), as.POSIXct("2012-12-01")), xlab = "",
              ylab = "# of steps per 5 min interval", main = "Original dataset", cex.main = .75))
with(dat, lines(steps[tow == "weekday"]~date[tow == "weekday"], col = "red"))
abline(v = c(day.na$day[day.na$date.na == TRUE], lty = 3, day.na$day[day.na$date.na == TRUE] + days(1)), col = "green")
with(dat.new1,plot(steps[tow == "weekend"]~date[tow == "weekend"],
              type = "l", ylim = c(0, 1000), xlim = c(as.POSIXct("2012-09-30"), as.POSIXct("2012-12-01")), xlab = "Date",
              ylab = "# of steps per 5 min interval", main = "Modified dataset", cex.main = .75))
with(dat.new1, lines(steps[tow == "weekday"]~date[tow == "weekday"], col = "red"))
abline(v = c(day.na$day[day.na$date.na == TRUE], day.na$day[day.na$date.na == TRUE] + days(1)), col = "green")
mtext("Number of steps per 5 min interval between 01/10/2012 and 30/11/2012 \n Difference between original and modified dataset", outer = TRUE)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottomright", c("Weekend", "Weekday"), xpd = TRUE, horiz = TRUE, inset = c(0, 0), bty = "n", lty = 1, col = c("black", "red"), cex = 1)

steps.weekday1 <- dat %>%
  select(steps, tow) %>%
  group_by(tow) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

steps.weekday2 <- dat.new1 %>%
  select(steps, tow) %>%
  group_by(tow) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

print(merge(steps.weekday1, steps.weekday2, by = "tow") %>%
        mutate(PartofWeek = tow, MeanStepsOriginal = round(steps.x, 1),
               MeanStepsModified = round(steps.y, 1)) %>%
        select(PartofWeek, MeanStepsOriginal , MeanStepsModified))

day.na <- dat %>%
  select(steps, day) %>%
  group_by(day) %>%
  summarize(steps = sum(steps), date.na = is.na(steps))


## integer division 
5 %/% 3
## modulus
5 %% 3