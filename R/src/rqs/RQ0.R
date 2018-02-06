#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
# 
# Basic statistics about our dataset
library(plyr)
library(ggplot2)
library(scales)

# number of users
printf("Number of unique users: %d", nrow(users))

users.active <- unique(intervals$uid)
# number of users with projects and intervals
printf("Number of active users in timeframe with intervals: %d", length(unique(intervals$uid)))

# Filter on active users
users <- subset(users, uid %in% users.active)

## RESULT: number of unique countries
countries <- as.data.frame(table(users$country))
names(countries) <- c("Country", "Freq")
printf("Number of unique countries: %d", length(unique(users$country)))

threshold <- 0
threshold.name <- sprintf("<%d",threshold)

countries.most <- countries[countries$Freq >= threshold,]
names(countries.most) <- c("names", "Freq")
remaining_users <- sum(countries[countries$Freq < threshold,]$Freq)
levels(countries.most$names) <- c(levels(countries.most$names), threshold.name)
rownames(countries.most) <- NULL
countries.most <- rbind(countries.most, list(threshold.name,remaining_users))
countries.most <- countries.most[order(-countries.most$Freq),]
countries.most <- cbind(countries.most,countries.most$Freq/sum(countries.most$Freq))
print(countries.most)

# RESULT country distribution
barplot(countries.most$Freq,names.arg=countries.most$names, main = "Countries")

# RESULT average number of sessions
sessions.per.uid <- aggregate(ss ~ uid, intervals, function(x) length(unique(x)))
sessions.per.uid$freq <- sessions.per.uid$ss
sessions.per.uid$ss <- NULL

printf("Total number of sessions: %d", sum(sessions.per.uid$freq))
printf("Mean Number of sessions per developer: %f", sum(sessions.per.uid$freq)/nrow(sessions.per.uid))
print(summary(sessions.per.uid$freq))

sessions.per.uid.sorted <- sessions.per.uid[order(sessions.per.uid$freq, decreasing = T),]
sessions.per.uid.sorted$num <- 1:nrow(sessions.per.uid.sorted)

sessions.per.uid.sorted$IDE <- join(sessions.per.uid.sorted, intervals, by = "uid", match = "first")$IDE.x

p <- ggplot(sessions.per.uid.sorted) + 
  aes(x=num,y=freq) + 
  geom_bar(stat="identity") +
  ggplot.small.defaults +
  ylab("#Sessions") +
  xlab("Developer-ID") +
  scale_x_continuous("Developer-ID", breaks=c(0, 500, 1000, 1500, 2000), labels=c(0, 500, 1000, 1500, 2000))
p$layout$clip[p$layout$name=="panel"] <- "off"

save.pdf(p, "rq0_sessions_per_user.pdf")

# RESULT: most users have little sessions

printf("NOTE: For rq0_sessions_per_user.pdf, We take the IDE the user was first registered with!")
aggregated.IDE <- aggregate(IDE.y ~ uid, intervals, function(x) length(unique(x)))
printf("%d users used more than one IDE", nrow(aggregated.IDE[aggregated.IDE$IDE.y > 1,]))

# A plot of the number of sessions per IDE
aggregated.sessions.per.uid.sorted <- aggregate(freq ~ IDE, sessions.per.uid.sorted, FUN = sum)

p <- ggplot(aggregated.sessions.per.uid.sorted) + 
  aes(x=IDE,y=freq,fill=IDE) + 
  geom_bar(stat="identity") +
  ggplot.small.defaults +
  ylab("#Sessions") +
  xlab("IDE") +
  scale_colour_few() +
  guides(fill=FALSE) +
  scale_y_continuous(labels = comma)
save.pdf(p, "rq0_sessions_per_IDE.pdf")

p <- ggplot(aggregated.sessions.per.uid.sorted) + 
  aes(x=IDE,y=freq,fill=IDE) + 
  geom_bar(stat="identity") +
  ggplot.small.defaults +
  ylab("#Sessions") +
  xlab("IDE") +
  scale_colour_few() +
  guides(fill=FALSE) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) 
save.pdf(p, "rq0_sessions_per_IDE_logscale.pdf")


printf("Number of intervals: %d", nrow(intervals))
intervals.per.uid <- sort(count(intervals, c('uid'))$freq, decreasing=T)
print(summary(intervals.per.uid))
barplot(intervals.per.uid, main = "#Intervals per User")


## RESULT average wd time per user
summaryOfDuration <- function(intervals) {
  time.per.uid <- aggregate(intervals$duration, by=list(intervals$uid), sum)
  time.per.uid$x <- time.per.uid$x/1000/60/60  # in hours
  summary(time.per.uid)
  return(time.per.uid)
}

## RESULT: number of intervals
barplot(sort(summary(intervals$it), decreasing = T), main = "# of interval types")
summary(intervals$it)
summary(intervals$it)/sum(summary(intervals$it))

## RESULT: interval length
print("Intervals length")

return.duration <- function(x) {
  return(subset(intervals, it == x)$duration/1000)
}

generate_overview_length <- function(interval.types, scale=1) {
  interval.duration.results <- c()
  for(i in interval.types) {
    interval.duration.results <- cbind(interval.duration.results, return.duration(i)*scale)
  }
  boxplot(interval.duration.results, outline = F, names=interval.types, main = "Overview of interval length")
}

generate_overview_length(c('ua','ty','re'))
generate_overview_length(c('eo','ua','ty','re'),1/60)

## RESULT: worktime
eo.intervals <- subset(intervals, it == "eo")
worktime <- sum(as.numeric(eo.intervals$duration))/1000/60/60  # in hours
printf("Total observed worktime (in hours): %f", worktime)
worktime.oecd <- worktime/1770
printf("OECD observed work years: %f", worktime.oecd)

barplot_all <- function(survey, survey.answers) {
  for(i in survey.answers) {
    printf('Your automated results for %s', i)
    res_table <- table(survey[[i]])
    print(res_table)
    print(res_table/sum(res_table))
    res <- as.data.frame(res_table)
    barplot(res$Freq, main = i)
  }  
}

## Survey stuff
survey.answers <- c('usesJunit','usesOtherTestingFrameworks','usesOtherTestingForms')
survey <- count(intervals, union(survey.answers, 'pid'))
barplot_all(survey, survey.answers)

users.copy <- users

survey.answers <- c('operatingSystem', 'programmingExperience')
barplot_all(users.copy, survey.answers)

## RESULT: programming experience
users.copy$IDE <- join(users.copy, intervals, by = "uid", match = "first")$IDE.x

progEx <- subset(users.copy, select=c('programmingExperience','IDE'))
progEx$programmingExperience <- revalue(progEx$programmingExperience, c("< 1 year"="< 1", "1-2 years"="1-2", "3-6 years"="3-6", "7-10 years"="7-10", "> 10 years"="> 10"))
progEx$programmingExperience <- factor(progEx$programmingExperience, levels =c("< 1","1-2", "3-6", "7-10", "> 10"), ordered = T)

res <- as.data.frame(table(progEx))

p <- ggplot(res) + 
  aes(x=programmingExperience, y=Freq, fill=IDE) + 
  geom_bar(stat='identity') +   
  ggplot.small.defaults +
  xlab("Programming Experience (Years)") +
  ylab("#Developers") +
  scale_colour_few() +
  theme(legend.position="none")
save.pdf(p, "rq0_programming_experience.pdf")

# Shapiro-test for normality
shapiro.test(as.data.frame(table(users.copy[['programmingExperience']]))$Freq)

