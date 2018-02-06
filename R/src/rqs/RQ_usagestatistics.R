#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
# 
# Basic statistics about our dataset
library(plyr)
library(ggplot2)
library(xts)
library(rworldmap)
library(RColorBrewer)
library(fields)

source("src/header.R")

# number of users
printf("Number of unique users: %d", nrow(users))

## Raffle-off prizes
winners.possible <- count(intervals, c('uid'))
winners.possible <- winners.possible[winners.possible$freq>100,]
winners.possible <- merge(winners.possible, users)
winners.possible <- winners.possible[winners.possible$mayContactUser,]
winners <- winners.possible[sample(nrow(winners.possible), 3),]
printf("Tablet goes to: %s", winners[1,]$email)
printf("Headphones go to: %s", winners[2,]$email)
printf("Keyboard goes to: %s", winners[3,]$email)

users.active <- unique(intervals$uid)
# number of users with projects and intervals
printf("Number of active users in timeframe with intervals: %d", length(unique(intervals$uid)))

# Filter on active users
users.active <- subset(users, uid %in% users.active)

## Get mails from active people
users.may.contact <- subset(users, mayContactUser)

write.csv(users.may.contact$email,file.path(pRes, "emails.csv"),quote=F,row.names=F,col.names=F,sep=",")

## PLOT: users / time
count.data <- 1:nrow(projects)
projects.per.time <- data.frame(projects$regDate, count.data)

convertUnixTimeStampsToDate <- function(x) {
  as.POSIXct(as.numeric(as.character(x)),origin="1970-01-01",tz="GMT")
}

convert.to.count.data <- function(x) {
  count.data <- seq(1, 1, length.out=nrow(x))
  count.data <- 1:nrow(x)
  users.dates <- convertUnixTimeStampsToDate(x$regDate)
  xts.ts <- xts(count.data, users.dates)
  users.per.day <- apply.daily(xts.ts, sum)
  users.per.day <- data.frame(users.dates, count.data)
  users.per.day
}

users.per.day <- data.frame(convert.to.count.data(users), Group="Users")
projects.per.day <- data.frame(convert.to.count.data(projects), Group="Projects")
combined.per.day <- rbind(users.per.day, projects.per.day)

p <- ggplot(data=combined.per.day, aes(x=users.dates, y=count.data, group=Group, colour=Group)) +
  ylab("count") +
  xlab("") +
  geom_line(size=1.5) +
  ggplot.defaults + 
  theme(legend.title=element_blank())

save.pdf(p, "rq_usagestatistics.pdf", height=7, width=14)




sPDF <- joinCountryData2Map(countries,
                            joinCode = "NAME",
                            nameJoinColumn = "Country",
                            verbose = TRUE)

cc <- unique(countries$Freq)
colourPalette = two.colors(n=max(cc)-min(cc)+1, start="white", end="white", alpha=1.0)
colourPalette = colourPalette[cc - min(cc) + 1]

p <- mapCountryData(sPDF,nameColumnToPlot="Freq",colourPalette=colourPalette,catMethod=cc,addLegend = F,mapTitle = '')
save.pdf(p, "rq_worldmap.pdf", height=14, width=25)

