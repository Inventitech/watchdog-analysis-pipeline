library(plyr)
library(data.table)
library(ggplot2)
library(ggthemes)

source("src/utils.R")
source("src/config-filtering.R")

intervals <- data.table(read.csv(file.path(pRes, 'intervals.csv'), numerals=c("no.loss")))
events <- data.table(read.csv(file.path(pRes, 'events.csv'), numerals=c("no.loss")))
users <- data.table(read.csv(file.path(pRes, 'users.csv'), numerals=c("no.loss")))
projects <- data.table(read.csv(file.path(pRes, 'projects.csv'), numerals=c("no.loss")))

########################################################
# ggplot configuration
########################################################
ggplot.defaults <- 
  #theme(axis.text.x = element_text(size = 15)) +
  #theme(axis.text.y = element_text(size = 15)) +
  theme_few() +
  #theme(legend.key = element_blank()) +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())  +
  theme(axis.text.x = element_text(size = 20)) + 
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 18, vjust = 1)) +
  theme(axis.title.y = element_text(size = 18, vjust = 1)) +
  theme(legend.text = element_text(size = 16))  +
  theme(legend.title = element_text(size = 18))
ggplot.small.defaults <-
  ggplot.defaults + theme(axis.title.x = element_text(size = 18, vjust = -0.5)) +
  theme(axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 1.5))

########################################################
# Filtering of data 
########################################################
# Only use intervals of WD>=2.0.0 versions
intervals <- subset(intervals, wdv %in% c("2.0.0","2.0.1","2.0.2"))

# Filter out users with user id to filter
users <- subset(users, !(uid %in% users.toremove))

# Filter out intervals if only 2 of them have unique session seed: not necessary?
# numintervals.per.session <- count(intervals, c('ss'))
# valid.ss <- numintervals.per.session[numintervals.per.session$freq > 2,]$ss
# length(valid.ss)
# intervals <- subset(intervals, ss %in% valid.ss)

# Filter out intervals and events from removed users
intervals <- subset(intervals, uid %in% users$uid)
intervals$programmingExperience <- factor(intervals$programmingExperience)
events <- subset(events, uid %in% users$uid)
events$programmingExperience <- factor(events$programmingExperience)

# group intervals: not necessary?
# intervals <- subset(intervals, programmingExperience %in% experience)
gc()
