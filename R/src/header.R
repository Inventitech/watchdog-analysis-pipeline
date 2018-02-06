library(plyr)
library(data.table)
library(ggplot2)
library(ggthemes)

source("src/utils.R")
source("src/config-filtering.R")

intervals <- data.table(read.csv(file.path(pRes, 'intervals.csv'), numerals=c("no.loss")))
users <- data.table(read.csv(file.path(pRes, 'users.csv'), numerals=c("no.loss")))
projects <- data.table(read.csv(file.path(pRes, 'projects.csv'), numerals=c("no.loss")))

projects <- plyr::rename(projects, c("ide" = "IDE"))
intervals <- plyr::rename(intervals, c("ide.x" = "IDE.x"))
intervals <- plyr::rename(intervals, c("ide.y" = "IDE.y"))

intervals$IDE.x <- toupper(intervals$IDE.x)
intervals$IDE.y <- toupper(intervals$IDE.y)
projects$IDE <- toupper(projects$IDE)

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
  theme(axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"), size = 18, vjust = 1)) +
  theme(axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), size = 18, vjust = 1)) +
  theme(legend.text = element_text(size = 16))  +
  theme(legend.title = element_text(size = 18))
ggplot.small.defaults <-
  ggplot.defaults + theme(axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"), size = 18, vjust = 0.1)) +
  theme(axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), size = 18, hjust = 0.5, vjust = 1.5))

########################################################
# Filtering of data 
########################################################
# Only use specified good WD versions
intervals <- subset(intervals, wdv %in% versions)

# Only use specified IDEs
intervals <- subset(intervals, IDE.x %in% ides)
projects <- subset(projects, IDE %in% ides)

# We have to rename because plyr's join does not understand vectors
users <- plyr::rename(users, replace = c('uid' = 'userId'))
users$IDE <- plyr::join(users, projects, by = 'userId', match = "first")$IDE
users <- plyr::rename(users, replace = c('userId' = 'uid'))
users <- subset(users, IDE %in% ides)

# bug 153: https://github.com/TestRoots/watchdog/issues/153
# remove users where programmingExperience == "" 
users <- subset(users, programmingExperience != "")
users$programmingExperience <- factor(users$programmingExperience)

# Filter out users with user id to filter
users <- subset(users, !(uid %in% users.toremove))

# Filter out intervals if only 2 of them have unique session seed
numintervals.per.session <- count(intervals, c('ss'))
valid.ss <- numintervals.per.session[numintervals.per.session$freq > 2,]$ss
length(valid.ss)
intervals <- subset(intervals, ss %in% valid.ss)

# Filter out intervals from removed users
intervals <- subset(intervals, uid %in% users$uid)
intervals$programmingExperience <- factor(intervals$programmingExperience)

# group intervals
intervals <- subset(intervals, programmingExperience %in% experience)
gc()
