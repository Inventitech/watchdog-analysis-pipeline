# This is an extracted version of the immediate reaction graph to generate on-the-fly graphs from exisiting reactions data on your local machine
# Pulls-in some duplicated code (from config.R) to make it work without hassle

# RQ 3.2 Whats the immediate reaction to a failing test?
# "Super"-fast algorithm
library(testit)
library(dplyr)
library(plyr)
library(data.table)
library(fastmatch)
library(ggplot2)
library(foreach)
library(doMC)
library(ggthemes)
library(grid)
library(scales)
library(mgcv)
library(RColorBrewer)

library('rjson')
library(lmPerm)
library(ggplot2)
library(metap)

# printf for R
printf <- function(...) invisible(print(sprintf(...)))

unwrap <- function(str) {
  strwrap(str, width=10000, simplify=TRUE)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

replaceNABy0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

replaceNaNBy0 <- function(x) {
  x[is.nan(x)] <- 0
  return(x)
}

replaceNullBy0 <- function(x) {
  x[is.null(x)] <- 0
  return(x)
}

is.empty <- function(x) {
  if(is.null(x)){
    return(TRUE)
  } else {
    nchar(x) == 0
  }
}

store.pdf <- function(data, where, name) {
  pdf(file.path(where, name))
  plot(data)
  dev.off()
}

plot.location <- function(filename) {
  file.path(filename)
}

save.plot <- function(filename) {
  ggsave(plot.location(filename))
}

save.pdf <- function(plot, filename, width = 7, height = 7) {
  pdf(plot.location(filename), height = height, width = width)
  print(plot)
  dev.off()
}


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

dt.types <- c('te', 'pr')
interval.special.types <- c('re.pr', 're.te', 'ty.pr', 'ty.te')
interval.types <- c('eo', 'ea', 'ua', 'ju', 'pe', interval.special.types)
temp.result.file <- file.path("reactions_to_failing_test.csv")


noreaction.type <- 'nr'
interval.levels <- interval.types
#interval.levels <- c(interval.types, noreaction.type)
reactions.to.failing.test <- data.frame(time=numeric(0), reaction=factor(levels(interval.levels)))
times <- c(0:60, seq(70,110,10), seq(120,300,20))
times <- times*1000  # in ms
times.max <- max(times)

reactions <- read.csv(temp.result.file)

# READ generated reactions
reactions.to.failing.test <- read.csv(temp.result.file)
reactions.to.failing.test$X <- NULL
reactions.to.failing.test$time <- reactions.to.failing.test$time/1000

aggregated <-
  aggregate(reactions.to.failing.test,
            by = list(reactions.to.failing.test$reaction, reactions.to.failing.test$time), length)

aggregated$reaction <- NULL
aggregated <- plyr::rename(aggregated, c("Group.1" = "reaction", "Group.2" = "time", "time" = "freq"))

accumulatedReactions <- data.frame(reaction=factor(levels=(interval.levels)),time=numeric(0),freq=numeric(0))

#aggregated$time <- aggregated$time * 1000
adj.times <- times / 1000
for(t in 1:length(adj.times)) {
  for(r in unique(aggregated$reaction)) {
    cur.row.val <- max(subset(aggregated, time==adj.times[t] & reaction==r)$freq, 0)
    prev.row.val <- max(subset(accumulatedReactions, time==adj.times[t-1] & reaction==r)$freq, 0)
    new.row.val <- cur.row.val + prev.row.val
    accumulatedReactions <- rbind(accumulatedReactions, data.frame(reaction=r, time=adj.times[t], freq=new.row.val))
  }
}

accumulatedReactions <- merge(accumulatedReactions, aggregate(freq ~ time, accumulatedReactions, sum), by.x = "time", by.y = "time")
accumulatedReactions$perc <- accumulatedReactions$freq.x / accumulatedReactions$freq.y

# Preparation for plotting
accumulatedReactions$Reaction <- accumulatedReactions$reaction
accumulatedReactions$reaction <- NULL

# Remove Eclipse closing
accumulatedReactions <- subset(accumulatedReactions, !(Reaction %in% c("eo", NA)))

from.factor <- c("re.pr",           "ea",             "ua",            "ty.pr",            "re.te",          "ty.te",           "ju",               "pe",              "eo")
to.factor <-   c("Read Prod. Code", "Switched Focus", "Were inactive", "Typed Prod. Code", "Read Test Code", "Typed Test Code", "Ran a Junit Test", "Switched Persp.", "Quit IDE")
accumulatedReactions$Reaction <- mapvalues(accumulatedReactions$Reaction, 
                                           from = from.factor, 
                                           to = to.factor)


# from http://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
myColors <- brewer.pal(length(to.factor),"Set2")
names(myColors) <- to.factor
colScale <- scale_colour_manual(name = "Reaction",values = myColors)

accumulatedReactions <- accumulatedReactions[accumulatedReactions$time < 201,]

colorPallette <- c("Read Prod. Code" = "#66C2A5", "Switched Focus" = "#FC8D62", "Were inactive" = "#8DA0CB", "Typed Prod. Code" = "#E78AC3",
  "Read Test Code" = "#A6D854", "Typed Test Code" = "#FFD92F", "Ran a Junit Test" = "#E5C494", "Switched Persp." = "#B3B3B3", 
  "Quit IDE" = "#786cbf")

shapePallette <- c("Read Prod. Code" = 1, "Switched Focus" = 2, "Were inactive" = 3, "Typed Prod. Code" = 4,
                   "Read Test Code" = 5, "Typed Test Code" = 6, "Ran a Junit Test" = 7, "Switched Persp." = 8, 
                   "Quit IDE" = 9)

p <-
  ggplot(data=accumulatedReactions, aes(x=time, y=perc, colour=Reaction)) +
  xlab("Time (s)") +
  geom_line(size=0.8) +
  geom_point(data=subset(accumulatedReactions, time==1 | time%%100==0), aes(shape = Reaction), size = 3.5) +
  ggplot.defaults +
  ylab("") +
  ylab("Frequency of Reaction") +
  scale_y_continuous(labels = percent, limits = c(0, 0.6) ) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)) +
  theme(legend.position="none") +
  theme(legend.position="bottom") +
  scale_colour_manual(values = colorPallette) +
  scale_shape_manual(values = shapePallette)


save.pdf(p, "rq3_immediate_reactions.pdf", width = 20, height = 7)
