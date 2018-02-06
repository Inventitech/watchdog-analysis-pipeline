#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ3 is concerneced with typical reactions to failing tests.

source('config.R')

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

registerDoMC(num.processes)

## Split and setup intervals
intervals.ju <- subset(intervals, it == 'ju')

# Extension: RQ3.1.1 
extract.individual.test.cases <- function(intervals, expected.test.result) {
  apply(intervals, 1, function(row) {
    test.cases <- vector("list")
    i <- 1
    for(tc in unflatten.tests(row["ju.tests"])) {
      if(extract.test.result(tc) %in% expected.test.result) {
        etc <- remove.test.results(tc)
        test.cases[[i]] <- etc
        i <- i + 1
      }
    }
    
    unlist(unique(test.cases))
  })
}

intervals.ju$tc.e <- extract.individual.test.cases(intervals.ju, test.failures)
intervals.ju$tc.o <- extract.individual.test.cases(intervals.ju, "O")

intervals.ju$tc.n.e <- apply(intervals.ju, 1, function(row) {
  tc.e <- unlist(row["tc.e"])
  
  length(tc.e)
})

intervals.ju.error <- subset(intervals.ju, ju.result %in% test.failures)
intervals.ju.ok <- subset(intervals.ju,!(ju.result %in% test.failures))

# RQ3.1
## RESULT
printf("#Errored executions: %d", nrow(intervals.ju.error))
printf("Percentage of errored executions: %f", nrow(intervals.ju.error)/nrow(intervals.ju)*100)

per.project.ju <- plyr::count(intervals.ju,c('pid'))
per.project.ju.error <- plyr::count(intervals.ju.error,c('pid'))
names(per.project.ju.error) <- c('pid','errors')
per.project.ju.ok <- plyr::count(intervals.ju.ok,c('pid'))
names(per.project.ju.ok) <- c('pid','oks')
per.project.ju <- merge(per.project.ju, per.project.ju.error, all=T)
per.project.ju <- merge(per.project.ju, per.project.ju.ok, all=T)
per.project.ju <- replaceNABy0(per.project.ju)
per.project.ju$errorPerc <- per.project.ju$errors/per.project.ju$freq
print(summary(per.project.ju$errorPerc))
hist(per.project.ju$errorPerc)
boxplot(per.project.ju$errorPerc)
print(shapiro.test(per.project.ju$errorPerc))

## RQ3.1 Extension: New Result: How many individual test cases (percentage) fail?
summary(intervals.ju.error$tc.n.e)
boxplot(intervals.ju.error$tc.n.e)
printf("Number of times just one test case fails: %d (of %d)", nrow(intervals.ju.error[intervals.ju.error$tc.n.e == 1,]), nrow(intervals.ju.error))

intervals.ju.error$tc.errorperc <- apply(intervals.ju.error, 1, function(row) {
  tc.e <- unlist(row["tc.e"])
  tc.o <- unlist(row["tc.o"])
  
  length(tc.e)/(length(tc.o)+length(tc.e))
})

summary(intervals.ju.error$tc.errorperc)
boxplot(intervals.ju.error$tc.errorperc)
sparkline(intervals.ju.error$tc.errorperc, "intervals.ju.error$tc.errorperc.pdf")


## RQ3.1 Extension: x% of test are responsible for 50% of test failures
# commented-out code concerns the removal of cases when there are really, really few test cases
steps <- seq(0,1,0.001)
project.failure.tc.percentages <- data.frame(pid=character(),classUse=numeric(),testFail=numeric(),stringsAsFactors = F)

pids <- unique(intervals.ju.error$pid)
r <- foreach(i=1:length(pids), .combine=rbind) %dopar% {
#for(i in 1:length(pids)) {
  project.failure.tc.percentages <- data.frame(pid=character(),classUse=numeric(),testFail=numeric(),stringsAsFactors = F)
  p <- pids[[i]]
  
  project.intervals.ju.error <- intervals.ju.error[pid == p,]
  project.intervals.ju.ok <- intervals.ju.ok[pid == p,]
  
  all.proj.tcs <- union(unlist(project.intervals.ju.ok$tc.o),unlist(project.intervals.ju.error$tc.o))
  if(debug.output)
    printf("Project %s: Number of executed test cases %s", p, length(all.proj.tcs))
  
  if(length(all.proj.tcs) < 10)
    return(project.failure.tc.percentages)
    #next
  
  tb <- data.frame(Tc=character(),Freq=integer(),stringsAsFactors = F)
  for(tc in unlist(project.intervals.ju.error$tc.e)) {
    row <- tb[tb$Tc == tc,]
    n.rows <- nrow(row)
    if(n.rows == 0) {
      tb <- bind_rows(tb, data.frame(Tc=tc,Freq=1))
    } else if(n.rows == 1) {
      assert("selected row should have exactly 1 element", nrow(row) == 1)
      row$Freq <- row$Freq + 1
      tb[tb$Tc == tc,] <- row
    }
    else {
      assert("This should never be called. If you see this, there is an error in RQ3.1.", F)
    }
  }
  # Whenever there is a test case that is not included in the temporary table, we add it with an error frequency of 0
  for(tc in all.proj.tcs) {
    row <- tb[tb$Tc == tc,]
    n.rows <- nrow(row)
    if(n.rows == 0) {
      tb <- bind_rows(tb, data.frame(Tc=tc,Freq=0))
    } 
  }
  setorder(tb, -Freq)
  total <- sum(tb$Freq)
  
  if(debug.output) {
    print(tb)
    print("-------------------")
  }
  
  failure.perc <- NULL
  for(step in steps) {
    # floor gives a conservative over estimation; round might be accurate; the use of ceiling questionable.
    failure.perc <- sum(head(tb, round(step*nrow(tb)))$Freq)/total
    project.failure.tc.percentages <- rbind(project.failure.tc.percentages,data.frame(p,step,failure.perc))
  }
  project.failure.tc.percentages
}
project.failure.tc.percentages <- rbind(project.failure.tc.percentages, r)

# Annotate projects with project ids
project.failure.tc.percentages <- plyr::rename(project.failure.tc.percentages, c("p" = "id"))
project.failure.tc.percentages$IDE <- join(project.failure.tc.percentages, projects, by = "id", match = "first")$IDE

p <-
ggplot(project.failure.tc.percentages, aes(step, failure.perc, colour=IDE)) +
  stat_summary(fun.y = "mean", size = 0.8, geom = "line", alpha = 0.6) +
  geom_smooth() +
  ylab("Ratio of Induced Test Failures") +
  xlab("Ratio of All Executed Test Cases") +
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(labels = percent) +
  scale_colour_few() +
  ggplot.defaults

save.pdf(p, "rq31_testclass_error_responsibility.pdf", width = 9, height = 8)
pdot <- p + geom_point(size=0.6, alpha = 1/45) 
save.pdf(pdot, "rq31_testclass_error_responsibility_with_p_alpha45.pdf", width = 9, height = 8)
pdot <- p + geom_point(size=0.6, alpha = 1/100) 
save.pdf(pdot, "rq31_testclass_error_responsibility_with_p_alpha100.pdf", width = 9, height = 8)

printf("This is based on an analysis of xx projects per IDE")
included.projects.per.IDE <- aggregate(id ~ IDE, project.failure.tc.percentages, function(x) { length(unique(x)) } )
print(included.projects.per.IDE)

## RESULT: Do failing tests stop faster than OK'ed tests?
print(summary(intervals.ju.error$duration/1000))
print(summary(intervals.ju.ok$duration/1000))
boxplot(intervals.ju.ok$duration/1000, intervals.ju.error$duration/1000, outline=F, names=c('OK','ERROR'))
# TODO (MMB) Make nicer ggplot2 boxplot?

print("Do failing tests stop faster than OK'ed tests?")
compute.effectSize(intervals.ju.ok$duration, intervals.ju.error$duration, direction="greater")


# RQ 3.2 Whats the immediate reaction to a failing test?
# "Super"-fast algorithm
set_ts_to_te <- function(x, ivals) {
  # Non-junit intervals.copy start after the end of junit execution (later the intervals.copy are ordered by "starting time" ts)
  ivals[which(ivals$it==x),]$ts <- ivals[which(ivals$it==x),]$te
  ivals
}

# replace slow match by fast match generally
old_match <- match
match <- fmatch

# options(digits=22)
# Non-junit intervals.copy start after the end of junit execution (later the intervals.copy are ordered by "starting time" ts)
intervals.sequence <- set_ts_to_te("ua",intervals)
intervals.sequence <- set_ts_to_te("eo",intervals.sequence)
intervals.sequence <- set_ts_to_te("ea",intervals.sequence)
# TODO (MMB) It does not seem to make sense to shuffle around ju intervals. Why did we do it?
# intervals.sequence <- set_ts_to_te("ju",intervals.sequence)
intervals.sequence <- arrange(intervals.sequence, ts)
intervals.sequence <- data.table(intervals.sequence)
# print(intervals.sequence)
ju.intervals.failing <- intervals.sequence[it == 'ju' & ju.result %in% test.failures,]

dt.types <- c('te', 'pr')
interval.special.types <- c('re.pr', 're.te', 'ty.pr', 'ty.te')
interval.types <- c('eo', 'ea', 'ua', 'ju', 'pe', interval.special.types)
temp.result.file <- file.path(pRes, "reactions_to_failing_test.csv")


noreaction.type <- 'nr'
interval.levels <- interval.types
#interval.levels <- c(interval.types, noreaction.type)
reactions.to.failing.test <- data.frame(time=numeric(0), reaction=factor(levels(interval.levels)))
times <- c(0:60, seq(70,110,10), seq(120,300,20))
times <- times*1000  # in ms
times.max <- max(times)

ju.intervals.number <- nrow(ju.intervals.failing)

reactions.list <- foreach (ju.t = seq(1,nrow(ju.intervals.failing),1)) %dopar% {
  ju <- ju.intervals.failing[ju.t,]
  # Ordering assumes that ts is monotonically increasing
  intervals.following <- intervals.sequence[pid == ju$pid &
                                              session == ju$session &
                                              ts >= ju$te &
                                              ts <= ju$te + times.max, ]
  results = vector("list", 100)
  results.idx = 1

  for(time.ind in seq(1,length(times))) {
    time <- times[time.ind]
    diff.to.previous.time <- max(0, times[time.ind-1])
    intervals.time <- intervals.following[ ts > ju$te + diff.to.previous.time &
                                             ts < ju$te + time, ]
    for(type in interval.types) {
      if(type %in% interval.special.types) {
        type.split <- strsplit(type, split = ".", fixed = T)[[1]]
        if(type.split[2] == "te") {
          type.found <- nrow(subset(intervals.time, it == type.split[1] & doc.dt %in% test.types)) > 0
        } else {
          type.found <- nrow(subset(intervals.time, it == type.split[1] & doc.dt == type.split[2])) > 0
        }
      } else {
        type.found <- nrow(subset(intervals.time, it == type)) > 0
      }

      if(type.found) {
        results[[results.idx]] <- list(time=time, reaction=type)
        results.idx <- results.idx + 1
      }
    }
  }
  if(debug.output) printf("Analyzed reactions to test failure %d/%d", ju.t, ju.intervals.number)
  rbindlist(results)
}

reactions <- rbindlist(reactions.list)

match <- old_match
write.csv(reactions, file = temp.result.file)
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


# Disabled because it did not generate astable palette and we supply our own later
# from http://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
#myColors <- brewer.pal(length(to.factor),"Set2")
#names(myColors) <- to.factor
#colScale <- scale_colour_manual(name = "Reaction",values = myColors)

accumulatedReactions <- accumulatedReactions[accumulatedReactions$time < 201,]

colorPallette <- c("Read Prod. Code" = "#66C2A5", "Switched Focus" = "#FC8D62", "Were inactive" = "#8DA0CB", "Typed Prod. Code" = "#E78AC3",
  "Read Test Code" = "#A6D854", "Typed Test Code" = "#FFD92F", "Ran a Junit Test" = "#E5C494", "Switched Persp." = "#B3B3B3", 
  "Quit IDE" = "#786cbf")

p <-
  ggplot(data=accumulatedReactions, aes(x=time, y=perc, colour=Reaction)) +
  xlab("Time (s)") +
  geom_line(size=0.8) +
  geom_point(data=subset(accumulatedReactions, time==1 | time%%100==0), aes(shape = Reaction), size = 3.5) +
  ggplot.defaults +
  ylab("Frequency of reaction") +
  scale_y_continuous(labels = percent) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)) +
  #theme(legend.position="none") +
  scale_colour_manual(values = colorPallette)

save.pdf(p, "rq3_immediate_reactions.pdf", width = 14, height = 7)


# RQ 3.3 How long does it take to fix a failing test?
# for each fn, a duration of how long it took to fix it.
time.to.fix.test <- data.frame(pid=character(0), ss=character(0), fn=character(0), duration=numeric(0))
never.fixed.tests <- data.frame(pid=character(0), ss=character(0), fn=character(0))

ju.intervals = subset(intervals, intervals$it == 'ju')
for(p in unique(ju.intervals$pid)) {
  for(s in unique(subset(ju.intervals, pid == p)$session)) {
    # Ordering assumes that ts is monotonically increasing
    session.ju.ordered <- arrange(subset(ju.intervals, pid == p & session == s), ts)
    failing.tests <- data.frame(fn=character(0), te=double(0))

    if(debug.output) print("--------------------")
    for (t in seq(1, nrow(session.ju.ordered), 1)) {
      interval <- session.ju.ordered[t,]
      cur.tests  <- unflatten.tests(interval$ju.tests)
      for(cur.test in cur.tests) {
        cur.test.res <- extract.test.result(cur.test)
        cur.test <- remove.test.results(cur.test)

        if(cur.test.res %in% test.failures && !any(cur.test %in% failing.tests$fn)) {
          if(debug.output) printf("s%s: Added test %s because failing and not there yet", s, cur.test)
          failing.tests <- rbind(failing.tests, data.frame(fn=cur.test, te=interval$te))
        } else if(cur.test.res == 'O' && any(cur.test %in% failing.tests$fn)) {
          if(debug.output) printf("Congratualations, you fixed test %s !", cur.test)
          pte <- failing.tests[failing.tests$fn == cur.test,]
          delta <- interval$ts-pte$te
          time.to.fix.test <- rbind(time.to.fix.test, data.frame(pid=p, ss=s, fn=pte$fn, duration=delta))
          failing.tests <- failing.tests[!failing.tests$fn == cur.test,]
        }
      }
    }

    # add remaining failing tests to the never fixed tests
    for(cur.test in failing.tests$fn) {
      never.fixed.tests <- rbind(never.fixed.tests, data.frame(pid=p, ss=s, fn=cur.test))
    }
  }
}

# filter out results where the fixing time is negative. Sanity checking
time.to.fix.test <- time.to.fix.test[time.to.fix.test$duration >= 0,]
time.to.fix.test$duration <- time.to.fix.test$duration/1000

printf("First failing, later fixed cases: %i", nrow(time.to.fix.test))
hist(time.to.fix.test$duration)
boxplot(time.to.fix.test$duration)
time.to.fix.test$x <- 1:nrow(time.to.fix.test)
time.to.fix.test$duration <- time.to.fix.test$duration/60

# compute lower and upper whiskers
ylim1 = boxplot.stats(time.to.fix.test$duration)$stats[c(1, 5)]

ggplot(as.data.frame(time.to.fix.test)) + aes(x=x, y=duration) + geom_boxplot() +
  ggplot.small.defaults + coord_cartesian(ylim = ylim1*1.05) + theme(axis.text.x = element_blank()) + theme(axis.text.y = element_text(size = 25))
save.plot("rq3_time_to_fix_a_test.pdf")
summary(time.to.fix.test$duration)
sparkline(time.to.fix.test$duration, "time-fix-test-spark.pdf")

## Extension: How many test cases were never fixed?
printf("# of never fixed test cases: %d", nrow(never.fixed.tests))
printf("# of unique test cases: %d", nrow(never.fixed.tests)+nrow(time.to.fix.test))

# This was pretty hefty. Always good to clean things up.
gc()