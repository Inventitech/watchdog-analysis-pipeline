#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
library(ggplot2)

## Split and setup intervals
ua.intervals <- subset(intervals, it == 'ua')
re.intervals <- subset(intervals, it == 're')
ty.intervals <- subset(intervals, it == 'ty')

ua.time <- sumDuration(ua.intervals)
re.time <- sumDuration(re.intervals)
ty.time <- sumDuration(ty.intervals)

## RESULT: How much users are active reading, writing, and doing other active stuff in the IDE
re.overallPercent <- re.time/ua.time
ty.overallPercent <- ty.time/ua.time
ot.overallPercent <- 1 - (re.overallPercent + ty.overallPercent)


# calculate absoulte numbers for testing and production code.
## Split and setup intervals
projects.with.tests <- unique(subset(intervals, it == 're' & doc.dt %in% test.types)$pid)
projects.with.ju.tests <- unique(subset(intervals, it == 're' & doc.dt == "te")$pid)
projects.with.supposed.ju.tests <- unique(subset(intervals, usesJunit == 'Yes')$pid)
projects.with.supposed.ju.tests.intersect <- intersect(projects.with.ju.tests, projects.with.supposed.ju.tests)

intervals.with.tests <- subset(intervals, pid %in% projects.with.tests)
printf("projects with tests: %d", length(projects.with.tests))
printf("projects with exectuable tests: %d", length(projects.with.ju.tests))
printf("projects who say they have junit tests actually have (truth tellers): %f", length(projects.with.supposed.ju.tests.intersect)/length(projects.with.supposed.ju.tests))

ua.intervals <- subset(intervals.with.tests, it == 'ua')
re.intervals <- subset(intervals.with.tests, it == 're')
ty.intervals <- subset(intervals.with.tests, it == 'ty')

aggregate_duration_per_project <- function(intervals) {
  return(aggregate(intervals$duration,by=list(intervals$pid),sum))
}

time.per.proj <- c()
ty.intervals.test <- subset(ty.intervals, doc.dt %in% test.types)
ty.intervals.testTime <- aggregate_duration_per_project(ty.intervals.test)
names(ty.intervals.testTime) <- c("pid","test.ty")
# to validate. ty.intervals.testTime <- sum(ty.intervals.testTime$x)
re.intervals.test <- subset(re.intervals, doc.dt %in% test.types)
re.intervals.testTime <- aggregate_duration_per_project(re.intervals.test)
names(re.intervals.testTime) <- c("pid","test.re")


ty.intervals.prod <- subset(ty.intervals, doc.dt == 'pr')
ty.intervals.prodTime <- aggregate_duration_per_project(ty.intervals.prod)
names(ty.intervals.prodTime) <- c("pid","prod.ty")
re.intervals.prod <- subset(re.intervals, doc.dt == 'pr')
re.intervals.prodTime <- aggregate_duration_per_project(re.intervals.prod)
names(re.intervals.prodTime) <- c("pid","prod.re")

time.per.proj <- merge(re.intervals.testTime, ty.intervals.testTime, by=c('pid'), all=T)
time.per.proj <- replaceNABy0(time.per.proj)
time.per.proj$test <- time.per.proj$test.ty + time.per.proj$test.re

time.per.proj <- merge(time.per.proj, ty.intervals.prodTime, by=c('pid'), all=T)
time.per.proj <- merge(time.per.proj, re.intervals.prodTime, by=c('pid'), all=T)
time.per.proj <- replaceNABy0(time.per.proj)
time.per.proj$prod <- time.per.proj$prod.ty + time.per.proj$prod.re

prod.percent.est <- plyr::count(re.intervals, c('pid','productionPercentage'))
prod.percent.est$freq <- NULL
names(prod.percent.est) <- c('pid','prodEst')
time.per.proj <- merge(time.per.proj, prod.percent.est, by='pid')
time.per.proj <- subset(time.per.proj, !is.na(time.per.proj$prodEst))

time.per.proj$overall <- time.per.proj$test + time.per.proj$prod
time.per.proj$overallPerc <- time.per.proj$overall/sum(time.per.proj$overall)
# check: should be 1
# sum(time.per.proj$overallPerc)


## RESULT
printf("Mean results per project")
time.per.proj$prodPerc <- time.per.proj$prod/time.per.proj$overall*100
time.per.proj$testPerc <- time.per.proj$test/time.per.proj$overall*100
time.per.proj$diff <- time.per.proj$prodPerc - time.per.proj$prodEst
printf("Mean production perc (per project): %f", mean(time.per.proj$prodPerc, na.rm = T))
summary(time.per.proj$prodPerc)
printf("Mean test perc (per project): %f", mean(time.per.proj$testPerc, na.rm = T))
summary(time.per.proj$testPerc)
printf("Mean difference (per project): %f", mean(time.per.proj$diff, na.rm=T))
summary(time.per.proj$diff)
printf("Mean Estimated Production perc (per proc): %f", mean(time.per.proj$prodEst, na.rm=T))
summary(time.per.proj$prodEst)
p <- ggplot(time.per.proj) + 
  aes(x = diff) + 
  geom_histogram(binwidth=10) +
  geom_vline(xintercept=0, colour="red", linetype = "longdash") +
  ggplot.small.defaults + 
  xlab("Delta production-% reality vs. estimation (% points)") +
  ylab("Number of projects")
save.pdf(p, "rq5_hist_diff.pdf")

printf("Normalized results (per time)")
time.per.proj$prodRel = time.per.proj$prodPerc * time.per.proj$overallPerc
time.per.proj$testRel = time.per.proj$testPerc * time.per.proj$overallPerc
printf("Production perc (normalized): %f", sum(time.per.proj$prodRel))
printf("Test perc (normalized): %f", sum(time.per.proj$testRel))

time.per.proj$prodEstRel = time.per.proj$prodEst * time.per.proj$overallPerc
time.per.proj$testEstRel = (100-time.per.proj$prodEst) * time.per.proj$overallPerc
printf("Estimated prod perc (normalized): %f", sum(time.per.proj$prodEstRel, na.rm=T))
printf("Estimated test perc (normalized): %f", sum(time.per.proj$testEstRel, na.rm=T))

## sIDE-result
printf("Interesting sIDE-results")
time.test.ty <- sum(time.per.proj$test.ty)/sum(time.per.proj$test)
time.test.re <- sum(time.per.proj$test.re)/sum(time.per.proj$test)
printf("Typing in tests: %f", time.test.ty)
printf("Reading in tests: %f", time.test.re)

time.prod.ty <- sum(time.per.proj$prod.ty)/sum(time.per.proj$prod)
time.prod.re <- sum(time.per.proj$prod.re)/sum(time.per.proj$prod)
printf("Typing in prod: %f", time.prod.ty)
printf("Reading in prod: %f", time.prod.re)

time.per.proj$ty <- time.per.proj$test.ty + time.per.proj$prod.ty
time.per.proj$re <- time.per.proj$test.re + time.per.proj$prod.re
time.per.proj$ty.perc <- time.per.proj$ty/time.per.proj$overall
time.per.proj$re.perc <- time.per.proj$re/time.per.proj$overall

plots <- data.frame(time.per.proj$re.perc, time.per.proj$ty.perc)
names(plots) <- c('Reading','Typing')
boxplot(plots)

time.per.proj$test.re.perc <- time.per.proj$test.re/time.per.proj$test
time.per.proj$prod.re.perc <- time.per.proj$prod.re/time.per.proj$prod
time.per.proj$prod.re.perc <- replaceNABy0(time.per.proj$prod.re.perc)
time.per.proj$test.re.perc <- replaceNABy0(time.per.proj$test.re.perc)

## Result
# is the distribution in reading and writing in tests sig. different from in production
compute.effectSize(time.per.proj$test.re.perc, time.per.proj$prod.re.perc, "greater", T)

## RESULT: Although there are fewer ty intervals, they are much longer.
intervals.re <- subset(intervals, it == 're')
intervals.ty <- subset(intervals, it == 'ty')
summary(intervals.re$duration)
summary(intervals.ty$duration)
boxplot(intervals.re$duration, intervals.ty$duration, outline=F, names=c('Reading','Typing'))

compute.effectSize(intervals.re$duration, intervals.ty$duration, "less")
