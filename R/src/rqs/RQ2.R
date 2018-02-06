#
# (c) 2015 - onwards Georgios Gousios <gousiosg@gmail.com>
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ2: Test run duration

library(ggplot2)
library(plyr)
ju.intervals <- subset(intervals, it == 'ju')

## RESULT: summary on test execution length
summary(ju.intervals$duration / 1000)
sparkline(ju.intervals$duration / 1000, "test-execution-spark.pdf")

printf("Percentage of tests > 30 s: %f", sum(ju.intervals$duration / 1000 > 30) / nrow(ju.intervals))
printf("Percentage of tests > 60 s: %f", sum(ju.intervals$duration / 1000 > 60) / nrow(ju.intervals))
printf("Percentage of tests > 120 s: %f", sum(ju.intervals$duration / 1000 > 120) / nrow(ju.intervals))


# Mean test run duration per project/session
mean.duration <- aggregate(ju.duration ~ pid, data = ju.intervals, FUN = mean)
mean.duration <- rename(mean.duration, replace = c('ju.duration' = 'mean.duration'))

# Median test run duration per project/session
median.duration <- aggregate(ju.duration ~ pid, data = ju.intervals, FUN = median)
median.duration <- rename(median.duration, replace = c('ju.duration' = 'median.duration'))

# Number of executions per project/session
num.executions <- aggregate(ju.duration ~ pid, data = ju.intervals, FUN = length)
num.executions <- rename(num.executions, replace = c('ju.duration' = 'num.executions'))

# RESULT: how many test cases per test execution?
summary(ju.intervals$ju.number)
sparkline(ju.intervals$ju.number, "test-cases-per-run-spark.pdf")

boxplot(ju.intervals$ju.number)
printf("Percentage of > 1 tests: %f", sum(ju.intervals$ju.number > 1) / nrow(ju.intervals))
printf("Percentage of > 5 tests: %f", sum(ju.intervals$ju.number > 5) / nrow(ju.intervals))
printf("Percentage of > 10 tests: %f", sum(ju.intervals$ju.number > 10) / nrow(ju.intervals))
printf("Percentage of > 20 tests: %f", sum(ju.intervals$ju.number > 20) / nrow(ju.intervals))
printf("Percentage of > 50 tests: %f", sum(ju.intervals$ju.number > 50) / nrow(ju.intervals))

# two-way permutation test: do more experienced user run more tests?
# 1) preparing the data
interaction.rq2 <- subset(ju.intervals, select=c('uid','ju.number','IDE.x'))
interaction.rq2$userExperience = ""
user.subset <- unique(interaction.rq2$uid)
for (userId in user.subset){
  userExperience <- users[which(users$uid == userId), ]$programmingExperience[1]
  userExperience <- as.character(userExperience)
  interaction.rq2[ interaction.rq2$uid == userId, ]$userExperience = userExperience
}

significant <- two.way.permutation.test('ju.number','userExperience', interaction.rq2) 
if (significant) {
  ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
  plot.2way.interaction2('ju.number','userExperience', interaction.rq2[interaction.rq2$ju.number>1], ordering, y.label="# JUnit Sessions", x.label="Programming Experience")
}

significant <- two.way.permutation.test('ju.number','IDE.x', interaction.rq2) 
if (significant){
  interaction.rq2[interaction.rq2$IDE.x=="ec"]$IDE.x = "Eclipse"
  interaction.rq2[interaction.rq2$IDE.x=="ij"]$IDE.x = "IntelliJ"
  plot.2way.interaction('ju.number','IDE.x', interaction.rq2[interaction.rq2$ju.number>1], y.label="# JUnit Sessions", x.label="IDE")
}

# RESULT: Different number of test cases depending on test result
ju.intervals.failing <- subset(ju.intervals, ju.result %in% test.failures)
ju.intervals.ok <- subset(ju.intervals, !(ju.result %in% test.failures))
boxplot(ju.intervals.ok$ju.number, ju.intervals.failing$ju.number, names = c('OK','Error'))
compute.effectSize(ju.intervals.ok$ju.number, ju.intervals.failing$ju.number, direction="less")

# format of ju.stats is
# project session number.junit.executions mean(time.ju) median(time.ju)
ju.stats <- merge(mean.duration, num.executions)
ju.stats <- merge(ju.stats, median.duration)

## RESULT: Do fast tests trigger/facilitate/provoke more executions?
#cor.test(ju.stats$mean.duration, ju.stats$num.executions, method="spearman")
interpret.spearman('Test run mean duration', 'Test run executions',
                   cor.test(ju.stats$mean.duration, ju.stats$num.executions, method="spearman"),
                   "RQ2 - Do fast tests provoke more executions?:")


## RESULT: Test selection, @gousiosg's interpretation
# Test selection takes place if two consecutive runs contain common tests
# and the first of those runs contains more tests than the second
# We basically notice 2 types of test selection
# * Run all tests - 1 (supposedely, a failing test they try to avoid)
# * Run only one test (supposedely, a failing test they try to fix)
test.selection.gousiosg <- data.frame(pid = c(), session = c(), test.run = c(),
                                      tests.before = c(), tests.after = c(),
                                      tests.common = c())
for(p in unique(ju.intervals$pid)) {
  for(s in unique(subset(ju.intervals, pid == p)$session)) {
    # Ordering assumes that ts is monotonically increasing
    session.ju.ordered <- arrange(subset(ju.intervals, pid == p & session == s), ts)

    # No test selection can occur with just 1 test run
    if (nrow(session.ju.ordered) < 2) {next}

    for (t in seq(1, nrow(session.ju.ordered) - 1, 1)) {
      cur.tests  <- unflatten.tests(session.ju.ordered[t,]$ju.tests)
      cur.tests <- Map(remove.test.results, cur.tests)

      next.tests <- unflatten.tests(session.ju.ordered[t + 1,]$ju.tests)
      next.tests <- Map(remove.test.results, next.tests)

      if (length(next.tests) < length(cur.tests) & length(intersect(next.tests, cur.tests)) > 0) {
        if(debug.output) printf("Test selection: Project: %s, Session: %s, run: %d (%d total tests) contains %d common tests with run: %d (%d total tests)",
               p, s, t, length(cur.tests), length(intersect(next.tests, cur.tests)), t + 1, length(next.tests))
        test.selection.gousiosg <-
          rbind(test.selection.gousiosg, data.frame(pid = p, session = s, test.run = t,
                                                    tests.before = length(cur.tests),
                                                    tests.after  = length(next.tests),
                                                    tests.common = length(intersect(next.tests, cur.tests))))
      }
    }
  }
}

test.selection.gousiosg$scenario.1 <- test.selection.gousiosg$tests.before - test.selection.gousiosg$tests.common >= 2 &
  (test.selection.gousiosg$tests.after > 0.5 * test.selection.gousiosg$tests.before)
test.selection.gousiosg$scenario.2 <- test.selection.gousiosg$tests.before - test.selection.gousiosg$tests.after - test.selection.gousiosg$tests.common >= 0

printf("Test selection, Georgios, cases: %f", nrow(test.selection.gousiosg))
summary(test.selection.gousiosg)

printf("Test selection, scenario 1: %f", nrow(subset(test.selection.gousiosg, scenario.1 == T))/ nrow(test.selection.gousiosg))
printf("Test selection, scenario 2: %f", nrow(subset(test.selection.gousiosg, scenario.2 == T))/ nrow(test.selection.gousiosg))

## RESULT: Test selection, @Inventitech's interpretation
# Create list of all unique tests run per project
# result is one row per testrun. percentage calculated as
# number of test cases executed vs. number of test cases
# encountered in the project over all sessions
project.tests <- data.frame(project = c(), test = c())
for(p in unique(ju.intervals$pid)) {
  test.runs <- subset(ju.intervals, pid == p)$ju.tests
  all.tests <- unique(unlist(Map(function(x){
    (Map(remove.test.results, unlist(unflatten.tests(x))))
  }, test.runs)))
  if(length(all.tests) != 0) {
  project.tests <- rbind(project.tests, data.frame(project = p, test = all.tests)) }
}

calc.test.selection.inventitech <- function(ju.tests, pid) {
  a <- unlist(Map(remove.test.results, unflatten.tests(ju.tests)))
  b <- subset(project.tests, as.character(project) == as.character(pid))$test
  if(debug.output) printf("%d test run, %d total tests", length(a), length(b))
  length(intersect(a, b)) / length(b)
}

test.selection.inventitech <- subset(ju.intervals, select = c(pid, session))
test.selection.inventitech$test.perc <-
  ju.intervals[, calc.test.selection.inventitech(ju.tests, pid), by = 1:nrow(ju.intervals)]$V1

printf("Test selection, Inventitech, cases: %f", nrow(test.selection.inventitech))
boxplot(test.selection.inventitech$test.perc)
summary(test.selection.inventitech$test.perc)
#sparkline(test.selection.inventitech$test.perc, "test-selection-spark.pdf")

printf("Select 1 test out of all available: %f", 1-sum(test.selection.inventitech$test.perc == 1, na.rm = T)/nrow(test.selection.inventitech))
printf("No test selection: %f", sum(test.selection.inventitech$test.perc == 1, na.rm = T)/nrow(test.selection.inventitech))
# Aggregate
test.selection.mean <- aggregate(test.perc ~ pid , test.selection.inventitech, mean)

ggplot(test.selection.mean) + aes(x = test.perc) + geom_histogram(binwidth = 0.05) + theme_bw()
