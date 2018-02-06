#
# (c) 2015 - onwards Annibale Panichella
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ4 wants to detect TDD.

source('src/tdd_utils.R')
library(doBy)

intervals.copy <- intervals
# we select only interval of types "ju" and "ty"
intervals.copy <- subset(intervals.copy, intervals.copy$it=="ju" | intervals.copy$it=="ty")
intervals.copy[which(intervals.copy$it=="ju"),]$ts <- intervals.copy[which(intervals.copy$it=="ju"),]$te # Non-junit intervals.copy start after the end of junit execution (later the intervals.copy are ordered by "starting time" ts)

#list of interval sequences by project
results <- vector(mode = "list", length = length(intervals.copy[,1]))

counter <- 0
for(s in unique(intervals.copy$session)) {
  for(p in unique(subset(intervals.copy, session == s)$pid)) {
    intevals.subset <- subset(intervals.copy, pid == p & session == s)
    contains_junit <- (length(subset(intevals.subset, intevals.subset$it=="ju")$it) > 0)

    if (contains_junit) {
      #order table by "ts"
      #intevals.subset <- intevals.subset[with(intevals.subset, order(ts)), ]

      intevals.subset <- arrange(intevals.subset, ts)

      #find all test cases in the current table
      test.set <- find.all.testcases(intevals.subset)
      n.testcases <- length(test.set)
      if (n.testcases>0){
        # first we IDEntify the size of the sequence
        sequence.size <- nchar(create.sequence(intevals.subset, test.set[[1]]))

        # these vectors of boolean store whether each interval matchs or not the TDD
        boolean.strict <- matrix(FALSE, 1, sequence.size)
        boolean.lenient <- matrix(FALSE, 1, sequence.size)
        boolean.refactor <- matrix(FALSE, 1, sequence.size)
        flaky.is.flaky <- matrix(FALSE, 1, n.testcases)
        flaky.flakiness.of.tc <- matrix(0, 1, n.testcases)
        flaky.tc <- matrix("", 1, n.testcases)
        
        # then we cylce over all the test cases
        i <- 1
        for (test in test.set){
            if(debug.output) printf("[RQ4:  finding TDD pattern]: Test Case %s", test)
            sequence <- create.sequence(intevals.subset, test)
            # TDD
            boolean.strict <- boolean.union(boolean.strict, tdd.boolean.matching(tdd.strict, sequence))
            boolean.lenient <- boolean.union(boolean.lenient, tdd.boolean.matching(tdd.lenient, sequence))
            boolean.refactor <- boolean.union(boolean.refactor, tdd.boolean.matching(tdd.refactor, sequence))
            # Flaky test detection
            flaky.flakiness.of.tc[i] <- tdd.matching(flaky.testcase.pattern, sequence)/2/number.of.testcase.executions(sequence)
            flaky.is.flaky[i] <- flaky.flakiness.of.tc[i] > 0
            if(!is.na(flaky.is.flaky[i]) && flaky.is.flaky[i]) {
              flaky.tc[i] <- test
            }
            i <- i + 1
        }
        counter <- counter + 1
        
        results[[counter]] <- list (
            uid =  intevals.subset$uid[1],
            session = s,
            pid = p,
            # TODO (AP) results seems to be aggregated per session, not test case. Why then report tc on this level?
            tc = test,
            seq.length = nchar(sequence)/2,
            tdd.lenient.length = sum(boolean.lenient)/2,
            tdd.strict.length = sum(boolean.strict)/2,
            tdd.refactor.length = sum(boolean.refactor)/2,
            flaky.n.testcases = n.testcases,
            flaky.percentage = sum(flaky.is.flaky)/n.testcases,
            flaky.flakiness.of.tc = mean(flaky.flakiness.of.tc),
            flaky.tcs = list(flaky.tc)
          )
      }
    }
    if(debug.output) printf("[RQ4:  finding TDD pattern]: Project %s  Session %s", p, s)
  } # for project
} # for session

results <- rbindlist(results, fill = T)
printf("[gen-tdd-analysis.csv]: tdd_analysis.csv")
tdd.analystics.file <- file.path(pRes, 'tdd_analysis.Rdata')
save(results, file = tdd.analystics.file)
#results <- load(tdd.analystics.file)

# Results flaky tests

ju.intervals <- subset(intervals, it == 'ju')

### Results for RQ4
users.with.tests <- unique(ju.intervals$uid)
tot.users <- unique(intervals$uid)
sessions.with.tests <- unique(subset(ju.intervals, select=c('pid','session')))
projects.with.tests <- unique(subset(intervals, it == 're' & doc.dt %in% test.types)$pid)
## Strict TDD process
print("----------------------------------")
print(" ### Results for STRICT DD")
strict.users <- unique(as.character(subset(results, results$tdd.strict.length>0)$uid))
printf("Users using STRITC TDD %s", length(strict.users))
printf("Perc. of users using STRITC TDD %s", length(strict.users)/length(tot.users)*100)
printf("Perc. of users wit JUnt intervals using STRITC TDD %s", length(strict.users)/length(users.with.tests)*100)

# Add refactoring to those users who use TDD
refactoring <- (results$tdd.strict.length>0) * results$tdd.refactor.length
results$tdd.strict.match<-replaceNABy0(refactoring/results$seq.length)
# Compute percentage of macthing with TDD for each user
user.analysis <- summaryBy(tdd.strict.match ~ uid, data = results , FUN = function(x) { c(min = min(x), max=max(x), mean = mean(x), sd = sd(x)) } )
printf("Users with STRITC TDD matching > 20/100 = %s", nrow(subset(user.analysis, tdd.strict.match.mean >0.20)))
printf("Users with STRITC TDD matching > 5/1000 = %s", nrow(subset(user.analysis, tdd.strict.match.mean >0.05)))

# See percentage of refactoring intervals in strict TDD ('tdd.strict.length' is a subset of 'tdd.refactor.length')
refactoring.alone <- (results$tdd.strict.length>0) * (results$tdd.refactor.length-results$tdd.strict.length)
results$tdd.refactoring.match<-replaceNABy0(refactoring.alone/results$seq.length)
refactoring.users <- unique(as.character(subset(results, tdd.refactoring.match>0)$uid))
printf("Users with STRITC TDD applying refactoring ===> %s", length(refactoring.users))
user.analysis.refactor <- summaryBy(tdd.refactoring.match ~ uid, data = results , FUN = function(x) { c(min = min(x), max=max(x), mean = mean(x), sd = sd(x)) } )
printf("Users with STRITC TDD applying refactoring> 20/100 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.20)))
printf("Users with STRITC TDD applying refactoring > 5/1000 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.05)))
printf("Users with STRITC TDD applying refactoring > 4/1000 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.04)))

refactoring.perc <- replaceNABy0(user.analysis.refactor$tdd.refactoring.match.mean/user.analysis$tdd.strict.match.mean)
refactoring.perc <- refactoring.perc[(refactoring.perc>0)]
printf("Perc. of strict TDD intervals devoted to refactoring activities %s: ", mean(refactoring.perc))

# See experience of users
user.experience <- merge(subset(user.analysis, tdd.strict.match.mean>0), users, by="uid")
printf("Users (STRICT TDD) with experince < 1 year ===>  %s", sum(user.experience$programmingExperience=="< 1 year"))
printf("Users (STRITC TDD) with experince 1-2 years ===>  %s", sum(user.experience$programmingExperience=="1-2 years"))
printf("Users (STRITC TDD) with experince 3-6 years ===>  %s", sum(user.experience$programmingExperience=="3-6 years"))
printf("Users (STRITC TDD) with experince 7-10 years ===>  %s", sum(user.experience$programmingExperience=="7-10 years"))
printf("Users (STRITC TDD) with experince > 10 years ===>  %s", sum(user.experience$programmingExperience=="> 10 years"))

# See data for session and project
strict.session <- unique(subset(results, results$tdd.strict.length>0), select=c('pid','session'))
strict.project <- unique(as.character(subset(results, results$tdd.strict.length>0)$pid))
printf("Perc. of sessions with JUnit having STRITC TDD %s", nrow(strict.session)/nrow(sessions.with.tests)*100)
printf("Perc, of projects with JUnit having STRITC TDD %s", length(strict.project)/length(projects.with.tests)*100)

## Lenient TDD process
print("----------------------------------")
print(" ### Results for LENIENT DD")
lenient.users <- unique(as.character(subset(results, results$tdd.lenient.length>0)$uid))
printf("Users using Lenient TDD %s", length(lenient.users))
printf("Perc. of users using Lenient TDD %s", length(lenient.users)/length(tot.users)*100)
printf("Perc. of users wit JUnt intervals using Lenient TDD %s", length(lenient.users)/length(users.with.tests)*100)

# Add refactoring to those users who use TDD
refactoring <- c()
for (i in 1:length(results$tdd.lenient.length)){
  if (results$tdd.refactor.length[i] < results$tdd.lenient.length[i])
    { refactoring[i] <- results$tdd.lenient.length[i]}
  else
    { refactoring[i] <- (results$tdd.lenient.length[i]>0) * results$tdd.refactor.length[i]}
}
results$tdd.lenient.match<-replaceNABy0(refactoring/results$seq.length)
# Compute percentage of macthing with TDD for each user
user.analysis <- summaryBy(tdd.lenient.match ~ uid, data = results , FUN = function(x) { c(min = min(x), max=max(x), mean = mean(x), sd = sd(x)) } )
printf("Users with Lenient TDD matching > 20/100 = %s", nrow(subset(user.analysis, tdd.lenient.match.mean >0.20)))
printf("Users with Lenient TDD matching > 20/100 = %s", nrow(subset(user.analysis, tdd.lenient.match.mean >0.10)))
printf("Users with Lenient TDD matching > 5/1000 = %s", nrow(subset(user.analysis, tdd.lenient.match.mean >0.05)))

# See percentage of refactoring intervals in lenient TDD ('tdd.lenient.length' is NOT a subset of 'tdd.refactor.length')
refactoring.alone <- refactoring - results$tdd.lenient.length
results$tdd.refactoring.match<-replaceNABy0(refactoring.alone/results$seq.length)
refactoring.users <- unique(as.character(subset(results, tdd.refactoring.match>0)$uid))
printf("Users with Lenient TDD applying refactoring ===> %s", length(refactoring.users))
user.analysis.refactor <- summaryBy(tdd.refactoring.match ~ uid, data = results , FUN = function(x) { c(min = min(x), max=max(x), mean = mean(x), sd = sd(x)) } )
printf("Users with Lenient TDD applying refactoring> 20/100 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.20)))
printf("Users with Lenient TDD applying refactoring > 5/1000 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.05)))
printf("Users with Lenient TDD applying refactoring > 4/1000 = %s", nrow(subset(user.analysis.refactor, tdd.refactoring.match.mean >0.04)))

refactoring.perc <- replaceNABy0(user.analysis.refactor$tdd.refactoring.match.mean/user.analysis$tdd.lenient.match.mean)
refactoring.perc <- refactoring.perc[(refactoring.perc>0)]
printf("Perc. of Lenient TDD intervals devoted to refactoring activities %s: ", mean(refactoring.perc))


# See experience of users
user.experience <- merge(subset(user.analysis, tdd.lenient.match.max>0), users, by="uid")
printf("Users (Lenient TDD) with experince < 1 year ===>  %s", sum(user.experience$programmingExperience=="< 1 year"))
printf("Users (Lenient TDD) with experince 1-2 years ===>  %s", sum(user.experience$programmingExperience=="1-2 years"))
printf("Users (Lenient TDD) with experince 3-6 years ===>  %s", sum(user.experience$programmingExperience=="3-6 years"))
printf("Users (Lenient TDD) with experince 7-10 years ===>  %s", sum(user.experience$programmingExperience=="7-10 years"))
printf("Users (Lenient TDD) with experince > 10 years ===>  %s", sum(user.experience$programmingExperience=="> 10 years"))

# permutation tests for users' experience and TDD matches
# question: do users with different (e.g., higher) experience follow more/less the TDD?
significant <- two.way.permutation.test("tdd.lenient.match.mean","programmingExperience", user.experience)
#if (significant) {
ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
plot.2way.interaction2("tdd.lenient.match.mean","programmingExperience", user.experience, ordering, y.label="% TDD Match", x.label="Programming Experience", log.scale=FALSE)
#}

# See data for session and project
lenient.session <- unique(subset(results, results$tdd.lenient.length>0), select=c('pid','session'))
lenient.project <- unique(as.character(subset(results, results$tdd.lenient.length>0)$pid))
printf("Perc. of sessions with JUnit having Lenient TDD %s", nrow(lenient.session)/nrow(sessions.with.tests)*100)
printf("Perc, of projects with JUnit having Lenient TDD %s", length(lenient.project)/length(projects.with.tests)*100)

#
projects.claiming.tdd <- unique(projects[projects$followTestDrivenDesign == "Yes",]$id)
projects.using.tdd <- results[results$tdd.lenient.length>0,]$pid

printf("projects claiming TDD %i", length(projects.claiming.tdd))
printf("projects doing (lenient) TDD %i", length(projects.using.tdd))
printf("projects claiming AND doing TDD %i", length(intersect(projects.claiming.tdd, projects.using.tdd)))

# RESULT: extension of RQ4
results <- as.data.frame(results)
n.flaky.tests.per.session <- results$flaky.n.testcases*results$flaky.percentage
printf("Number of flaky test cases per session %f", mean(n.flaky.tests.per.session))
summary(n.flaky.tests.per.session)

printf("SUM of flaky test cases per session per project %f", sum(n.flaky.tests.per.session, na.rm = TRUE))

if(sum(n.flaky.tests.per.session, na.rm = TRUE) > 0) {
  # Flaky percentage per session
  print(summary(results$flaky.percentage))
  boxplot(results$flaky.percentage)
  sparkline(results$flaky.percentage, "test-flaky-percentage-per-session-spark")
  
  # Results per project: Flaky Percentage
  results.per.project <- aggregate(cbind(flaky.percentage, flaky.n.testcases, flaky.flakiness.of.tc) ~ pid, data = results, FUN=mean)
  n.flaky.tests.per.project <- results.per.project$flaky.n.testcases*results.per.project$flaky.percentage
  printf("Number of flaky test cases per session per project %f", mean(n.flaky.tests.per.project))
  print(summary(n.flaky.tests.per.project))
  printf("SUM of flaky test cases per session per project %f", sum(n.flaky.tests.per.project, na.rm = TRUE))

  # Flaky percentage per project
  print(summary(results.per.project$flaky.percentage))
  boxplot(results.per.project$flaky.percentage)
  
  sparkline(results.per.project$flaky.percentage, "test-flaky-percentage-per-project-spark")
  
  results.per.project$IDE.x <- merge(results.per.project, projects, by.x = "pid", by.y = "id")$IDE
  
  # Flakiness of tests, i.e. how many execution pairs show a flaky behavior. 
  # This is not the number/percentage of total test executions that were affected by flaky behavior, but probably very similar
  summary(results.per.project$flaky.flakiness.of.tc)
  ggplot(results.per.project, aes(y=flaky.flakiness.of.tc, x=factor(IDE.x))) + geom_boxplot() + xlab("IDE") + ylab("Flakiness of individual test cases") + ggplot.small.defaults
  
  boxplot(results.per.project$flaky.flakiness.of.tc)
  sparkline(results.per.project$flaky.flakiness.of.tc, "test-flaky-percentage-execution-pairs-spark")
  
  
  # Extension: How many test cases of all per project are flaky?
  results <- results[,c("pid","flaky.tcs")]
  
  flaky.tcs.per.pid <- aggregate(results, by=list(results$pid),  FUN=function(x) { list(unique(unlist(x))) })
  f.tcs.per.pid <- data.table(project=character(0),test=character(0))
  project.flaky.tests <- rbindlist(apply(flaky.tcs.per.pid,1,FUN=function(x) {
    df <- data.frame(project=x$pid,test=matrix(unlist(x$flaky.tcs), byrow=T))
    df
  }))
  
  # RESULTS on Flakniess
  printf("RESULT: Flakiness of test cases across all projects: %f", nrow(project.flaky.tests)/nrow(project.tests))
  
  # Falkiness aggregated per project level
  n.testcases.per.project <- plyr::count(project.tests, "project")
  n.flaky.testcases.per.project <- plyr::count(project.flaky.tests, "project")
  n.flaky.testcases.per.project <- plyr::rename(n.flaky.testcases.per.project, c("freq" = "flakyfreq"))
  n.testcases.per.project <- merge(n.testcases.per.project, n.flaky.testcases.per.project)
  n.testcases.per.project$flakyperc <- n.testcases.per.project$flakyfreq/n.testcases.per.project$freq
  print(summary(n.testcases.per.project$flakyperc))
  sparkline(n.testcases.per.project$flakyperc, n.testcases.per.project$flakyperc)
  
  
  # Maybe flakiness is especially high in projects with "abnormally" small number of tests?
  # a standard deviations from mean
  has.really.few.tcs <- round(exp(mean(log(n.testcases.per.project$freq))-sd(log(n.testcases.per.project$freq))))
  has.really.few.tcs
  print(summary(subset(n.testcases.per.project, freq <= has.really.few.tcs)$flakyperc))
  
  
  # permutation tests for users' experience and tests flakiness
  # question: do users with different (e.g., higher) experience write more/less flaky tests?
  results.per.project$userId <- merge(results.per.project, projects, by.x = "pid", by.y = "id")$userId
  results.per.project$userExperience <- merge(results.per.project, users, by.x = "userId", by.y = "uid")$programmingExperience
  significant <- two.way.permutation.test("flaky.percentage","userExperience", as.data.table(results.per.project))
  if (significant) {
    ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
    plot.2way.interaction2("flaky.percentage","userExperience", as.data.table(results.per.project), ordering)
  }
}