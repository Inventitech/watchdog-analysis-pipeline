#
# (c) 2015 - onwards Georgios Gousios <gousiosg@gmail.com>
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ1 is concerned with statistics and correlations about test executions.

## Basic statistics for ju intervals
ju.intervals <- subset(intervals, it == 'ju')

printf("Projects total: %d", length(unique(intervals$pid)))
printf("Users total: %d", length(unique(intervals$uid)))
printf("Projects with test executions: %d", length(unique(ju.intervals$pid)))
printf("Sessions with test executions: %d", length(unique(ju.intervals$ss)))
printf("Test executions: %d", nrow(ju.intervals))
# calculate absoulte numbers for testing and production code.
## Split and setup intervals
projects.with.tests <- unique(subset(intervals, it == 're' & doc.dt %in% test.types)$pid)
projects.with.ju.tests <- unique(subset(intervals, it == 're' & doc.dt == "te")$pid)
intervals.with.tests <- subset(intervals, pid %in% projects.with.tests)
printf("projects with tests: %d", length(projects.with.tests))
printf("projects with exectuable tests: %d", length(projects.with.ju.tests))

## RESULT: number of test runs per session
sessions.per.test.project <- count(intervals.with.tests, c('session', 'pid'))
testruns.per.session <- count(ju.intervals, c('session', 'pid'))
tests.per.session.in.test.project <- merge(sessions.per.test.project, testruns.per.session, by=c('session','pid'), all.x=T)
tests.per.session.in.test.project$freq.x <- NULL
tests.per.session.in.test.project$freq.y <- replaceNABy0(tests.per.session.in.test.project$freq.y)
printf("#Average test runs per session in a porject in which a test was run: %f", mean(tests.per.session.in.test.project$freq.y))

summary(testruns.per.session)
printf("Number of sessions in which pot. a test coudl be run: %d", nrow(tests.per.session.in.test.project))
printf("Number of sessions with >= 1 test run: %d", nrow(testruns.per.session))
printf("#Average test runs per session in which a test was run: %f", mean(testruns.per.session$freq))

barplot(sort(tests.per.session.in.test.project$freq.y, decreasing=T))
# Interpretation: Developers largely do not run tests, but when they do, they do it heftily
###########################################


sessions.with.tests <- unique(subset(ju.intervals, select=c('pid','session')))

session.churn <- apply(sessions.with.tests, 1, function(x) {
  #x fields: 1 -> pid, 2 -> session, 3 -> num.ju.executions
  session.intervals <- subset(intervals, pid == as.character(x[1]) & session == as.integer(x[2]), 
                              select = c(it, doc.dt, diff, uid, IDE.x))
  
  userId <- session.intervals$uid[1]
  userId <- as.character(userId)
  userExperience <- users[which(users$uid == userId), ]$programmingExperience[1]
  userExperience <- as.character(userExperience)
  
  list(
    'pid'        = x[1],
    'test.churn' = sum(subset(session.intervals,  it == 'ty' & doc.dt %in% test.types)$diff),
    'prod.churn' = sum(subset(session.intervals,  it == 'ty' & doc.dt == 'pr')$diff),
    'prod.edits' = nrow(subset(session.intervals, it == 'ty' & doc.dt == 'pr')),
    'test.edits' = nrow(subset(session.intervals, it == 'ty' & doc.dt %in% test.types)),
    'test.runs'  = nrow(subset(session.intervals, it == 'ju')),
    'userExperience'= userExperience,
    'IDE' = session.intervals$IDE.x[1]
  )
})
session.churn <- rbindlist(session.churn)

## TRIVIAL RESULT: number of analysed session with test executions
printf("Number of analysed sessions with test executions: %d", nrow(session.churn))

## TRIVIAL RESULT: Are test churn and test edits correlated (we expect this)
# Shapiro test for normal distribution
pvalue.test.edits <- shapiro.test(session.churn$test.edits)$p.value
pvalue.test.churn <- shapiro.test(session.churn$test.churn)$p.value
pvalue.prod.churn <- shapiro.test(session.churn$prod.churn)$p.value
pvalue.prod.edits <- shapiro.test(session.churn$prod.edits)$p.value
pvalue.test.runs  <- shapiro.test(session.churn$test.runs)$p.value
if (pvalue.test.edits<=0.05 | pvalue.test.churn<=0.05 | pvalue.prod.churn<=0.05 
    | pvalue.prod.edits<=0.05 | pvalue.test.runs<=0.05){
  printf("Results of the Shapiro test: p-value <= %s", max(pvalue.test.edits, pvalue.test.churn, pvalue.prod.churn, pvalue.prod.edits, pvalue.test.runs))
  print("The data are non-normal distributed. We used the Spearman rank correlation.")
  cor.method <- "spearman"
} else {
  printf("Results of the Shapiro test: p-value > %s", min(pvalue.test.edits, pvalue.test.churn, pvalue.prod.churn, pvalue.prod.edits, pvalue.test.runs))
  print("The data are normally distributed. We used the Spearman rank correlation.")
  cor.method <- "pearson"
}
interpret.spearman('Number of test edits', 'Test churn',
                   cor.test(session.churn$test.edits, session.churn$test.churn, method=cor.method),
                   'RQ1 - Does test churn correlate with test code edits?')
interpret.spearman('Number of production code edits', 'Production code churn',
                   cor.test(session.churn$prod.edits, session.churn$prod.churn, method=cor.method),
                   'RQ1 - Does production churn correlate with code edits?')

## RESULT: The more you edit your tests, the more often you run them?
interpret.spearman('Test churn', 'Test runs',
                   cor.test(session.churn$test.churn, session.churn$test.runs,  method=cor.method),
                   'RQ1 - The more you change test code the more you run the tests?')
interpret.spearman('Test edits', 'Test runs',
                   cor.test(session.churn$test.edits, session.churn$test.runs,  method=cor.method),
                   'RQ1 - The more you edit tests the more you run them?')

## RESULT: The more you edit your production code, the more often you run your tests?
interpret.spearman('Production Churn', 'Test runs',
                   cor.test(session.churn$prod.churn, session.churn$test.runs,  method=cor.method),
                   'RQ1 - The more you change production code the more you run the tests?')
interpret.spearman('Production Churn', 'Test runs',
                   cor.test(session.churn$prod.edits, session.churn$test.runs,  method=cor.method),
                   'RQ1 - The more you edit production code the more you run the tests?')

## RESULT: The more you edit your production code, the more you edit your tests?
interpret.spearman('Production Churn', 'Test churn',
                   cor.test(session.churn$test.churn, session.churn$prod.churn, method=cor.method),
                   'RQ1 - The more you edit production code the more you edit the tests?')

interpret.spearman('Production edits', 'Test edits',
                   cor.test(session.churn$test.edits, session.churn$prod.edits, method=cor.method),
                   'RQ1 - The more you edit production code the more you edit the tests?')

## TRIVIAL RESULT: Per project, the more you edit your production code, the more you edit your tests?
# We only have 5 projects where the p value is < 0.05, but correlation is very strong in all cases
per.project.test.churn.prod.churn <- 
  Reduce(function(acc ,x){
    project.sessions <- subset(session.churn, pid == x & !is.na(test.churn) & !is.na(prod.churn))
    if(nrow(project.sessions) < 3){
      return(acc)
    }
    
    result <- cor.test(project.sessions$test.churn, project.sessions$prod.churn, method=cor.method)
    rbind(acc, data.frame(pid = x, num = nrow(project.sessions), result = result$estimate, p.value = result$p.value))
  }, unique(session.churn$pid), data.frame())

subset(per.project.test.churn.prod.churn, p.value < 0.05)

print("#### INTERACTION ANALYSIS for RQ1####")
# permutation tests 
significant <- three.way.permutation.test("test.edits", "test.runs", "userExperience", session.churn)
if (significant) {
  ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
  plot.3way.interaction2("test.edits", "test.runs", "userExperience", session.churn, ordering, x.label="#Test Runs", y.label="#Test Edits")
}

significant <- three.way.permutation.test("prod.edits", "test.runs", "userExperience", session.churn)
if (significant) {
  ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
  plot.3way.interaction2("prod.edits", "test.runs", "userExperience", session.churn, ordering, x.label="#Tests Run", y.label="#Production Edits")
}

significant <- three.way.permutation.test("prod.edits", "test.edits", "userExperience", session.churn)
if (significant) {
  ordering <- c("< 1 year","1-2 years","3-6 years","7-10 years","> 10 years")
  plot.3way.interaction2("prod.edits", "test.edits", "userExperience", session.churn, ordering, x.label="#Test Edits", y.label="#Production Edits")
}

session.churn[session.churn$IDE=="ec"]$IDE = "Eclipse"
session.churn[session.churn$IDE=="ij"]$IDE = "IntelliJ"

significant <- three.way.permutation.test("test.edits", "test.runs", "IDE", session.churn)
if (significant)
  plot.3way.interaction("test.edits", "test.runs", "IDE", session.churn, x.label="#Test Runs", y.label="#Test Edits")

significant <- three.way.permutation.test("prod.edits", "test.runs", "IDE", session.churn)
if (significant)
  plot.3way.interaction("prod.edits", "test.runs", "IDE", session.churn, x.label="#Tests Run", y.label="#Production Edits")

significant <- three.way.permutation.test("prod.edits", "test.edits", "IDE", session.churn)
if (significant)
  plot.3way.interaction("prod.edits", "test.edits", "IDE", session.churn, x.label="#Test Edits", y.label="#Production Edits")

