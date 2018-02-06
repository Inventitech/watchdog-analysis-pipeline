debug.output <- FALSE

# TODO (MMB) Default timestamps and versions to examine
# timestamps such that data from FSE paper is included;
# maybe it should be excluded?

start.ts <- 1414800000 * 1000 # FSE start timestamp #1455851200 * 1000 # was: 1398934864
stop.ts <- 1488326400000 # was: 1478995200000  #1460851200 * 1000 # Human time (GMT): Sun, 13 Nov 2016 00:00:00 GMT

users.toremove <- c('26178da52389146b8325325f1ae3cf32497ba0b9','33d85ef2f90c9263350b19e3334bd7bec5bd2282','b55e65dbf07c5236d7178a95310a6a222d0d2016','32db34c710938c8bfb80b698517ecd79a1e7b749','d39c51274af91151695fba56be865ad948dc972d','b8372be100850fb412fad17ddbb0f4ed829269f3')

# all available WD versions db.intervals.distinct("wdv")
versions <- c("1.0-RC",
  "1.0.1",
  "1.0.2",
  "1.1.0",
  "1.1.0.SNAPSHOT",
  "1.1.1",
  "1.1.2",
  "1.2.0",
  "1.2.1",
  "1.3.0",
  "1.4.0",
  "1.4.1",
  "1.4.2",
  #"0.9", # IntelliJ try versions
  #"0.9.1", # IntelliJ try versions
  "1.5.0",
  "1.6.0",
  "1.7.0",
  "2.0.0",
  "2.0.1",
  "2.0.2",
  "KaVE 0.1010-Default",
  "KaVE 0.1011-Default",
  "KaVE 0.1012-Default",
  "KaVE 0.1013-Default",
  "KaVE 0.1014-Default",
  "KaVE 0.1015-Default",
  "KaVE",
  "unknown")

ides <- c("EC", "AS", "IJ", "VS")

# What we define as test files.
# te = test, lt = likelytest, tf = has test in filename, pf = has test in pathname
test.types <- c('lt', 'te', 'tf', 'pf')
test.failures <- c('E', 'F')
experience <- c("> 10 years", "1-2 years", "< 1 year", "3-6 years", "7-10 years", "Unknown")
answers.yes <- c("Yes", "DontKnow")