#
# (c) 2016 - onwards Niels Spruit <spruit.niels@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ2 is: When and how long do developers debug?

library(plyr)

source("src/debugging-header.R")
source("src/utils.R")

### Useful data structures throughout the analysis
debugging_intervals <- intervals[intervals$it == 'db']
junit_intervals <- intervals[intervals$it == 'ju']
debug_perspective_intervals <- intervals[intervals$it == 'pe' & intervals$pet == "de"]
reading_intervals <- intervals[intervals$it == 're']
typing_intervals <- intervals[intervals$it == 'ty']

####################################################################
###     RQ2.1: How much of their active IDE time do developers   ###
###     spend on debugging (compared to other activities)?       ###
####################################################################

# RESULT total time for each interval type (in hours)
total_time_per_interval_type <- aggregate(intervals$duration/1000/60/60, by=list(intervals$it), sum)
total_time_eo <- total_time_per_interval_type[total_time_per_interval_type$Group.1 == "eo",]$x
total_time_per_interval_type$percentage <- (total_time_per_interval_type$x / total_time_eo) * 100

# create data frame with total times per interval type as wel as the overall total IDE open time
total_time_per_user <- aggregate(intervals$duration/1000/60/60, by=list(intervals$uid), sum)
total_time_per_user <- rename(total_time_per_user, c("Group.1"="uid","x"="total_time"))

debugging_time_per_user <- aggregate(debugging_intervals$duration/1000/60/60, by=list(debugging_intervals$uid), sum)
debugging_time_per_user <- rename(debugging_time_per_user, c("Group.1"="uid","x"="debugging_time"))
times_per_user <- merge(total_time_per_user, debugging_time_per_user, all.x = TRUE)

junit_time_per_user <- aggregate(junit_intervals$duration/1000/60/60, by=list(junit_intervals$uid), sum)
junit_time_per_user <- rename(junit_time_per_user, c("Group.1"="uid","x"="junit_time"))
times_per_user <- merge(times_per_user, junit_time_per_user, all.x = TRUE)

debug_perspective_time_per_user <- aggregate(debug_perspective_intervals$duration/1000/60/60, by=list(debug_perspective_intervals$uid), sum)
debug_perspective_time_per_user <- rename(debug_perspective_time_per_user, c("Group.1"="uid","x"="debug_perspective_time"))
times_per_user <- merge(times_per_user, debug_perspective_time_per_user, all.x = TRUE)

reading_time_per_user <- aggregate(reading_intervals$duration/1000/60/60, by=list(reading_intervals$uid), sum)
reading_time_per_user <- rename(reading_time_per_user, c("Group.1"="uid","x"="reading_time"))
times_per_user <- merge(times_per_user, reading_time_per_user, all.x = TRUE)

typing_time_per_user <- aggregate(typing_intervals$duration/1000/60/60, by=list(typing_intervals$uid), sum)
typing_time_per_user <- rename(typing_time_per_user, c("Group.1"="uid","x"="typing_time"))
times_per_user <- merge(times_per_user, typing_time_per_user, all.x = TRUE)

# RESULT summary statistics per column in times_per_user 
# Note: only non-NA rows are taking into account!
summary_debugging_time_per_user <- summary(times_per_user$debugging_time)
summary_junit_time_per_user <- summary(times_per_user$junit_time)
summary_debug_perspective_time_per_user <- summary(times_per_user$debug_perspective_time)
summary_reading_time_per_user <- summary(times_per_user$reading_time)
summary_typing_time_per_user <- summary(times_per_user$typing_time)

# RESULT summary statistics per column in times_per_user with NAs
# Note: NA columns are taking into account by using 0's instead!
summary_debugging_time_per_user_with_NA <- summary(replaceNABy0(times_per_user$debugging_time))
summary_junit_time_per_user_with_NA <- summary(replaceNABy0(times_per_user$junit_time))
summary_debug_perspective_time_per_user_with_NA <- summary(replaceNABy0(times_per_user$debug_perspective_time))
summary_reading_time_per_user_with_NA <- summary(replaceNABy0(times_per_user$reading_time))
summary_typing_time_per_user_with_NA <- summary(replaceNABy0(times_per_user$typing_time))

# add columns with percentages
times_per_user$debugging_percentage <- (times_per_user$debugging_time / times_per_user$total_time) * 100
times_per_user$junit_percentage <- (times_per_user$junit_time / times_per_user$total_time) * 100
times_per_user$debug_perspective_percentage <- (times_per_user$debug_perspective_time / times_per_user$total_time) * 100
times_per_user$reading_percentage <- (times_per_user$reading_time / times_per_user$total_time) * 100
times_per_user$typing_percentage <- (times_per_user$typing_time / times_per_user$total_time) * 100

sparkline(times_per_user[!is.na(times_per_user$debugging_time),]$debugging_time, "rq21_debug.pdf")
sparkline(times_per_user[!is.na(times_per_user$reading_time),]$reading_time, "rq21_reading.pdf")
sparkline(times_per_user[!is.na(times_per_user$typing_time),]$typing_time, "rq21_typing.pdf")
sparkline(times_per_user[!is.na(times_per_user$junit_time),]$junit_time, "rq21_junit.pdf")

# RESULT summary statistics of percentage columns
# Note: again, NA's are not taken into account
summary_debugging_percentage_per_user <- summary(times_per_user$debugging_percentage)
summary_junit_percentage_per_user <- summary(times_per_user$junit_percentage)
summary_debug_perspective_percentage_per_user <- summary(times_per_user$debug_perspective_percentage)
summary_reading_percentage_per_user <- summary(times_per_user$reading_percentage)
summary_typing_percentage_per_user <- summary(times_per_user$typing_percentage)

# RESULT summary statistics of percentage columns with NAs
# Note: NA's are taken into account by replacing them with 0s
summary_debugging_percentage_per_user_with_NA <- summary(replaceNABy0(times_per_user$debugging_percentage))
summary_junit_percentage_per_user_with_NA <- summary(replaceNABy0(times_per_user$junit_percentage))
summary_debug_perspective_percentage_per_user_with_NA <- summary(replaceNABy0(times_per_user$debug_perspective_percentage))
summary_reading_percentage_per_user_with_NA <- summary(replaceNABy0(times_per_user$reading_percentage))
summary_typing_percentage_per_user_with_NA <- summary(replaceNABy0(times_per_user$typing_percentage))

####################################################################
###     RQ2.2: What is the (average) frequency and length        ###
###     of the debugging sessions performed by developers?       ###
####################################################################

# RESULT number of debugging intervals per user
# Note: only considers users with debugging intervals
debugging_intervals_per_user <- count(debugging_intervals, "uid")
summary_debugging_intervals_per_user <- summary(debugging_intervals_per_user$freq)

# RESULT length of debugging intervals (in seconds)
summary_debug_intervals_length <- summary(debugging_intervals$duration / 1000)


####################################################################
###     RQ2.3: At what times do developers launch the            ###
###     IDE debugger?                                            ###
####################################################################

# sort intervals on the start TS
sorted_intervals <- intervals[order(ts)]

# look for intervals right before (within 15 seconds of) a debugging interval and store them in a new column
RIGHT_BEFORE_LIMIT <- 16 * 1000

debug_intervals_with_intervals_right_before <- debugging_intervals
debug_intervals_with_intervals_right_before$intervals_before <- list()

debug_intervals_with_intervals_right_before <- apply(debug_intervals_with_intervals_right_before, 1, function(x) {
  intervals_right_before_x <- subset(sorted_intervals, (x$ts - RIGHT_BEFORE_LIMIT) <= ts & ts < x$ts & ss == x$ss)
  x$intervals_before <- intervals_right_before_x$X.1
  x
})

debug_intervals_with_intervals_right_before <- as.data.frame(do.call(rbind, debug_intervals_with_intervals_right_before))

# RESULT analysis of the types of intervals occuring right before debugging sessions
intervals_before_id_list <- unlist(debug_intervals_with_intervals_right_before$intervals_before)
intervals_before_type_list <- sorted_intervals[X.1 %in% intervals_before_id_list,]$it
intervals_before_freq_table <- count(intervals_before_type_list)
intervals_before_freq_table$percentage <- (intervals_before_freq_table$freq / length(intervals_before_id_list)) * 100

#######################################################################
###     RQ 2.4: Do smelly files (line length) need more debugging?  ###
#######################################################################

#issue description: Correlate with frequency/length of debugging with file size.
#if we find a super-linear relationship, this holds true.

# find reading and typing intervals with a start time that is within the debugging interval
debug_intervals_with_editor_intervals <- debugging_intervals
debug_intervals_with_editor_intervals$intervals_within <- list()

debug_intervals_with_editor_intervals <- apply(debug_intervals_with_editor_intervals, 1, function(x) {
  intervals_within_x <- subset(sorted_intervals, ss == x$ss & (it == 're' | it == 'ty') & x$ts <= ts & ts <= x$te)
  x$intervals_within <- intervals_within_x$X.1
  x
})

debug_intervals_with_editor_intervals <- as.data.table(do.call(rbind, debug_intervals_with_editor_intervals))

# RESULT correlate file size with the number of editor intervals starting within a debugging interval
intervals_within_id_list <- unlist(debug_intervals_with_editor_intervals$intervals_within)
intervals_within_sloc_list <- sorted_intervals[X.1 %in% intervals_within_id_list]$doc.sloc
intervals_within_freq_table <- count(intervals_within_sloc_list)
spearman_all_line_lengths <- cor(intervals_within_freq_table$x, intervals_within_freq_table$freq, method = "spearman")
intervals_within_sloc_list_non_zero <- intervals_within_sloc_list[intervals_within_sloc_list != 0]
intervals_within_non_zero_freq_table <- count(intervals_within_sloc_list_non_zero)
spearman_non_zero_line_lengths <- cor(intervals_within_non_zero_freq_table$x, intervals_within_non_zero_freq_table$freq, method = "spearman")

# RESULT correlate file size of the editor intervals starting within a debugging interval with duration of debugging interval
sloc_durations <- data.table(sloc=numeric(), duration=numeric())
for (row in 1:nrow(debug_intervals_with_editor_intervals)) {
  int_within <- debug_intervals_with_editor_intervals[row,]$intervals_within[[1]]
  for (int in int_within) {
    new <- data.table(sloc=sorted_intervals[X.1 == int,]$doc.sloc,
                      duration=as.numeric(debug_intervals_with_editor_intervals[row,]$duration))
    sloc_durations <- rbind(sloc_durations,new,use.names = TRUE, fill = FALSE)
  }
}
spearman_sloc_duration_all_values <- cor(sloc_durations$sloc, sloc_durations$duration, method = 'spearman')
sloc_durations_non_zero <- sloc_durations[sloc != 0,]
spearman_sloc_duration_non_zero_values <- cor(sloc_durations_non_zero$sloc, sloc_durations_non_zero$duration, method = 'spearman')

################################################################################
###     RQ 2.5: Are 20% of classes responsible for 80% of debugging effort?  ###
################################################################################

# For this RQ, compare the number of debugged classes in a debug interval:
#   (1) to the number of classes we ever got for this project; and
#   (2) to the number of classes that were ever opened during a debug interval per project.

# add set of all classes, all production classes and all test classes seen and all classed ever debugged per project
editor_intervals <- intervals[intervals$it == 're' | intervals$it == 'ty']
unique_projects <- data.table(unique(editor_intervals$pid))
unique_projects$all_class_names <- list()
unique_projects <- apply(unique_projects, 1, function(x) {
  # all classes seen
  all_classes <- as.vector(unique(editor_intervals[pid == x$V1]$doc.fn))
  x$all_class_names <- all_classes
  x$total_nr_of_classes <- as.integer(length(all_classes))
  
  # all production classes seen
  all_prod_classes <- as.vector(unique(editor_intervals[pid == x$V1 & doc.dt == 'pr']$doc.fn))
  x$all_prod_class_names <- all_prod_classes
  x$total_nr_of_prod_classes <- as.integer(length(all_prod_classes))
  
  # all testing classes seen
  all_test_classes <- as.vector(unique(editor_intervals[pid == x$V1 & !(doc.dt %in% c('pr','un'))]$doc.fn))
  x$all_test_class_names <- all_test_classes
  x$total_nr_of_test_classes <- as.integer(length(all_test_classes))
  
  # all debugged classes
  all_debug_intervals_for_project <- debug_intervals_with_editor_intervals[pid == x$V1]
  all_intervals_within <- unique(unlist(all_debug_intervals_for_project$intervals_within))
  x$all_debugged_classes <- unique(editor_intervals[X.1 %in% all_intervals_within]$doc.fn)
  x$total_nr_of_classes_debugged <- as.integer(length(x$all_debugged_classes))
  x
})
unique_projects <- as.data.table(do.call(rbind, unique_projects))

# add number of classes that have been debugged to each debug interval and all classes seen in debugged project
debug_intervals_with_editor_intervals <- apply(debug_intervals_with_editor_intervals, 1, function(x) {
  intervals_within <- x$intervals_within
  unique_classes <- unique(editor_intervals[X.1 %in% intervals_within]$doc.fn)
  x$nr_of_debugged_classes <- length(unique_classes)
  x$nr_of_debugged_prod_classes <- length(unique(editor_intervals[X.1 %in% intervals_within & doc.dt == 'pr']$doc.fn))
  x$nr_of_debugged_test_classes <- length(unique(editor_intervals[X.1 %in% intervals_within & !(doc.dt %in% c('pr','un'))]$doc.fn))
  x$total_project_classes <- as.integer(unique_projects[V1 == x$pid]$total_nr_of_classes)
  x$total_project_prod_classes <- as.integer(unique_projects[V1 == x$pid]$total_nr_of_prod_classes)
  x$total_project_test_classes <- as.integer(unique_projects[V1 == x$pid]$total_nr_of_test_classes)
  if (nrow(unique_projects[V1 == x$pid]) == 0) {
    x$total_project_classes <- 0
    x$total_project_prod_classes <- 0
    x$total_project_test_classes <- 0
  }
  x$perc_debugged_of_total <- (x$nr_of_debugged_classes/x$total_project_classes) * 100
  x$perc_debugged_of_total_prod <- (x$nr_of_debugged_prod_classes/x$total_project_prod_classes) * 100
  x$perc_debugged_of_total_test <- (x$nr_of_debugged_test_classes/x$total_project_test_classes) * 100
  
  # add total debugged classes and percentage of currently debugged of all debugged
  x$total_debugged_classes <- unlist(unique_projects[V1 == x$pid]$total_nr_of_classes_debugged)
  if (nrow(unique_projects[V1 == x$pid]) == 0) {
    x$total_debugged_classes <- 0
  }
  x$perc_debugged_of_all_debugged <- (x$nr_of_debugged_classes/x$total_debugged_classes) * 100
  x
})
debug_intervals_with_editor_intervals <- as.data.table(do.call(rbind, debug_intervals_with_editor_intervals))
  
# RESULT statistical analysis for (1) (split up in all classes, all production classes and all test classes)
summary_perc_debugged_of_total_all <- 
  summary(as.numeric(debug_intervals_with_editor_intervals$perc_debugged_of_total))
summary_perc_debugged_of_total_non_zero <- 
  summary(as.numeric(debug_intervals_with_editor_intervals[nr_of_debugged_classes>0]$perc_debugged_of_total))

summary_perc_debugged_of_total_prod_all <- 
  summary(as.numeric(debug_intervals_with_editor_intervals$perc_debugged_of_total_prod))
summary_perc_debugged_of_total_prod_non_zero <- 
  summary(as.numeric(debug_intervals_with_editor_intervals[nr_of_debugged_prod_classes>0]$perc_debugged_of_total_prod))

summary_perc_debugged_of_total_test_all <- 
  summary(as.numeric(debug_intervals_with_editor_intervals$perc_debugged_of_total_test))
summary_perc_debugged_of_total_test_non_zero <- 
  summary(as.numeric(debug_intervals_with_editor_intervals[nr_of_debugged_test_classes>0]$perc_debugged_of_total_test))

# RESULT statistical analysis for (2)
summary_perc_debugged_of_all_debugged_all <- 
  summary(as.numeric(debug_intervals_with_editor_intervals$perc_debugged_of_all_debugged))
summary_perc_debugged_of_all_debugged_non_zero <- 
  summary(as.numeric(debug_intervals_with_editor_intervals[nr_of_debugged_classes>0]$perc_debugged_of_all_debugged))

#########################################################################
###     RQ 2.6: Do developers who test a lot have to debug less?      ###
#########################################################################

# RESULT spearman analysis using results from RQ2.1 
# make selection of rows (so ignore rows with 0 hrs of debugging and/or testing time)
at_most_one_NA_data <- times_per_user[!is.na(times_per_user$debugging_time) | !is.na(times_per_user$junit_time),]
at_most_one_NA_data <- replaceNABy0(at_most_one_NA_data)
at_most_one_NA_cor <- cor(at_most_one_NA_data$junit_time, at_most_one_NA_data$debugging_time, method = 'spearman')

both_non_NA_data <- times_per_user[!is.na(times_per_user$debugging_time) & !is.na(times_per_user$junit_time),]
both_non_NA_cor <- cor(both_non_NA_data$junit_time, both_non_NA_data$debugging_time, method = 'spearman')

# Add column with time spend spend in test classes
testing_time_per_user <- aggregate(editor_intervals[!(doc.dt %in% c('pr','un'))]$duration/1000/60/60, by=list(editor_intervals[!(doc.dt %in% c('pr','un'))]$uid), sum)
testing_time_per_user <- rename(testing_time_per_user, c("Group.1"="uid","x"="testing_time"))
times_per_user <- merge(times_per_user, testing_time_per_user, all.x = TRUE)

# RESULT spearman analysis when looking at time spend in test classes instead of running tests
# make selection of rows (so ignore rows with 0 hrs of debugging and/or testing time)
users_with_debug_or_testing_time <- times_per_user[!is.na(times_per_user$debugging_time) | !is.na(times_per_user$testing_time),]
users_with_debug_or_testing_time <- replaceNABy0(users_with_debug_or_testing_time)
users_with_debug_or_testing_time_cor <- cor(users_with_debug_or_testing_time$testing_time, users_with_debug_or_testing_time$debugging_time, method = 'spearman')

users_with_debug_and_testing_time <- times_per_user[!is.na(times_per_user$debugging_time) & !is.na(times_per_user$testing_time),]
users_with_debug_and_testing_time_cor <- cor(users_with_debug_and_testing_time$testing_time, users_with_debug_and_testing_time$debugging_time, method = 'spearman')

#########################################################################
###     RQ 2.7: Do experienced developers have to debug less?         ###
#########################################################################

# 1. add experience of users to times_per_user and remove users without experience or debug intervals
times_per_user_with_experience <- apply(times_per_user, 1, function(x) {
  experience <- users[uid==x['uid'],]$programmingExperience
  x$experience <- as.character(experience)
  x
})
times_per_user_with_experience <- as.data.table(do.call(rbind, times_per_user_with_experience))
times_per_user_with_experience <- times_per_user_with_experience[!is.na(experience) & !is.na(debugging_time)]

# 2. replace experience with numerical values
times_per_user_with_experience <- subset(times_per_user_with_experience, select = c('debugging_time','experience'))
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(x) gsub("< 1 year", 1, x)))
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(x) gsub("1-2 years", 2, x)))
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(x) gsub("3-6 years", 3, x)))
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(x) gsub("7-10 years", 4, x)))
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(x) gsub("> 10 years", 5, x)))

# convert factors back to numeric
times_per_user_with_experience <- data.frame(lapply(times_per_user_with_experience, function(f) as.numeric(levels(f))[as.integer(f)])) 

# RESULT: 3. calculate spearman correlation experience~debugging time
experience_debugging_time_cor <- cor(times_per_user_with_experience$experience, times_per_user_with_experience$debugging_time, method = 'spearman')
