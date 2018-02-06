#
# (c) 2016 - onwards Niels Spruit <spruit.niels@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
# RQ3 is: What is typical behaviour during debugging?

library(plyr)

source("src/debugging-header.R")

### Useful data structures throughout the analysis
debugging_intervals <- intervals[intervals$it == 'db']

### Debugging intervals including list with event IDs occuring WITHIN debugging intervals to the data
debugging_intervals_within <- debugging_intervals
debugging_intervals_within$events_within <- list()
debugging_intervals_within <- apply(debugging_intervals_within, 1, function(x) {
  events_within_x <- subset(events, ss == x$ss & x$ts <= ts & ts <= x$te)
  x$nr_events_within <- nrow(events_within_x)
  x$events_within <- events_within_x$X.1
  x
})
debugging_intervals_within <- as.data.table(do.call(rbind, debugging_intervals_within))

### Debugging intervals including list with event IDs occuring BETWEEN debugging intervals to the data
debugging_intervals_between <- debugging_intervals
debugging_intervals_between$events_after <- list()
debugging_intervals_between <- apply(debugging_intervals_between, 1, function(x) {
  # add ID of the debugging interval right after x (if they are within the same session)
  interval_after <- head(subset(debugging_intervals_between, ss == x$ss & ts > x$te), n = 1)
  x$interval_after <- interval_after[1]$X.1
  
  # add IDs of events occuring after x, but before interval_after
  events_after_x <- subset(events, ss == x$ss & x$te < ts & ts < interval_after$ts)
  x$nr_events_after <- nrow(events_after_x)
  x$events_after <- events_after_x$X.1
  x
})
debugging_intervals_between <- as.data.table(do.call(rbind, debugging_intervals_between))

####################################################################
###     RQ3.1: Which sequences of events are commonly found      ###
###     within and between debugging sessions?                   ###
####################################################################

##############################################################
# analysis of sequences of events WITHIN debugging intervals #
##############################################################
debugging_intervals_within_seq <- debugging_intervals_within
debugging_intervals_within_seq$event_sequence <- list()
debugging_intervals_within_seq <- apply(debugging_intervals_within_seq, 1, function(x) {
  x$event_sequence <- as.character(events[X.1 %in% x$events_within]$et)
  x
})
debugging_intervals_within_seq <- as.data.table(do.call(rbind, debugging_intervals_within_seq))

# RESULT: create frequency table of sequences of events
# NOTE: the lists are converted to a string to make the count function work
event_sequences <- debugging_intervals_within_seq$event_sequence
complete_sequences_freq_table <- count(paste(event_sequences))

# RESULT: Compress and analyze event sequences by collapsing equal consecutive events into a single event.
# This will reduce the number of sequences that occur only once.
compressSequence <- function(event_sequence, compressStepping = F) {
  compressed_sequence <- c()
  last_event_type <- "NULL"
  stepping_events <- c('so','si','st')
  for (event in event_sequence) {
    if (compressStepping & (event %in% stepping_events)) {
      event <- 'step'
    }
    if (event != last_event_type) {
      compressed_sequence <- append(compressed_sequence, event)
      last_event_type <- event
    }
  }
  compressed_sequence
}

compressed_sequences <- lapply(event_sequences, compressSequence)
compressed_sequences_freq_table <- count(paste(compressed_sequences))

# RESULT: compress even further by collapsing all stepping events (i.e. all 'st','so(ver)' and 'si' events)
compressed_sequences_with_step <- lapply(event_sequences, function(x) compressSequence(x, compressStepping = T))
compressed_sequences_with_step_freq_table <- count(paste(compressed_sequences_with_step))

###############################################################
# analysis of sequences of events BETWEEN debugging intervals #
###############################################################
debugging_intervals_between_seq <- debugging_intervals_between
debugging_intervals_between_seq$event_sequence <- list()
debugging_intervals_between_seq <- apply(debugging_intervals_between_seq, 1, function(x) {
  x$event_sequence <- as.character(events[X.1 %in% x$events_after]$et)
  x
})
debugging_intervals_between_seq <- as.data.table(do.call(rbind, debugging_intervals_between_seq))

# RESULT: create frequency table of sequences of events occuring between debugging intervals
# NOTE: the lists are converted to a string to make the count function work
event_sequences_between <- debugging_intervals_between_seq$event_sequence
complete_sequences_between_freq_table <- count(paste(event_sequences_between))

# RESULT: compress and analyze between event sequences by collapsing equal consecutive events into a single event.
compressed_sequences_between <- lapply(event_sequences_between, compressSequence)
compressed_sequences_between_freq_table <- count(paste(compressed_sequences_between))

#########################################################################
###     RQ 3.2: When/how are breakpoints set and how do they evolve   ###
###     over time?                                                    ###
#########################################################################

# create sequences of breakpoint events per session with debugging intervals
wd_sessions_with_debug_intervals <- data.table(unique(debugging_intervals$ss))
breakpoint_events <- events[et %in% c('ba','br','bc')]
wd_sessions_with_debug_intervals$breakpoint_sequence <- list()
wd_sessions_with_debug_intervals$breakpoint_changes <- list()
wd_sessions_with_debug_intervals <- apply(wd_sessions_with_debug_intervals, 1, function(x) {
  x$breakpoint_sequence <- as.character(breakpoint_events[ss == x['V1']]$et)
  x$breakpoint_changes <- as.character(breakpoint_events[ss == x['V1'] & et == 'bc']$ch)
  x
})
wd_sessions_with_debug_intervals <- as.data.table(do.call(rbind, wd_sessions_with_debug_intervals))

# RESULT: analyze frequency of sequences
breakpoint_sequences_freq_table <- count(paste(wd_sessions_with_debug_intervals$breakpoint_sequence))

# RESULT: compress sequences and analyze again
compressed_breakpoint_sequences <- lapply(wd_sessions_with_debug_intervals$breakpoint_sequence, compressSequence)
compressed_breakpoint_sequences_freq_table <- count(paste(compressed_breakpoint_sequences))

# RESULT: analyze sequences of breakpoint changes
breakpoint_changes_sequences_freq_table <- count(paste(wd_sessions_with_debug_intervals$breakpoint_changes))
compressed_breakpoint_changes_sequences <- lapply(wd_sessions_with_debug_intervals$breakpoint_changes, compressSequence)
compressed_breakpoint_changes_sequences_freq_table <- count(paste(compressed_breakpoint_changes_sequences))

#########################################################################
###     RQ3.3: How often does it occur that a developer steps         ###
###     over the point of interest and has to start all over again?   ###
#########################################################################

# add column with last event in sequence to debugging_intervals_within_seq
debugging_intervals_within_seq <- apply(debugging_intervals_within_seq, 1, function(x) {
  x$last_event <- last(x$event_sequence)
  x
})
debugging_intervals_within_seq <- as.data.table(do.call(rbind, debugging_intervals_within_seq))

# if last event == 'step', check if interval_after != NA (in between list)
debugging_interval_ending_with_step <- debugging_intervals_within_seq[last_event %in% c('st','si','so')]
debugging_interval_ending_with_step <- apply(debugging_interval_ending_with_step, 1, function(x) {
  x$interval_after <- as.numeric(debugging_intervals_between[X.1 == x$X.1]$interval_after)
  x
})
debugging_interval_ending_with_step <- as.data.table(do.call(rbind, debugging_interval_ending_with_step))

# for remaining intervals: check time between the two debugging intervals
debugging_interval_ending_with_step <- debugging_interval_ending_with_step[!is.na(interval_after)]

# calculate possible cases for multiple time limits
calculate_cases <- function(TIME_BETWEEN_LIMIT) {
  result <- apply(debugging_interval_ending_with_step, 1, function(x) {
    x$interval_right_after <- debugging_intervals[X.1 == x$interval_after]$ts <= (x$te+TIME_BETWEEN_LIMIT)
    x
  })
  result <- as.data.table(do.call(rbind, result))
  nrow(result[interval_right_after == TRUE,])
}

cases_stepping_over_per_time_between <- data.frame(time=numeric(),possible_cases=numeric())
time_limits <- seq(1,900, by=5)
for(time_limit in time_limits) {
  nr_cases <- calculate_cases(time_limit*1000)
  new_row <- data.frame(time=time_limit, possible_cases=nr_cases)
  cases_stepping_over_per_time_between <- rbind(cases_stepping_over_per_time_between, new_row)
}

# RESULT: plot of possible cases depending on time between limit
ggplot(cases_stepping_over_per_time_between, aes(time, possible_cases)) + geom_point() + geom_smooth() +
  labs(x="Maximum time between debug intervals (in seconds)", y="Number of possible cases")

