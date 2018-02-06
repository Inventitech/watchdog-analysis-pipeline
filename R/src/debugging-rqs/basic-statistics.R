#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
# (c) 2016 - onwards Niels Spruit <spruit.niels@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
# 
# Basic statistics about our dataset
library(plyr)

source("src/debugging-header.R")

# number of users, projects, intervals and events
printf("Number of users: %d", nrow(users))
printf("Number of projects: %d", nrow(projects))
printf("Number of intervals (in WD>=2.0.0): %d", nrow(intervals))
printf("Number of events: %d", nrow(events))

# number of users and projects with (debug) intervals in WD>=2.0.0
users.with.intervals <- unique(intervals$uid)
printf("Number of WD>=2.0.0 users with intervals: %d", length(users.with.intervals))
users.with.debugging.intervals <- unique(intervals[it=='db']$uid)
printf("Number of WD>=2.0.0 users with debugging intervals: %d", length(users.with.debugging.intervals))
projects.with.intervals <- unique(intervals$pid)
printf("Number of WD>=2.0.0 projects with intervals: %d", length(projects.with.intervals))

# number of users and projects with events
users.with.events <- unique(events$uid)
printf("Number of users with events: %d", length(users.with.events))
projects.with.events <- unique(events$pid)
printf("Number of projects with events: %d", length(projects.with.events))

# number of users with events AND debug intervals
users.with.debug.intervals.and.events <- intersect(users.with.events,users.with.debugging.intervals)
printf("Number of users with events AND debug intervals: %d", length(users.with.debug.intervals.and.events))

# Filter on users with events
users.with.events <- subset(users, uid %in% users.with.events)

# Average number of events per user that actually has events
avg.nr.of.events.per.user <- nrow(events) / nrow(users.with.events)
printf("Average number of events per user: %.2f", avg.nr.of.events.per.user)

# (Average) number of events per session (using session seed)
printf("Number of sessions with events: %d", length(unique(events$ss)))
events.per.session <- count(events, "ss")
avg.nr.of.events.per.session <- mean(events.per.session$freq)
printf("Average number of events per session: %.2f", avg.nr.of.events.per.session)

# Frequency table of breakpoint types in added breakpoint
added_bp_types_freq_table <- count(events[et == 'ba'], 'bt')

# Frequency table of breakpoint options in changed breakpoints
changed_bp_options_freq_table <- count(events[et == 'bc'], 'ch')

# Frequency table of different debug event types
event_types_freq_table <- count(events, 'et')
