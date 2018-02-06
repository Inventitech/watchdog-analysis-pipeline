#
# (c) 2015 - onwards Georgios Gousios <gousiosg@gmail.com>
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
# (c) 2016 - onwards Niels Spruit <spruit.niels@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#

# Parse all event records in the MongoDB database, create a CSV file with interesting 
# fields

library(rmongodb)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

source('src/data-access.R')
source('src/utils.R')
source('src/config-filtering.R')

### 1. Query MongoDB for events in the specified timeframe
# Default processing for events, if we need more fields
# we define a custom processor for each event type
processor <- function(event) {
  switch(event$et,
         'ba' = breakpoint.event.processor(event),
         'br' = breakpoint.event.processor(event),
         'bc' = breakpoint.change.event.processor(event),
         super.event.processor(event)
  )
}

# Connect to MongoDB
mongo.conn <- mongo.connection()

printf("[gen-events-file]: Querying data from %s to %s", start.ts, stop.ts)
query  <- construct.events.query(NULL, NULL, NULL, ide=NULL, start.ts, stop.ts)

count  <- mongo.count(mongo.conn, "watchdog.events",
                      mongo.bson.from.JSON(query))
printf("[gen-events-file]: %d events", count)

cursor <- mongo.find(mongo.conn, "watchdog.events",
                     query = mongo.bson.from.JSON(query))

# The following code is there for speed: pre-alloc all required space
# and write to a position specified by the ever increasing cursor
counter <- 1
results <- vector(mode = "list", length = count)
while (mongo.cursor.next(cursor)) {
  counter <- counter + 1
  if(counter %% 100000 == 0){printf("[gen-events-file]: Processed %d events", counter)}
  results[[counter]] <- processor(mongo.bson.to.list(mongo.cursor.value(cursor)))
}
mongo.cursor.destroy(cursor)

# Merge all parsed event lists into a big data.table
events <- rbindlist(results, fill = T)
bc_un_events <- subset(events, et=='bc' & ch=='un')
printf("[gen-events-file]: Total %d events, of which %d bp change events with undefined changes", nrow(events), nrow(bc_un_events))

# Remove all breakpoint change events with 'un'(=undefined) as changes 
# as they mostly indicate a breakpoints hit, but not always)
events <- events[!events$ch %in% c('un'),]
printf("[gen-events-file]: Total %d filtered events", nrow(events))

### Part 2. Deriving columns not in the original dataset
# Aux functions
project.ids <- function(events) unique(events$pid)
project.sessions <- function(events, project.id) sort(unique(subset(events, pid == project.id)$ss))

# Initialize empty session id column
events$session <- 0

## Alternate solution 2 to produce session ids:
# produce usable session ids - using ss as session id
events <- foreach (project.id = project.ids(events)) %dopar% {
  printf("[gen-events-file]: Project %s", project.id)
  session.id <- 0
  project.events <- subset(events, pid == project.id)
  for (project.session in project.sessions(project.events, project.id)) {
    session.id <- session.id + 1
    affected <- nrow(subset(project.events, pid == project.id & ss == project.session))
    project.events[(pid == project.id & ss == project.session), session := session.id]
    printf("[gen-events-file]: Project: %s, ss: %s, sessions: %d, events: %s", 
           project.id, project.session, session.id, affected)
  }
  project.events
}
printf("Finished parallel foreach")
events.bound <- rbindlist(events)
printf("Finished rbind")
stopifnot(nrow(subset(events.bound, session == 0)) == 0)

events <- events.bound
printf("Reassigned events")
gc()

# Get project information in a data frame
count  <- mongo.count(mongo.conn, "watchdog.projects",
                      mongo.bson.from.JSON('{}'))
printf("[gen-events-file]: %d projects", count)

cursor <- mongo.find(mongo.conn, "watchdog.projects",
                     query = mongo.bson.from.JSON('{}'))

counter <- 1
results <- vector(mode = "list", length = count)
while (mongo.cursor.next(cursor)) {
  counter <- counter + 1
  results[[counter]] <- project.processor(mongo.bson.to.list(mongo.cursor.value(cursor)))
}
mongo.cursor.destroy(cursor)
projects <- rbindlist(results, fill = T)

NAed.production.percentage <-  apply(projects, 1, function(x) {
  if (is.na(x[3]) &&  is.na(x[4]) || x[3] == "No" && x[4] == "No" ) {
    return(NA);
  } else {
    return(as.integer(x[6]));
  }
})

projects$productionPercentage <- NAed.production.percentage

# Rename column for easier merging later on
projects <- rename(projects, replace = c('id' = 'pid'))

# Get user information in a data frame
users <- data.table(read.csv(file.path(pRes, 'users.csv')))

# Remove redundant userId column in projects
projects$userId <- NULL

# Merge events with projects and users
events <- merge(events, projects, by=c('pid'))
events <- merge(events, users, by=c('uid'))

### 3. Write to CSV
printf("[gen-events-file]: Writing results to events.csv")
write.csv(events, file = file.path(pRes,'events.csv'))