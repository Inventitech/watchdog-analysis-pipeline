#
# (c) 2015 - onwards Georgios Gousios <gousiosg@gmail.com>
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#

# Parse all records in the MongoDB database, create a CSV file with intersting 
# fields

library(rmongodb)
library(data.table)
library(plyr)
library(foreach)
library(doMC)

source('src/data-access.R')
source('src/utils.R')
source('src/config-filtering.R')


### 1. Query MongoDB for intervals in the specified timeframe
# Default processing for intervals, if we need more fields
# we define a custom processor for each interval type
processor <- function(interval) {
  switch(interval$it,
         're' = editor.interval.processor(interval),
         'ty' = typing.interval.processor(interval),
         'pe' = perspective.interval.processor(interval),
         'ju' = junit.detail.processor(interval),
         super.processor(interval)
  )
}

# Connect to MongoDB
mongo.conn <- mongo.connection()

printf("[gen-intervals-file]: Querying data from %s to %s", start.ts, stop.ts)
query  <- construct.intervals.query(NULL, NULL, NULL, ide=NULL, start.ts, stop.ts)

count  <- mongo.count(mongo.conn, "watchdog.intervals",
                     mongo.bson.from.JSON(query))
printf("[gen-intervals-file]: %d intervals", count)

cursor <- mongo.find(mongo.conn, "watchdog.intervals",
                     query = mongo.bson.from.JSON(query))

# The following code is there for speed: pre-alloc all required space
# and write to a position specified by the ever icreasing cursor
counter <- 1
results <- vector(mode = "list", length = count)
while (mongo.cursor.next(cursor)) {
  counter <- counter + 1
  if(counter %% 100000 == 0){printf("[gen-intervals-file]: Processed %d intervals", counter)}
  results[[counter]] <- processor(mongo.bson.to.list(mongo.cursor.value(cursor)))
}
mongo.cursor.destroy(cursor)

# Merge all parsed interval lists into a big data.table
intervals <- rbindlist(results, fill = T)
printf("[gen-intervals-file]: Total %d intervals", nrow(intervals))

### Part 2. Deriving columns not in the original dataset
# Aux functions
project.ids <- function(intervals) unique(intervals$pid)
project.sessions <- function(intervals, project.id) sort(unique(subset(intervals, pid == project.id)$ss))
project.ea <- function(intervals, project.id) 
  subset(intervals, pid == project.id & it == 'ea', select = c(ts, te))[order(ts),]

# Check that the provided data.frame does only contain consecutive
# (i.e. non-overlapping) intervals. Must have columns ts and te.
check.non.overlapping <- function(df) {
  df <- with[order(df$ts)]
  df$ts.next <- c(0,tail(df$ts, -1))
  df$check <- df$ts.next > df$te
  !(nrow(subset(df, check == T)) > 0)
}

# Initialize empty session id column
intervals$session <- 0

# Makes foreach not use prescheduling, which is slower, but does not run into long vector return problems with their internal call to mclapply
mcoptions <- list(preschedule=FALSE)

## Alternate solution 2 to produce session ids:
# produce usable session ids - using ss as session id
intervals <- foreach (project.id = project.ids(intervals), .options.multicore=mcoptions) %dopar% {
  printf("[gen-intervals-file]: Project %s", project.id)
  session.id <- 0
  project.intervals <- subset(intervals, pid == project.id)
  for (project.session in project.sessions(project.intervals, project.id)) {
    session.id <- session.id + 1
    affected <- nrow(subset(project.intervals, pid == project.id & ss == project.session))
    project.intervals[(pid == project.id & ss == project.session), session := session.id]
    printf("[gen-intervals-file]: Project: %s, ss: %s, sessions: %d, intervals: %s", 
           project.id, project.session, session.id, affected)
  }
  project.intervals
}
printf("Finished parallel foreach")
intervals.bound <- rbindlist(intervals)
printf("Finished rbind")
stopifnot(nrow(subset(intervals.bound, session == 0)) == 0)

intervals <- intervals.bound
printf("Reassigned intervals")
gc()

# Get project information in a data frame
count  <- mongo.count(mongo.conn, "watchdog.projects",
                      mongo.bson.from.JSON('{}'))
printf("[gen-intervals-file]: %d projects", count)

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

# Merge intervals with projects
intervals <- merge(intervals, projects, by=c('pid'))
intervals <- merge(intervals, users, by=c('uid'))

# Generate duration column, for convenience mostly
intervals$duration <- intervals$te - intervals$ts
stopifnot(nrow(subset(intervals, duration < 0)) == 0)  

### 3. Write to CSV
printf("[gen-intervals-file]: Writing results to intervals.csv")
write.csv(intervals, file = file.path(pRes,'intervals.csv'))
