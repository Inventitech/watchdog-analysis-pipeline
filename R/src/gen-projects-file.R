#
# (c) 2015 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#

# Parse all user records in the MongoDB database, create a CSV file with intersting 
# fields

library(rmongodb)
library(data.table)
library(plyr)

source('src/data-access.R')
source('src/utils.R')

# Connect to MongoDB
mongo.conn <- mongo.connection()

# Get project information in a data frame
count  <- mongo.count(mongo.conn, "watchdog.projects")
printf("[gen-projects-file]: %d projects", count)

cursor <- mongo.find(mongo.conn, "watchdog.projects")

counter <- 1
results <- vector(mode = "list", length = count)
while (mongo.cursor.next(cursor)) {
  counter <- counter + 1
  results[[counter]] <- project.processor(mongo.bson.to.list(mongo.cursor.value(cursor)))
}
mongo.cursor.destroy(cursor)
projects <- rbindlist(results, fill = T)

### 3. Write to CSV
printf("[gen-projects-file]: Writing results to projects.csv")
write.csv(projects, file = file.path(pRes,'projects.csv'))



