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


prepare_users <- function(users) {
  # GeoIP for missing countries
  GEOIPed.country <-  apply(users, 1, function(x) {
    if (x[3] == "NA") {
      geo.located <- freegeoip(x[5])
      return(geo.located$country_name)
    } else {
      return(x[3]);
    }
  })
  
  GEOIPed.country <- replace(GEOIPed.country, GEOIPed.country == "Russian Federation", "Russia")
  GEOIPed.country <- replace(GEOIPed.country, GEOIPed.country == "Iran, Islamic Republic of", "Iran")
  
  users$country <- GEOIPed.country
  # Rename column for easier merging later on
  users <- rename(users, replace = c('id' = 'uid'))
  
  return(users)
}


# Connect to MongoDB
mongo.conn <- mongo.connection()

# Get user information in a data frame
count  <- mongo.count(mongo.conn, "watchdog.users")
printf("[gen-users-file]: %d users", count)

cursor <- mongo.find(mongo.conn, "watchdog.users")

counter <- 1
results <- vector(mode = "list", length = count)
while (mongo.cursor.next(cursor)) {
  counter <- counter + 1
  results[[counter]] <- user.processor(mongo.bson.to.list(mongo.cursor.value(cursor)))
}
mongo.cursor.destroy(cursor)
users <- rbindlist(results, fill = T)
printf("[gen-users-file]: Preparing users")
users <- prepare_users(users)

### 3. Write to CSV
printf("[gen-users-file]: Writing results to users.csv")
write.csv(users, file = file.path(pRes,'users.csv'))



