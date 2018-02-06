#
# (c) 2014 - onwards Georgios Gousios <gousiosg@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#

# Functions to connect to, query the interval database and de-serialize intervals
library(rmongodb)
library(data.table)

source('src/utils.R')
try(source('config.R'))

# Connect to mongo db
mongo.connection <- function() {
  if (exists('mongo.user') && !is.empty(mongo.user)) {
    mongo.create(host = mongo.host, username = mongo.user,
                 password = mongo.passwd, db = "watchdog")
  } else {
    mongo.create(db = "watchdog")
  }
}

# Generate a valid MongoDB interval query based on the provided arguments
construct.intervals.query <- function(project.id = NULL, int.type = NULL, 
                                      version = NULL, ide = NULL, from.ts = -1,
                                      to.ts = -1) {
  project.arg <-
    if (!is.null(project.id)) { paste("\"projectId\" : \"", project.id, "\"", sep = '')}

  int.arg <-
    if (!is.null(int.type)) { paste("\"it\" : \"", int.type, "\"", sep = '')}

  version <-
    if (!is.null(version)) { paste("\"wdv\" : \"", version, "\"", sep = '')}
  
  ide <-
    if (!is.null(ide)) { paste("\"ide\" : \"", ide, "\"", sep = '')}

  from.ts <-
    if (from.ts > -1) { paste("\"ts\" : { \"$gte\" : ", from.ts, "}", sep = '')}
  
  to.ts <-
    if (to.ts > -1) { paste("\"te\" : { \"$lte\" : ", to.ts, "}", sep = '')}

  q <- paste(c(project.arg, int.arg, version, ide, from.ts, to.ts), 
             sep = '', collapse = ', ')
  paste('{', q, '}')
}

# Return a data frame with all intervals that match the provided project.id
# and interval types
get.intervals <- function(project.id = NULL,
                          int.types = c(),
                          processor = NULL, 
                          mongo.conn = NULL) {

  results <- if(length(int.types) == 0) {
      get.intervals.for.type(project.id, NULL, processor, mongo.conn)
    } else {
      Reduce(function(acc, x) {
        rbind(acc, get.intervals.for.type(project.id, x, processor, mongo.conn))
      }, int.types, data.table())
    }

  unique(results, by=c('it', 'ts', 'te', 'ss', 'pid'))
}

get.intervals.for.type <- function(project.id, int.type, processor = NULL, 
                                   mongo.conn = NULL) {
  count <- mongo.count(mongo.conn, "watchdog.intervals",
                       mongo.bson.from.JSON(construct.intervals.query(project.id, int.type)))
  cursor <- mongo.find(mongo.conn, "watchdog.intervals",
                       query = mongo.bson.from.JSON(construct.intervals.query(project.id, int.type)))

  # Read and process
  counter <- 1
  results <- vector(mode = "list", length = count)
  while (mongo.cursor.next(cursor)) {
    counter <- counter + 1
    proc <- if(is.null(processor)){ determine.processor(int.type) } else { processor }
    results[[counter]] <- proc(mongo.bson.to.list(mongo.cursor.value(cursor)))
  }
  mongo.cursor.destroy(cursor)

  #Fast merging of a list of data frames
  na.omit(rbindlist(results))
}

# Generate a valid MongoDB event query based on the provided arguments
construct.events.query <- function(project.id = NULL, event.type = NULL, 
                                      version = NULL, ide = NULL, from.ts = -1,
                                      to.ts = -1) {
  project.arg <-
    if (!is.null(project.id)) { paste("\"projectId\" : \"", project.id, "\"", sep = '')}
  
  event.arg <-
    if (!is.null(event.type)) { paste("\"et\" : \"", event.type, "\"", sep = '')}
  
  version <-
    if (!is.null(version)) { paste("\"wdv\" : \"", version, "\"", sep = '')}
  
  ide <-
    if (!is.null(ide)) { paste("\"ide\" : \"", ide, "\"", sep = '')}
  
  ts <-
    if (from.ts > -1) { paste("\"ts\" : { \"$gte\" : ", from.ts, ", \"$lte\" : ", to.ts, "}", sep = '')}
  
  q <- paste(c(project.arg, event.arg, version, ide, ts), 
             sep = '', collapse = ', ')
  paste('{', q, '}')
}

determine.processor <- function(int.type) {
  if (is.null(int.type)){
    int.type <- '0xdeadbabe'
  }
  
  switch(EXPR = int.type,
         're' = editor.interval.processor,
         'ty' = editor.interval.processor,
         'pe' = perspective.interval.processor,
         'ju' = junit.detail.processor,
          super.processor)
}

# Converts a base interval to a data frame
super.processor <- function(interval) {
  null.to.NA(list(
    it = interval$it,
    wdv = interval$wdv,
    ts = interval$ts,
    te = interval$te,
    ss = interval$ss,
    uid = as.character(interval$userId),
    pid = as.character(interval$projectId),
    ide = interval$ide
  ))
}

# Converts an editor-based interval to a data frame
editor.interval.processor <- function(interval) {
  base.int <- super.processor(interval)

  doc.int <- tryCatch({
    list(doc.dt = interval$doc$dt,
         doc.fn = interval$doc$fn,
         doc.sloc = interval$doc$sloc)
    }, error = function(e) {
      list(doc.dt = NA,
           doc.fn = NA,
           doc.sloc = NA)
    })

  null.to.NA(c(base.int, doc.int))
}

typing.interval.processor <- function(interval) {
  base.int <- editor.interval.processor(interval)

  diff <- tryCatch({
    if(!is.null(interval$diff)) {
      list(diff = interval$diff)
    } else {
      list(diff = NA)
    }
  }, error = function(e) {
    list(diff = NA)
  })

  null.to.NA(c(base.int, diff))
}

# Converts a prespective interval to a data frame
perspective.interval.processor <- function(interval) {
  base.int <- super.processor(interval)
  c(base.int, list("pet" = interval$pet))
}

# Converts a junit interval to a data frame
junit.interval.processor <- function(interval) {
  base.int <- super.processor(interval)

  junit.int <- tryCatch({
    list(
      ju.result = interval$je$r,
      ju.duration = replaceNullBy0(interval$je$d),
    )}, error = function(e) {
      list(
        ju.result = NA,
        ju.duration = 0
      )
    })

  null.to.NA(c(base.int, junit.int))
}

flatten.tests <- function(x) {
  #"_id" : ObjectId("5437d9a34681247329000a14")
  #the.interval$je$c[[1]]$c[[1]]$c[[1]]$c[[1]]$c[[1]]
  tests <- c()

  for(i in x$c) {
    children.tests <- c()
    if(!is.null(i$c)) {
      children.tests <- flatten.tests(i)
    }
    tests <- c(tests, children.tests)
  }

  if(!is.null(x$t)) {
    tests <- c(tests, paste(c(x$t, x$r), collapse = "@"))
  }

  tests
}

junit.detail.processor <- function(interval) {
  base.int <- super.processor(interval)
  tests <- flatten.tests(interval$je)
  
  junit.int <- tryCatch({
    list(
      ju.result = interval$je$r,
      ju.duration = replaceNullBy0(interval$je$d),
      ju.tests = paste(tests, collapse = "#"),
      ju.number =   length(tests)
    )}, error = function(e) {
      list(
        ju.result = NA,
        ju.duration = 0,
        ju.tests = "",
        ju.number = 0
      )
    })
  null.to.NA(c(base.int, junit.int))
}

project.processor <- function(project) {
  null.to.NA(list(
    id                         = project$id,
    belongToASingleSofware     = project$belongToASingleSofware,
    usesJunit                  = project$usesJunit,
    usesOtherTestingFrameworks = project$usesOtherTestingFrameworks,
    usesOtherTestingForms      = project$usesOtherTestingForms,
    productionPercentage       = project$productionPercentage,
    useJunitOnlyForUnitTesting = project$useJunitOnlyForUnitTesting,
    followTestDrivenDesign     = project$followTestDrivenDesign,
    userId                     = project$userId,
    name                       = project$name,
    website                    = project$website,
    regDate  	                 = project$regDate,
    ide  	                     = project$ide
  ))
}

user.processor <- function(user) {
  null.to.NA(list(
    id                    = user$id,
    programmingExperience = user$programmingExperience,
    country               = user$country,
    city                  = user$city,
    ip                    = user$ip,
    mayContactUser        = user$mayContactUser,
    operatingSystem       = user$operatingSystem,
    email                 = user$email,
    regDate		  = user$regDate
  ))
}

# Converts a base event to a data frame
super.event.processor <- function(event) {
  null.to.NA(list(
    et = event$et,
    wdv = event$wdv,
    ts = event$ts,
    ss = event$ss,
    uid = as.character(event$userId),
    pid = as.character(event$projectId),
    ide = event$ide
  ))
}

# Converts a breakpoint event to a data frame
breakpoint.event.processor <- function(event) {
  base.event <- super.event.processor(event)
  c(base.event, list("bt" = event$bt))
}

# Converts a breakpoint change event to a data frame
breakpoint.change.event.processor <- function(event) {
  base.event <- breakpoint.event.processor(event)
  c(base.event, list("ch" = event$ch))
}

# Convert all null values to NAs in the provided list
null.to.NA <- function(x = list()) {
  lapply(x, function(x) ifelse(is.null(x), NA, x))
}
