#
# (c) 2014 - onwards Georgios Gousios <gousiosg@gmail.com>, Moritz Beller
#
# MIT Licensed, see LICENSE in top level dir
#

library(data.table)
library(timeline)
library(rmarkdown)
library(plyr)

source('src/utils.R')
source('src/data-access.R')
source('config.R')
source('src/tdd_utils.R')

library(foreach)
library(doMC)
registerDoMC(num.processes)

empty.report.values <- function() {
	list(
			'project.id'              = '',
			'num.intervals'           = 0,
			'total.time'              = 0,
			'avg.per.day'             = 0,
			'active.eclipse.usage'    = 0,
			'perc.time.spent.reading' = 0,
			'perc.time.spent.typing'  = 0,
			'time.remaining'          = 0,
			'perc.time.java.read'     = 0,
			'perc.time.java.write'    = 0,
			'perc.time.test'          = 0,
			'perc.time.prod'          = 0,
			'perc.time.prod.est'      = 0,
			'perc.time.test.est'      = 0,
			'time.debugging'          = 0,
			'perc.time.debugging'     = 0,
			'num.test.executions'     = 0,
			'num.test.executions.per.day' = 0,
			'num.test.failures'       = 0,
			'perc.test.failures'      = 0,
			'avg.test.duration'       = 0,
			'tdd.frequency' 		  = 0,
			'tdd.frequency.word'	  = 'never', 
			'tdd.refactoring.perc'	  = 0,
			'tdd.test.perc'			  = 0,
			'num.debug.sessions'      = 0,
			'time.debug.sessions'     = 0,
			'avg.debug.session.duration' = 0
	)
}


cache.name <- file.path(pRes, "all.project.report.values.csv")
cached.values <- data.frame(empty.report.values())

# Read cached report values for all projects
cache <- function() {
	if(file.exists(cache.name)) {
		if(nrow(cached.values) <= 1) {
			cached.values <<- read.csv(cache.name)
		}
	}
	cached.values
}

filter <- function(xs) {
	Filter(function(x){!is.na(x)},
			Filter(function(x){!is.infinite(x)}, xs))
}

calculate.mean.project.values <- function() {
	list (
			'mean.num.intervals'           = mean(cache()$num.intervals, na.rm = T),
			'mean.total.time'              = mean(cache()$total.time, na.rm = T),
			'mean.avg.per.day'             = mean(filter(cache()$avg.per.day), na.rm = T),
			'mean.active.eclipse.usage'    = mean(filter(cache()$active.eclipse.usage), na.rm = T),
			'mean.perc.time.spent.reading' = mean(cache()$perc.time.spent.reading, na.rm = T),
			'mean.perc.time.spent.typing'  = mean(cache()$perc.time.spent.typing, na.rm = T),
			'mean.time.remaining'          = mean(cache()$time.remaining, na.rm = T),
			'mean.perc.time.java.read'     = mean(cache()$perc.time.java.read, na.rm = T),
			'mean.perc.time.java.write'    = mean(cache()$perc.time.java.write, na.rm = T),
			'mean.perc.time.test'          = mean(cache()$perc.time.test, na.rm = T),
			'mean.perc.time.prod'          = mean(cache()$perc.time.prod, na.rm = T),
			'mean.perc.time.prod.est'      = mean(cache()$perc.time.prod.est, na.rm = T),
			'mean.perc.time.test.est'      = mean(cache()$perc.time.test.est, na.rm = T),
			'mean.time.debugging'          = mean(cache()$time.debugging, na.rm = T),
			'mean.num.test.executions.per.day'     = mean(filter(cache()$num.test.executions.per.day), na.rm = T),
			'mean.num.test.executions'     = mean(filter(cache()$num.test.executions), na.rm = T),
			'mean.num.test.failures'       = mean(filter(cache()$num.test.failures), na.rm = T),
			'mean.avg.test.duration'       = mean(cache()$avg.test.duration, na.rm = T),
			'projects.tdd.frequency'       = cache()$tdd.frequency,
			'mean.num.debug.sessions'      = mean(cache()$num.debug.sessions, na.rm = T),
			'mean.time.debug.sessions'     = mean(cache()$time.debug.sessions, na.rm = T),
			'mean.avg.debug.session.duration' = mean(cache()$avg.debug.session.duration, na.rm = T)
	)
}

mean.project.values <- calculate.mean.project.values()

# Get all project ids for which an interval exists and that have at least the
# provided number of arguments recorded. Requires a mongo.conn object in the
# environment
projects.with.intervals <- function(min.intervals = 1000, mongo.conn = NULL) {
	# Unfortunately, aggregations don't work yet:
	# mongo.aggregation(mongo.conn, 'watchdog.intervals',
	#                  mongo.bson.from.JSON('{"$group": {"_id":"$projectId", "count": {"$sum":1}}}'))
	all <- mongo.distinct(mongo.conn, "watchdog.intervals", "projectId")
	num.intervals <- Map(function(x) {
				c <- mongo.count(mongo.conn, "watchdog.intervals",
						mongo.bson.from.JSON(sprintf("{\"projectId\": \"%s\"}", x)))
				list(id = x, intervals = c)
			}, all)
	subset(rbindlist(num.intervals), intervals > min.intervals)$id
}

# Get a project JSON object a list of lists.
project.entry <- function(project.id, mongo.conn = NULL) {
	mongo.bson.to.list(mongo.find.one(mongo.conn, "watchdog.projects",
					query = sprintf('{"id" : "%s"}', project.id)))
}

# Generate report for project
project.report.values <- function(pid, mongo.conn = NULL) {
	if(nrow(subset(cache(), project.id == pid)) == 1) {
		return(as.list(subset(cache(), project.id == pid)))
	}
	
	result <- empty.report.values()
	result$project.id <- pid
	project <- project.entry(pid, mongo.conn)
	
	intervals <- get.intervals(pid, c(), super.processor, mongo.conn)
	if (nrow(intervals) == 0) {return(result)}
	intervals$delta <- (intervals$te - intervals$ts) / 1000
	
	result$num.intervals <- nrow(intervals)
	
	first.interval <- head(intervals[with(intervals, order(ts)), ], 1)
	last.interval <- head(intervals[with(intervals, order(-ts)), ], 1)
	total.days <- (last.interval$ts - first.interval$ts) / 1000 / 3600 / 24
	total.time <- round((sum(subset(intervals, it == 'eo')$delta) / 3600), 1)
	
	result$total.time  <- total.time
	result$avg.per.day <- round(total.time/total.days,1)
	
	time.eo <- sum(subset(intervals, it == 'eo')$delta)
	time.ua <- sum(subset(intervals, it == 'ua')$delta)
	time.re <- sum(subset(intervals, it == 're')$delta)
	time.ty <- sum(subset(intervals, it == 'ty')$delta)
	
	perc.time.spent.reading <- floor((time.re / time.ua) * 100)
	perc.time.spent.typing  <- floor((time.ty / time.ua) * 100)
	
	result$active.eclipse.usage    <- min(100,floor((time.ua / time.eo) * 100))
	result$perc.time.spent.reading <- perc.time.spent.reading
	result$perc.time.spent.typing  <- perc.time.spent.typing
	result$time.remaining          <- 100 - perc.time.spent.reading - perc.time.spent.typing
	
	# Java development Behaviour
	editor.intervals <- get.intervals(pid, c('re', 'ty'), mongo.conn = mongo.conn)
	if (nrow(editor.intervals) > 0) {
		editor.intervals$delta <- (editor.intervals$te - editor.intervals$ts) / 1000
		
		time.java.read  <- sum(subset(editor.intervals, doc.dt == "pr" & it == "re")$delta)
		time.java.write <- sum(subset(editor.intervals, doc.dt == "pr" & it == "ty")$delta)
		time.java.total <-  time.java.read + time.java.write
		
		result$perc.time.java.read  <- (time.java.read  / time.java.total) * 100
		result$perc.time.java.write <- (time.java.write / time.java.total) * 100
		
		time.test.actual <- sum(subset(editor.intervals, doc.dt == "te" | doc.dt == "tf" | doc.dt == "lt")$delta) / 3600
		time.prod.actual <- sum(subset(editor.intervals, doc.dt == "pr")$delta) / 3600
		time.total <- time.test.actual + time.prod.actual
		
		result$perc.time.test <- (time.test.actual / time.total) * 100
		result$perc.time.prod <- (time.prod.actual / time.total) * 100
		
		result$perc.time.prod.est <- project$productionPercentage
		result$perc.time.test.est <- 100 - project$productionPercentage
	}
	
	presp.intervals <- get.intervals(pid, c('pe'), mongo.conn = mongo.conn)
	if (nrow(presp.intervals) > 0) {
		presp.intervals$delta <- (presp.intervals$te - presp.intervals$ts) / 1000
		result$time.debugging <- floor(sum(subset(presp.intervals, pet == 'de')$delta) / 3600)
	}
	
	# Testing
	junit.intervals <- get.intervals(pid, c('ju'), mongo.conn = mongo.conn)
	if (nrow(junit.intervals) > 0) {
		num.test.failures <- ifelse(nrow(junit.intervals) == 0, 0, nrow(subset(junit.intervals, ju.result == "E" | ju.result == "F")))
		avg.test.duration <- mean(junit.intervals$ju.duration)
		
		result$num.test.executions.per.day <- ceiling(nrow(junit.intervals) / total.days)
		result$num.test.executions <- nrow(junit.intervals)
		result$num.test.failures   <- num.test.failures
		result$perc.test.failures  <- replaceNABy0(result$num.test.failures/result$num.test.executions)*100
		result$avg.test.duration   <- mean(junit.intervals$ju.duration)
		
		##############################################
		# Test-Driven analysis
		##############################################
		# we select only interval of types "ju" and "ty"
		ty.intervals <- subset(editor.intervals, it=="ty")
		intervals.to.sequalize <- rbind(ty.intervals, junit.intervals, fill=T)
		intervals.to.sequalize[which(intervals.to.sequalize$it=="ju"),]$ts <- intervals.to.sequalize[which(intervals.to.sequalize$it=="ju"),]$te # Non-junit intervals.copy start after the end of junit execution (later the intervals.copy are ordered by "starting time" ts)
		
		
		# Load project-specific TDD cache and read-in result values
		tdd.project.cache.file <- file.path(pRes, "tdd.cache", sprintf("%s.csv", pid))
		tdd.project.cache <- load.or.generate.tdd.project.cache(tdd.project.cache.file)
		
		for(session in unique(intervals.to.sequalize$ss)) {
			intervals.per.session <- subset(intervals.to.sequalize, ss == session)
			contains.junit.execution <- (nrow(subset(intervals.per.session, intervals.per.session$it=="ju")) > 0)
			if (!contains.junit.execution) {
				next;
			}
			
			# caching stratgey: Look-up of current session in project-specific TDD cache
			# Abort in case of up-to-date cache hit
			cached.session.results <- tdd.project.cache[tdd.project.cache$session == session,]
			cache.hit <- nrow(cached.session.results) > 0
			if(cache.hit && cached.session.results[1,]$length == nrow(intervals.per.session)) {
				printf("[Cache-Manager] Using values from cache for session %s", session)
				next;
			}
			printf("[Cache-Manager] (Re-)computing values for session %s", session)
			
			# order intervals by their start time
			intervals.per.session <- intervals.per.session[with(intervals.per.session, order(ts)), ]
			
			# TODO (MMB) Refactor this into TDD Utils to make it usable from both RQ4 and project-report gen? 
			# find all test cases in the current table
			test.set <- find.all.testcases(intervals.per.session)
			if (length(test.set) > 0) {
				# first we identify the size of the sequence
				sequence.size <- nchar(create.sequence(intervals.per.session, test.set[[1]]))
				boolean.vector.strict <- matrix(FALSE, 1, sequence.size)
				boolean.vector.lenient <- matrix(FALSE, 1, sequence.size)
				boolean.vector.refactor <- matrix(FALSE, 1, sequence.size)
				
				for (test in test.set) {  
					#printf("[RQ4: found TDD pattern]: Test Case %s", test)
					sequence <- create.sequence(intervals.per.session, test)
					boolean.vector.strict <- boolean.union(boolean.vector.strict, tdd.boolean.matching(tdd.strict, sequence))
					boolean.vector.lenient <- boolean.union(boolean.vector.lenient, tdd.boolean.matching(tdd.lenient, sequence))
					boolean.vector.refactor <- boolean.union(boolean.vector.refactor, tdd.boolean.matching(tdd.refactor, sequence))
				}
				
				# Build an aggregated session-project entry in tdd.project.cache	
				cached.session.results[1,]$session <- session
				# TODO (MMB) is seq.length always equal to nchar(sequence)/2? then we can save some comp. time here
				cached.session.results[1,]$length <- nrow(intervals.per.session)
				cached.session.results[1,]$seq.length = nchar(sequence)/2
				cached.session.results[1,]$tdd.lenient.length <- sum(boolean.vector.lenient)/2
				cached.session.results[1,]$tdd.strict.length = sum(boolean.vector.strict)/2
				# for refactoring phase, we save the matching data iff "tdd.lenient.length">0 or "tdd.strict.length">0
				# otherwise it has no sense to check for "refactoring" when TDD is not applied
				if  (cached.session.results[1,]$tdd.lenient.length>0 
						|| cached.session.results[1,]$tdd.strict.length>0){
					cached.session.results[1,]$tdd.refactor.length = sum(boolean.vector.refactor)/2
				}
				else {
					cached.session.results[1,]$tdd.refactor.length = 0
				}
			}
			else {
				cached.session.results[1,]$session <- session
				cached.session.results[1,]$length <- nrow(intervals.per.session)
				cached.session.results[1,]$seq.length = nrow(intervals.per.session)
				cached.session.results[1,]$tdd.lenient.length <- 0
				cached.session.results[1,]$tdd.strict.length = 0
				cached.session.results[1,]$tdd.refactor.length = 0
			}
			
			# Write results to project cache
			if(cache.hit) {
				tdd.project.cache[tdd.project.cache$session == session,] <- cached.session.results[1,]
			}
			else {
				tdd.project.cache <- rbind(tdd.project.cache, cached.session.results)
			}
		} # finished session
		
		write.csv(tdd.project.cache, file = tdd.project.cache.file, row.names=F)
		printf("wrote [project-tdd-cache] %s", tdd.project.cache.file)
		
		# lift per-session values to per-project base
    tdd.num.refact <- mean(tdd.project.cache$tdd.refactor.length)
    tdd.num.test <- mean(tdd.project.cache$tdd.lenient.length)
		tdd.denom <-  mean(tdd.project.cache$tdd.refactor.length+tdd.project.cache$tdd.lenient.length)
    
		tdd.lenient.ratio = replaceNaNBy0(tdd.project.cache$tdd.refactor.length/tdd.project.cache$seq.length)
		tdd.test.phase.ratio = replaceNaNBy0(tdd.num.test/tdd.denom)
		tdd.refactor.phase.ratio = replaceNaNBy0(tdd.num.refact/tdd.denom)
		
		result$tdd.frequency <- replaceNABy0(mean(tdd.lenient.ratio,na.rm=T))*100
		result$tdd.frequency.word <- describeFrequency(result$tdd.frequency)
		result$tdd.refactoring.perc <- tdd.refactor.phase.ratio*100
		result$tdd.test.perc <- tdd.test.phase.ratio*100
	} # has junit intervals
	
	##############################################
	# Debugging
	##############################################
	debug.intervals = subset(intervals, it == 'db')
	result$num.debug.sessions <- nrow(debug.intervals)
	result$time.debug.sessions <- sum(debug.intervals$delta)
	result$avg.debug.session.duration <- result$time.debug.sessions / result$num.debug.sessions

	result
}

# Loads the TDD project cache of a file or returns an empty, pre-allocated dataframe to fill.
load.or.generate.tdd.project.cache <- function(tdd.project.cache.file) {
	tdd.project.cache <- data.frame(
			session=character(),
			length = integer(),
			seq.length = integer(),
			tdd.lenient.length = double(),
			tdd.strict.length=double(),
			tdd.refactor.length=double(),
			stringsAsFactors=FALSE)
	
	if(file.exists(tdd.project.cache.file)) {
		tdd.project.cache <- read.csv(tdd.project.cache.file, numerals=c("no.loss"), stringsAsFactors=F)
	}
	
	tdd.project.cache
}


describeFrequency <- function(x) {
	if(x == 0)
		"never"
	else if(x < 5)
		"almost never"
	else if(x < 10)
		"seldom"
	else if(x < 20)
		"sometimes"
	else if(x< 50)
		"quite often"
	else if(x < 60)
		"often"
	else if(x<80)
		"almost always"
	else
		"always"
}

# Generate and save a data.frame with all report values for all projects in
# the database. If pids is non-empty, reports will be generated just for these
# projects ids.
all.project.report.values <- function(pids = c(), cache.file = cache.name) {
	mongo.conn <- mongo.connection()
	
	if (length(pids) == 0) {
		pids <- projects.with.intervals(mongo.conn)
	}
	
	results <- foreach(x=pids, .combine=rbind, .verbose = F) %dopar% {
		#for(x in pids){
		tryCatch({
					mongo.conn <- mongo.connection()
					data.frame(project.report.values(x, mongo.conn))
				}, finally = {
					if(mongo.is.connected(mongo.conn)) {
						mongo.disconnect(mongo.conn)
					}
				})
	}
	if(mongo.is.connected(mongo.conn)) {
		mongo.disconnect(mongo.conn)
	}
	write.csv(results, file = cache.file)
	results
}

# Generate a report for the provided project id
gen.project.report <- function(pid, format = 'html') {
	doc.type <- switch(format,
			html = "html_document",
			pdf  = "pdf_document"
	)
	dir.create(report.location, showWarnings = F, recursive = T)
	tryCatch({ 
				printf("Rendering report ... %s", pid) 
				mongo.conn <<- mongo.connection()
				pid <<- pid
				render('src/reports/project-report.Rmd',
						output_format = doc.type,
						output_file = sprintf("%s.%s", pid, format),
						output_dir = file.path('../..',report.location))
			}, finally = {
				if(mongo.is.connected(mongo.conn)) {
					mongo.disconnect(mongo.conn)
				}
			})
}

# Generate reports for all projects
gen.project.reports <- function(pids, format = 'html') {
	for(x in pids) {
		#results <- foreach(x=pids, .combine=c, .verbose = F) %dopar% {
		tryCatch(gen.project.report(x, format),
			error = function(e) { printf("ERROR: Could not generate report for %s. Error: %s", x, e) })
	}
}

# Plotting
histograms.all.variables <- function(cache = cache) {
	library(reshape)
	c <- melt(cache()[,-c(1)])
	p <- ggplot(c, aes(x = value)) + 
			facet_wrap(~variable, scales = "free") + 
			geom_histogram() +
			theme_bw(base_size = 10) 
	
	store.pdf(p, plot.location, 'histogram-all-vars.pdf')
}
