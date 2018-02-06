#!/usr/bin/env Rscript
#
# (c) 2014 - onwards Georgios Gousios <gousiosg@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
library(optparse)
library(stringr)
library(doMC)

source('config.R')

# Disable scientific notation
options(scipen=999)

# Output Paths
base.dir        <- "out"
plot.location   <- file.path(base.dir, "figs")
report.location <- file.path(base.dir, "reports")

# Cmd-line parser
option_list <- list(
		make_option(c("-b", "--base-dir"), default=base.dir, dest = 'base.dir',
				help = "Base directory [\"%default\"]"),
		
		make_option(c("-s", "--mongo-server"), default=mongo.host, dest = 'mongo.host',
				help = "MongoDB host [\"%default\"]"),
		make_option(c("-u", "--mongo-user"), default=mongo.user, dest = 'mongo.user',
				help = "mongo user [\"%default\"]"),
		make_option(c("-p", "--mongo-passwd"), default=mongo.passwd, dest = 'mongo.passwd',
				help = "mongo password [\"%default\"]"),
		
		make_option(c("-n", "--num-processes"), default = num.processes, 
				dest = 'num.processes', type = "integer",
				help = "Number of processes to use when running in parallel [\"%default\"]")
)

parser <- OptionParser(usage = "usage: %prog [options] <cmd>", 
		option_list = option_list,
		epilogue = "<cmd> can be one of: gen-intervals-file, gen-events-file, gen-projects-file, gen-sequence-file, gen-users-file, gen-reports")

args <- parse_args(parser, print_help_and_exit = FALSE, positional_arguments = c(0,1))

if (args$options$help) {
	parse_args(parser)
	quit()
}

base.dir      = args$options$base.dir
mongo.user    = args$options$mongo.user
mongo.passwd  = args$options$mongo.passwd
mongo.host    = args$options$mongo.host
num.processes = args$options$num.processes
registerDoMC(num.processes)

plot.location   <- file.path(base.dir, "figs")
latex.location  <- file.path(base.dir, "latex")
report.location <- file.path(base.dir, "reports")

gen.intervals.file <- function() {
	source('src/gen-intervals-file.R')
}

gen.events.file <- function() {
  source('src/gen-events-file.R')
}

gen.users.file <- function() {
	source('src/gen-users-file.R')
}

gen.projects.file <- function() {
	source('src/gen-projects-file.R')
}

gen.sequence.file <- function() {
	source('src/gen-sequence-file.R')
}

gen.reports <- function() {
	source('src/reports/project-report.R')
	conn <- mongo.connection()
	projects.100.int <- projects.with.intervals(50, mongo.conn = conn)
	printf("%d projects with 100 intervals", length(projects.100.int))
	print("Calculating values for all projects")
	printf("Using %d processors", num.processes)
	all.project.report.values(projects.100.int)
	printf("Report values saved to %s", cache.name)
	print("Generating reports")
	mean.project.values <<- calculate.mean.project.values()
	gen.project.reports(projects.100.int)
}

if(is.null(args$args)) {
	print("Not enough arguments. Run with --help to see supported options.")
} else {
	switch(args$args,
			"gen-intervals-file" = gen.intervals.file(),
			"gen-events-file" = gen.events.file(),
			"gen-users-file" = gen.users.file(),
			"gen-projects-file" = gen.projects.file(),
			"gen-sequence-file" = gen.sequence.file(),
			"gen-reports" = gen.reports(),
			"Don't know what to do")
}
