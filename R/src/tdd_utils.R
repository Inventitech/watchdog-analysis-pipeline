#
# (c) 2015 - onwards the TestRoots team
#
# MIT Licensed, see LICENSE in top level dir
#

library(stringr)

source("src/config-filtering.R")

# regular expression for TDD
tdd.lenient <-  "((jO)*(tx)+(ty)*)+((jE)+(ty)+)+jO"
tdd.strict <-   "jO((jO)*(tx))+((jE)+(ty)+)+jO"
tdd.refactor <- "jO((tx(jE)*)|(ty(jE)*))+jO"
tdd.integral <- "((jE)+(ty)+)+jO"

tdd.matching.number <- function(regex, sequence){
	match <- gregexpr(regex,sequence, perl=TRUE)
	length <- max(0, sum(attr(match[[1]],"match.length"))) # to avoid that gregexpr gives -1
	if (length == 0)
		number <- 0
	else {
		number <- length(attr(match[[1]],"match.length"))
		sequence.sub <- str_replace_all(sequence,regex,"jO")
		number <- number + tdd.matching.number(regex, sequence.sub)
	}
	return(number)
}

tdd.matching.length <- function(regex, sequence){
	match <- gregexpr(regex,sequence, perl=TRUE)
	length <- max(0, sum(attr(match[[1]],"match.length"))) # to avoid that gregexpr gives -1
	if (length > 0){
		sequence.sub <- str_replace_all(sequence,regex,"jO")
		length <- length + tdd.matching.length(regex, sequence.sub)
	}
	return(length)
}

tdd.matching <- function(regex, sequence){
	match <- gregexpr(regex,sequence, perl=TRUE)
	length <- max(0, sum(attr(match[[1]],"match.length"))) # to avoid that gregexpr gives -1
	if (length > 0){
		sequence.sub <- str_replace_all(sequence,regex,"jO")
		length <- length + tdd.matching.length(regex, sequence.sub)
	}
	return(length)
}

tdd.boolean.matching <- function(regex, sequence){
	boolean <- matrix(FALSE, 1, nchar(sequence))
	match <- gregexpr(regex,sequence, perl=TRUE)
	if (sum(attr(match[[1]],"match.length"))==-1)
		return(boolean)
	length <- attr(match[[1]],"match.length") 
	position <- match[[1]]
	for (i in 1:length(position)){
		boolean[position[i]:(position[i]+length[i]-1)] <- TRUE
	}
	return(boolean)
}

boolean.union <- function(a, b){
	c <- (a+b)>0
	return(c)
}

# Creates tx for modifying tests
# Creates ty for modifying production code
# Creates jO for successful test case execution of argument testcase
# Creates jE for errorneous test case execution of argument testcase
# Creates jy? if it contains no test case. This might be an error
create.sequence <- function(table, testcase){
	sequence <- ""
	for (i in 1:length(table$it)){
		if (table$it[i]=="ty") {
		  # TODO (AP) Can't we make a better refinement here, with non-buggy WD versions?
			if (table$doc.dt[i] %in% test.types) { sequence <- paste(sequence, "tx", sep="") }
			else {sequence <- paste(sequence, "ty", sep="")}
		}
		if (table$it[i]=="ju") { # && table$ju.result[i]!="U"
			locate.testcase <- as.data.frame(str_locate(table$ju.tests[i], testcase))
			contains.testcase <- NAtoFALSE(locate.testcase$end > locate.testcase$start)
			if (contains.testcase) {
				test.result <- str_sub(table$ju.tests[i], locate.testcase$end+2, locate.testcase$end+2)
				if (test.result == "F" || test.result == "U") { 
				  test.result <- "E"
				}
				# Explicitly leaves jIgnored test cases as jI, which other analyses will disregard. 
				# An ignored test case can be considered not executed, so this behavior is fine.
				sequence <- paste(sequence, "j", toupper(test.result), sep="")
			}
			else {
			  	# TODO (AP) What do we use jy for?
				sequence <- paste(sequence, "jy", sep="")
			}
		}
	}
	return(sequence)
}


find.all.testcases <- function(intervals){
	all.tc <- c()
	table.ju <- subset(intervals, intervals$it=="ju")
	for (i in 1:length(table.ju$it)){
		test.set <- unflatten.tests(table.ju$ju.tests[i])
		test.set <- Map(remove.test.results, test.set)
		all.tc <- union(all.tc, test.set)
	}
	return(all.tc)
}




testTDD <- function(tddPattern, testSequence, expectedOutcome, expectedNMatches) {
	outcome <- tdd.matching.length(tddPattern, testSequence)
	match.number <- tdd.matching.number(tddPattern, testSequence)
	if(outcome != expectedOutcome || match.number != expectedNMatches) {
		printf("Test %s failed. Expected: %s. Outcome: %s Expected N. matches %s Outcome N. matches %s", 
				testSequence, expectedOutcome, outcome, expectedNMatches, match.number)
	}
	else {
		printf("Test %s succeeded.", testSequence)
	}
}

testTDD(tdd.integral, "jEjEtyjEtytytyjE", 0, 0)
testTDD(tdd.integral, "jEjEtyjEtytytyjEjO", 0, 0)
testTDD(tdd.integral, "jEjEtyjEtytytyjO", nchar("jEjEtyjEtytytyjO"), 1)
testTDD(tdd.integral, "stupidstuffjEjEtyjEtytytyjOstupidstuff", nchar("jEjEtyjEtytytyjO"), 1)
testTDD(tdd.integral, "stupidstuffjEjEtyjEtytytyjOstupidstuffj0", nchar("jEjEtyjEtytytyjO"), 1)

testTDD(tdd.lenient, "jEjEtyjEtytytyjO", 0, 0)
testTDD(tdd.lenient, "jOtxjEtyjO", nchar("jOtxjEtyjO"), 1)
testTDD(tdd.lenient, "jOtxjEjEjEtyjEtytytyjO", nchar("jOtxjEjEjEtyjEtytytyjO"), 1)
testTDD(tdd.lenient, "jOtxjEtxtxtxjOtxjEjEjEtyjEtytytyjO", nchar("txtxtxjOtxjEjEjEtyjEtytytyjO"), 1)
testTDD(tdd.lenient, "jOtxjEtyjO jOtxjEtyjO", nchar("jOtxjEtyjOjOtxjEtyjO"), 2)

testTDD(tdd.lenient, "txjOtxtytxjEtyjOtxtytxtyjEtyjO", 30,2)

testTDD("jO((tx(jE)*)|(ty(jE)*))+jO", "jOtyjO",6,1)

NAtoFALSE <- function(x) {
	if (is.na(x)) {
		return(FALSE)
	}
	return(x)
}

number.of.testcase.executions <- function(sequence) {
  # This is a super fast way of counting occurences in a string, according to 
  # stringr documentation. 
  str_count(sequence, fixed("jO")) + str_count(sequence, fixed("jE"))
}

flaky.testcase.pattern <-  "(jOjE)|(jEjO)"
testTDD(flaky.testcase.pattern, "jEjO", 4, 1)
testTDD(flaky.testcase.pattern, "jEjOjOjE", 8, 2)
testTDD(flaky.testcase.pattern, "jEjOtxtyjOjE", 8, 2)
testTDD(flaky.testcase.pattern, "jEtyjOtxtyjOtxjE", 0, 0)

