source('src/header.R')

intervals.file <- file.path(pRes,'intervals.csv')

if(!file.exists(intervals.file)) {
  printf("File %s does not exist. Generate intervals first!", intervals.file)
  quit()
}

intervals.copy <- intervals

intervals.copy[which(intervals.copy$it=="ju"),]$ts <- intervals.copy[which(intervals.copy$it=="ju"),]$te # Non-junit intervals.copy start after the end of junit execution (later the intervals.copy are ordered by "starting time" ts)
intervals.copy[which(intervals.copy$it=="ua"),]$ts <- intervals.copy[which(intervals.copy$it=="ua"),]$te # inactive
intervals.copy[which(intervals.copy$it=="eo"),]$ts <- intervals.copy[which(intervals.copy$it=="eo"),]$te # closed eclipse
intervals.copy[which(intervals.copy$it=="ea"),]$ts <- intervals.copy[which(intervals.copy$it=="ea"),]$te # change to browser

#list of interval sequences by project
results <- vector(mode = "list", length = length(intervals.copy[,1]))

tot_session_project <- 0
unique_session <- as.matrix(unique(intervals.copy$session))

for (i in 1:length(unique_session)){
  sub_table <- intervals.copy[which(intervals.copy$session==unique_session[i]),]
  unique_project <- as.matrix(unique(sub_table$pid))
  
  for (j in 1:length(unique_project)){
    sub_table2 <- sub_table[which(sub_table$pid==unique_project[j]),]
    
    #order sub_table2 by "ts" in ascending order
    sub_table2 <- sub_table2[with(sub_table2, order(ts, decreasing=F)), ]
    
    junit_counter <- length(sub_table2[which(sub_table2$it=="ju"),it])
    contains_junit <- (junit_counter>0)
    
    # build the sequence of interval or each project in each session
    # intervals  are separated by the character "####"
    # fiedls of each intervals are separated by ":"
    if (contains_junit) {      
      tot_session_project <- tot_session_project+1
      current.sequence <- ""
      for (iter in 1:length(sub_table2$uid)) {
        current.sequence <- paste(current.sequence, sub_table2$it[iter], #it
                                                    sub_table2$duration[iter], #duration
                                                    sub_table2$ju.result[iter], #ju.result
                                                    sub_table2$doc.dt[iter], #doc.dt
                                                    sub_table2$ju.tests[iter], #ju.tests
                                                    sub_table2$ts[iter], #ts
                                                    sub_table2$te[iter], #te
                                                    sep=":")
        current.sequence <- paste(current.sequence, "####", sep="")
      }
      
      results[[tot_session_project]] <- list(
                                              session = unique_session[i],
                                              pid = unique_project[j],
                                              intervals = current.sequence
                                            ) 
    }
  }  
  printf("[gen-sequence-file]: Session %s Progress %s", unique_session[i], (i/length(unique_session)*100))
}

results <- rbindlist(results, fill = T)
printf("[gen-sequence-file]: writing sequence_with_duration.csv")
write.csv(results, file = file.path(pRes,'sequence_with_duration.csv'))
