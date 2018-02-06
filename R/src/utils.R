library('rjson')
library(lmPerm)
library(ggplot2)
library(metap)

# Path variables 
pRes <- 'out'
pReports <- file.path(pRes,'reports')


# printf for R
printf <- function(...) invisible(print(sprintf(...)))

unwrap <- function(str) {
  strwrap(str, width=10000, simplify=TRUE)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

replaceNABy0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

replaceNaNBy0 <- function(x) {
  x[is.nan(x)] <- 0
  return(x)
}

replaceNullBy0 <- function(x) {
  x[is.null(x)] <- 0
  return(x)
}

is.empty <- function(x) {
  if(is.null(x)){
    return(TRUE)
  } else {
    nchar(x) == 0
  }
}

store.pdf <- function(data, where, name) {
  pdf(file.path(where, name))
  plot(data)
  dev.off()
}

sumDuration <- function(intervals)
  sum(as.numeric(intervals$duration))

interpret.spearman <- function(var1, var2, spearman, prefix = '', postfix = '') {
  intervals <- c(0, 0.3, 0.5, 0.7, 1)
  descr <- c('not', 'weakly', 'moderately', 'strongly')
  strength <- descr[Position(function(x){x == 1}, findInterval(intervals, spearman$estimate)) - 1]
  
  printf("%s %s is %s correlated (rho = %f, p = %f) with %s.%s", 
         prefix, var1, strength, spearman$estimate, spearman$p.value, var2, postfix)
}

compute.effectSize <- function(x, y, direction, paired=F) {
  ## Takes two probability distributions, performs a shapiro-wils test to see if they are normally distributed, determine their similarity and calculates different effect sizes.
  
  normal <- TRUE
  if(length(x) < 5000 && length(y) < 5000) {
    testres <- c(shapiro.test(x)$p.value, shapiro.test(y)$p.value)
    if (testres[0]<=0.05 || testres[1]<=0.05) {
      normal <- FALSE
    }
  }
  else {
    normal <- FALSE
  }
  
  if(!normal) {
    print(wilcox.test(x, y, alternative=direction, paired=paired))
    
    # effect-size
    # even with no statistical significance, calculate effect size
    library(effsize)
    print(VD.A(x, y))
    print(cohen.d(x, y))
  }
}


freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}


sparkline <- function(x, file.name) {
  # remove extreme outliers
  quant99 <- quantile(x, 0.99, na.rm = T)
  x <- Filter(function(y){y < quant99}, x)
  pdf(plot.location(file.name), height = 7, width = 14)
  hist(log(x), probability = TRUE, col = "blue", border = "white", 
       breaks = 15, xlab = "", ylab = "", axes = F, main = NULL)
  dev.off()
}

sparkline.standard <- function(x, file.name) {
  # remove extreme outliers
  quant99 <- quantile(x, 0.99, na.rm = T)
  x <- Filter(function(y){y < quant99}, x)
  pdf(plot.location(file.name), height = 7, width = 14)
  hist(x, probability = TRUE, col = "blue", border = "white", 
       breaks = 15, xlab = "", ylab = "", axes = F, main = NULL)
  dev.off()
}

# string magic for je
extract.test.result <- function(x) {
  strsplit(as.character(x), '@')[[1]][2]
}


unflatten.tests <- function(x) {
  strsplit(as.character(x), '#')[[1]]
}

remove.test.results <- function(x) {
  tryCatch(strsplit(as.character(x), '@')[[1]][1], error = function(e) { NA })
}

plot.location <- function(filename) {
  file.path("out", "figs", filename)
}

save.plot <- function(filename) {
  ggsave(plot.location(filename))
}

save.pdf <- function(plot, filename, width = 7, height = 7) {
  pdf(plot.location(filename), height = height, width = width)
  print(plot)
  dev.off()
}

## Code for permutation based MANOVA tests
three.way.permutation.test <- function(dependent, independent1, independent2, data_table){
  printf("")
  printf("Permutation test = %s ~ %s * %s", dependent, independent1, independent2)
  
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE]
  x1 <- data_table[, colnames(data_table)==independent1, with=FALSE]
  x2 <- data_table[, colnames(data_table)==independent2, with=FALSE]
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x1 <- as.matrix(x1)
  x2 <- as.matrix(x2)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x1)
  toRemove <- toRemove | is.na(x2)
  y <- y[!toRemove]
  x1 <- x1[!toRemove]
  x2 <- x2[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  
  # 4. we run the permutation based MANOVA
  data_table$y <- y
  data_table$x1 <- x1
  data_table$x2 <- x2
  results <- summary(aovp(y ~ x1*x2, data = data_table, maxIter=10000, perm="Prob", Ca=0.0001))[[1]]
  isSignificant <- FALSE
  
  # 5. data interpretation
  if (results$"Pr(Prob)"[1] <= 0.05)
    printf("%s INTERACTS with %s (p-vale = %f)", independent1, dependent, results$"Pr(Prob)"[1])
  else
    printf("%s DOES NOT interact with %s (p-vale = %f)", independent1, dependent, results$"Pr(Prob)"[1])
  
  if (results$"Pr(Prob)"[2] <= 0.05)
    printf("%s INTERACTS with %s (p-vale = %f)", independent2, dependent, results$"Pr(Prob)"[2])
  else
    printf("%s DOES NOT interact with %s (p-vale = %f)", independent2, dependent, results$"Pr(Prob)"[2])
  
  if (results$"Pr(Prob)"[3] <= 0.05){
    printf("(%s AND %s) INTERACT with %s (p-vale = %f)", independent1, independent2, dependent, results$"Pr(Prob)"[3])
    isSignificant <- TRUE
  }
  else
    printf("(%s AND %s) together DO NOT interact with %s (p-vale = %f)", independent1, independent2, dependent, results$"Pr(Prob)"[3])
  
  return(isSignificant)
}

plot.3way.interaction <- function (dependent, independent1, independent2, data_table, x.label=independent1, y.label=dependent){
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE]
  x1 <- data_table[, colnames(data_table)==independent1, with=FALSE]
  x2 <- data_table[, colnames(data_table)==independent2, with=FALSE]
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x1 <- as.matrix(x1)
  x2 <- as.matrix(x2)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x1)
  toRemove <- toRemove | is.na(x2)
  y <- y[!toRemove]
  x1 <- x1[!toRemove]
  x2 <- x2[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  data_table$y <- y
  data_table$x1 <- x1
  data_table$x2 <- x2
  
  # 4. plot the data gropued by 'independent2'
  p <- ggplot(data=data_table, aes(x=x1, y=y), colour=factor(x2)) + facet_grid(reformulate(independent2,"."), scale="free")
  p <- p + xlab(x.label) + ylab(y.label)
  # 5.1 use log scale
  p <- p +  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
  p <- p +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # 5.2 add trend line
  p <- p + stat_smooth(method="glm", formula = y ~ x, fullrange=F, se = F) + geom_point()
  save.pdf(p, paste("interaction_",dependent,"_",independent1,"_",independent2,".pdf", sep=""), width = 7, height = 7)
  #print(pp)
}

plot.3way.interaction2 <- function (dependent, independent1, independent2, data_table, ordering, x.label=independent1, y.label=dependent){
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE]
  x1 <- data_table[, colnames(data_table)==independent1, with=FALSE]
  x2 <- data_table[, colnames(data_table)==independent2, with=FALSE]
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x1 <- as.matrix(x1)
  x2 <- as.matrix(x2)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x1)
  toRemove <- toRemove | is.na(x2)
  y <- y[!toRemove]
  x1 <- x1[!toRemove]
  x2 <- x2[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  data_table$y <- y
  data_table$x1 <- x1
  data_table$x2 <- x2
  
  # 4. decide the order
  data_table <- within(data_table,  x2 <- factor(x2, levels = ordering))
  with(data_table, levels(x2))
  
  # 5. plot the data gropued by 'independent2'
  p <- ggplot(data=data_table, aes(x=x1, y=y), colour=factor(x2)) + facet_grid( ~ x2, scale="free")
  p <- p + xlab(x.label) + ylab(y.label)
  # 5.1 use log scale
  p <- p +  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # 5.2 add trend line
  p <- p + stat_smooth(method="lm", formula = y ~ x, fullrange=F, se = F) + geom_point()
  save.pdf(p, paste("interaction_",dependent,"_",independent1,"_",independent2,".pdf", sep=""), width = 7, height = 7)
  #print(pp)
}


two.way.permutation.test <- function(dependent, independent, data_table){
  printf("")
  printf("Permutation test = %s ~ %s", dependent, independent)
  p.value <- 0
  
  if (nrow(data_table)>10000){
    p.value <- multiple.perm.tests(dependent, independent, data_table)
  } else {
    p.value <- single.perm.test(dependent, independent, data_table)
  }
  if (p.value <= 0.05){
    isSignificant <- TRUE# 5. data interpretation
    printf("%s INTERACTS with %s (p-vale = %f)", independent, dependent, p.value)
  } else {
    isSignificant <- FALSE
    printf("%s DOES NOT interact with %s (p-vale = %f)", independent, dependent, p.value)
  }
  
  return(isSignificant)
}

multiple.perm.tests <- function (dependent, independent, data_table){
  # we split large datasets in small datasets with 20000 rows each
  # then, the achieved p-values are combined using the Fisher's method
  n.subset <- ceiling(nrow(data_table)/20000)
  p.values <- vector(mode="numeric", length=n.subset)
  for (index in 1:n.subset){
    # 1. get the first subset
    first.row = (index-1) * 20000+1
    last.row = min(index * 20000, nrow(data_table))
    subset <- data_table[first.row:last.row,]
    
    # 2. we retrieve the three columns from "subset"
    y <- subset[, colnames(subset)==dependent, with=FALSE] 
    x <- subset[, colnames(subset)==independent, with=FALSE] 
    
    # 3. convert the data in the right format
    y <- as.matrix(y)
    x <- as.matrix(x)
    
    # 4. we remove NA values
    toRemove <- is.na(y)
    toRemove <- toRemove | is.na(x)
    y <- y[!toRemove]
    x <- x[!toRemove]
    subset <- subset[!toRemove[,1],]
    subset$y <- y
    subset$x <- x
    
    # 5. Permutation test and storing p-values
    if (length(unique(y))>=2){
      results <- summary(aovp(y ~ x , data = subset, maxIter=1000, perm="Prob", Ca=0.0001))[[1]]
      #print(results)
      p.values[index] <- results$"Pr(Prob)"[1]
      if (p.values[index] == 0)
        p.values[index] <- 10^(-16)
    } else {
      p.values <- NA
    }
  }
  p.values <- p.values[!is.na(p.values)]
  return(sumlog(p.values)$p)
}

single.perm.test <- function (dependent, independent, data_table){
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE] 
  x <- data_table[, colnames(data_table)==independent, with=FALSE] 
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x <- as.matrix(x)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x)
  y <- y[!toRemove]
  x <- x[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  data_table$y <- y
  data_table$x <- x
  
  # 4. we run the permutation based MANOVA
  results <- summary(aovp(y ~ x , data = data_table, maxIter=10000, perm="Prob", Ca=0.0001))[[1]]
  return(results$"Pr(Prob)"[1])
}

plot.2way.interaction <- function (dependent, independent, data_table, x.label=independent1, y.label=dependent, log.scale=TRUE){
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE] 
  x <- data_table[, colnames(data_table)==independent, with=FALSE] 
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x <- as.matrix(x)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x)
  y <- y[!toRemove]
  x <- x[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  data_table$y <- y
  data_table$x <- x
  
  # 4. plot the data gropued by 'independent'
  p <- ggplot(aes(y = y, x = x), data = data_table) + geom_boxplot()
  p <- p + xlab(x.label) + ylab(y.label)
  # 5.1 use log scale
  if (log.scale){
    p <- p +  scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  }
  save.pdf(p, paste("interaction_",dependent,"_",independent,".pdf", sep=""), width = 7, height = 7)
  #print(pp)
}

plot.2way.interaction2 <- function (dependent, independent, data_table, ordering, x.label=independent1, y.label=dependent, log.scale=TRUE){
  # 1. we retrieve the three columns from "data_table"
  y <- data_table[, colnames(data_table)==dependent, with=FALSE] 
  x <- data_table[, colnames(data_table)==independent, with=FALSE] 
  
  # 2. convert the data in the right format
  y <- as.matrix(y)
  x <- as.matrix(x)
  
  # 3. we remove NA values
  toRemove <- is.na(y)
  toRemove <- toRemove | is.na(x)
  y <- y[!toRemove]
  x <- x[!toRemove]
  data_table <- data_table[!toRemove[,1],]
  data_table$y <- y
  data_table$x <- x
  
  # 4. decide the order
  data_table <- within(data_table,  x <- factor(x, levels = ordering))
  with(data_table, levels(x))
  
  # 5. plot the data gropued by 'independent'
  p <- ggplot(aes(y = y, x = x), data = data_table) + geom_boxplot()
  p <- p + xlab(x.label) + ylab(y.label)
  # 5.1 use log scale
  if (log.scale){
    p <- p +  scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  }
  save.pdf(p, paste("interaction_",dependent,"_",independent,".pdf", sep=""), width = 7, height = 7)
  #print(pp)
}
