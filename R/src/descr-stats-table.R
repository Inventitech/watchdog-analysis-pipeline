#
# (c) 2012 -- 2014 Georgios Gousios <gousiosg@gmail.com>
#

library(digest)
library(xtable)

# Create a descriptive statistics latex table given a dataset and prototype
# table with two columns (Feature and Description)
stats.table <- function(all = data.frame(),
                        descr.stats = data.frame(Feature = c(), Description = c()),
                        label = "tab:features", caption = "",
                        float = "table*", fname = "feature-stats.tex",
                        transform = "identity") {

  quant5 <- function(x){quantile(x, 0.05)}
  quant95 <- function(x){quantile(x, 0.95)}

  descr.stats$Feature   <- as.character(descr.stats$Feature)
  descr.stats$Unit      <- as.character(descr.stats$Unit)
  descr.stats$quant_05  <- lapply(descr.stats$Feature, function(x){quantile(all[,x], 0.05, na.rm = T)})
  descr.stats$Mean      <- lapply(descr.stats$Feature, function(x){mean(all[,x], na.rm = T)})
  descr.stats$Median    <- lapply(descr.stats$Feature, function(x){median(all[,x], na.rm = T)})
  descr.stats$quant_95  <- lapply(descr.stats$Feature, function(x){quantile(all[,x], 0.95, na.rm = T)})
  descr.stats$Histogram <- lapply(descr.stats$Feature, function(x) {
    print(sprintf("Histogram for %s", x))
    data  <- switch(transform,
                    "identity" = all[, x],
                    "log" = log(all[, x]),
                    all[,x])
    unq   <- digest(sprintf("descr.stats.hist.%s",as.character(x)))
    fname <- file.path(plot.location, sprintf("hist-%s.pdf",unq))

    par(mar=c(0,0,0,0))
    plot.window(c(0,1),c(0,1),  xaxs='i', yaxs='i')
    pdf(file = fname , width = 6, height = 3)
    hist(data, probability = TRUE, col = "red", border = "white",
         breaks = 10, xlab = "", ylab = "", axes = F, main = NULL)
    dev.off()
    sprintf("\\includegraphics[scale = 0.1, clip = true, trim= 50px 77px 50px 75px]{hist-%s.pdf}", unq)
  })

  
  descr.stats <- subset(descr.stats, select = -c(Feature))
  print(str(descr.stats))
  table <- xtable(descr.stats, label = label,
                  caption = caption,
                  align = c("c","r", "c", rep("r", 4), "c"))

  print.xtable(table, file = file.path(latex.location, fname),
               floating.environment = float,
               include.rownames = F, size = c(-2),
               sanitize.text.function = function(str)gsub("_","\\_",str,fixed=TRUE))
}
