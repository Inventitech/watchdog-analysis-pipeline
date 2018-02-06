#
# (c) 2016 - onwards Moritz Beller <moritzbeller@gmail.com>
#
# MIT Licensed, see LICENSE in top level dir
#
library(plyr)
library(data.table)
library(fastmatch)
library(ggplot2)

library(ggthemes)

not.supported.wdv <- c("1.0-RC",
                       "1.0.1",
                       "1.0.2",
                       "1.1.0",
                       "1.1.0.SNAPSHOT",
                       "1.1.1",
                       "1.1.2",
                       "1.2.0",
                       "1.2.1",
                       "1.3.0",
                       "1.4.0",
                       "1.4.1",
                       "1.4.2")

# filtering on WD versions greater 1.5 -- when we introduced wo monitoring. Also only on EC versions
intervals.comparative.wdviews <- subset(intervals, ! wdv %in% not.supported.wdv & IDE.x == "EC")
intervals.wdviews <- subset(intervals, it == 'wo')

## Result: number of times WDView was consulted
nrow(intervals.wdviews)

## Result: duration of WD view consultation
### Result: Length of WDView
summary(intervals.wdviews$duration/1000)
printf("The sum duration is %f seconds.", sum(intervals.wdviews$duration/1000, na.rm = T))
printf("The quantile sum duration is %f seconds.", sum(quantile(intervals.wdviews$duration/1000, 0.95), na.rm = T))

printf("%i unique users consulted the WDView", length(unique(intervals.wdviews$uid)))
# Add mean sum per user
aggregated.wdview <- aggregate(duration ~ uid, intervals.wdviews, sum)
summary(aggregated.wdview)


## Result: number of all sessions / number of sessions in which WDView was consulted
## Percentage of sessions in which WDView was consulted
printf("number of wdview sessions %i", length(unique(intervals.wdviews$ss)))
printf("number of all sessions %i", length(unique(intervals.comparative.wdviews$ss)))
