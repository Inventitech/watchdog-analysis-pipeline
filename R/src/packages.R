# Load or install and load R packages

## Default repo
local({r <- getOption("repos");
       r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

if (! "devtools" %in% installed.packages())
  install.packages("devtools")
library(devtools)


if (!"rmongodb" %in% installed.packages()) install_github("dselivanov/rmongodb")
if (!"data.table" %in% installed.packages()) install.packages("data.table")
if (!"timeline" %in% installed.packages()) install_github('gousiosg/timeline')
if (!"optparse" %in% installed.packages()) install.packages("optparse")
if (!"rmarkdown" %in% installed.packages()) install.packages("rmarkdown")
if (!"foreach" %in% installed.packages()) install.packages("foreach")
if (!"doMC" %in% installed.packages()) install.packages("doMC")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"rjson" %in% installed.packages()) install.packages("rjson")
if (!"effsize" %in% installed.packages()) install.packages("effsize")
if (!"stringr" %in% installed.packages()) install.packages("stringr")
if (!"ggplot2" %in% installed.packages()) install_version("ggplot2", version = "1.0.1", repos = "http://cran.us.r-project.org/")
if (!"fastmatch" %in% installed.packages()) install.packages("fastmatch")
if (!"ggthemes" %in% installed.packages()) install_version("ggthemes", version = "2.1.0", repos = "http://cran.us.r-project.org/")
if (!"doBy" %in% installed.packages()) install.packages("doBy")
if (!"xts" %in% installed.packages()) install.packages("xts")
if (!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")
if (!"rworldmap" %in% installed.packages()) install.packages("rworldmap")
if (!"lsr" %in% installed.packages()) install.packages("lsr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"splitstackshape" %in% installed.packages()) install.packages("splitstackshape")
if (!"lmPerm" %in% installed.packages()) install.packages("lmPerm")
if (!"metap" %in% installed.packages()) install.packages("metap")
if (!"mgcv" %in% installed.packages()) install.packages("mgcv")


