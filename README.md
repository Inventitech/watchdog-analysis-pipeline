### Analysis scripts for the watchdog MongoDB database

In order to run the R analyses in this repository, you must complete the following steps, detailed later in the Readme.

1. Required: All collections are in a local or remote mongo db
2. Install all R dependencies by running packages.R
3. Create config.R that contains connection information to mongodb*
4. Execute generate scripts, generate (in this order) users.csv, projects.csv, intervals.csv and sequences.csv 
5. Run research analysis: Execute script batch_start.R

#### Install R dependencies
To initialize R with the required library dependencies, do the following:

```bash
cd R
R --no-save < src/packages.R
```

#### Setup Mongo connection for R
You setup the conneciton to your Mongo databse via files `R/config.R` and `R/passwords.R`.

```R
mongo.user    <- "watchdog"
mongo.passwd  <- "watchdog"
mongo.host    <- "127.0.0.1"
num.processes <- 2
base.dir      <-  "."
```

If you do not run MongoDB locally, you can open an ssh tunnel to
`dutiap.st.ewi.tudelft.nl` as follows:

```bash
ssh -L 27017:dutiap:27017 dutiap
```

#### Execute generate scripts
 
We will first need to generate the CSV files that our R analysis can read-in from a Mongo database. You need to setup the connection to the mongo database (see previous step). This can either be local or remote. The generation code is meant to be run with Rscript as follows:

```R
Rscript src/generate-watchdog-csv.R [options] cmd
```

At the moment, the following `cmd`s exist (run them in this order)

* `gen-users-file`: Create a CSV file with all registered users.
* `gen-projects-file`: Create a CSV file with all registered projects.
* `gen-intervals-file`: Create a CSV file with all intervals flattened.
* `gen-sequence-file`: Create a CSV file with all intervals sequentialized for TDD analysis.
* `gen-reports` (optional): Generate test reports for all projects

The following `options` exist:
`-n x`: The number of processors to use for this analysis (default: 2). This makes the execution a lot faster on multi-core machines.

#### Run analysis
Start via `batch_start.R`. Analysis profits from multi-core machines. To run this from the command-line (and not interactively from Rstudio), do
`R --no-save <src/batch-start.R  2>&1 |tee out.log`

