rm(list = ls(all = TRUE))

source('descr-stats-table.R')

student.project.pids <- c(
  "cc6a954bfddeb55600a8e4e6eee4bb1cb48a8c63",
  "9542b3c91d5d749f66765631ca89518827aca09f",
  "09d5d532f5ef51b6b7551e25c2697290697098cd",
  "8cd4cdf32d2993bf8a3f7a336b1e42b3c879d66a",
  "b9d6dc128fb5a78ab2608a678d58b6369f1599cb",
  "bd187d3e05553beadba611b0a023d78363369062",
  "c56dc5303798b768e866136015f5f174f14b9b75",
  "2d1817e3ad311e2ef6abb2c2be1fcbd64ee0cf66",
  "1bc497352064103c4ef2043248c2d2c9619f1a51",
  "057682c10a6a188cb5ba7c91e93b89628c010101",
  "df7ef19ca1fbd64a61d7451f1b805156cb1aaeac",
  "83931b55e0ad51389fdd2383d072ff9f0f5f28e5",
  "119baa56e66ffea5a0027f9ae889989f4fd34704",
  "1a026be58825006c70666ac6cd4d17e5e0581287",
  "617b0e3d0b23fa96b43cc4676c930c37e9dce6da",
  "fdd8ad03a365499e12aad19e1167504ba2a82873",
  "3017c60d6193f7e12938e95d3e9f3b1bbf7cdc93",
  "f6e66338ce26b4e66e303ac3a0b4e916b210b86f",
  "c396c64b39bfb68ae17f016cdc22672fc103b0da",
  "0421475d88d547f81497260ec5e6920474ecd5db",
  "2d4f4800e856afe5d12d5e7a7047cf33488e2949",
  "947aa04afad66750d72ad374c418f2c74c24937d",
  "beefb344c24c95ac81882aeb3a686a3345b8f4aa",
  "856b90f17e4400ffd0a42a84ac72fed82e3cede4",
  "13bbe6edd49bf850a450f866951f90ead67b7e50",
  "db78ea4a34e0dd20bdd9ca614b193a390a99dac0",
  "3a43e255a80da4b2c12913614a4258c356dcf7ee",
  "c1c9a82479da3f20342e2f784091e81e9a3b1e3e",
  "19e3f7d5f64ddab75678a8de1200242895d928e6",
  "799530bf7d7bc5a5596587641cb16aa69ad91fa5",
  "ce7aad6aa5c6065c15aef5532f1881ce2cb8645f",
  "b7a935ebe04435b07f79e372c2c718814a9dcea9",
  "bffe49a642ef3de38aa37a1ae5c269de37749caa",
  "08fc3a704d1615841eb52f7c9f5631a62169aadb",
  "be6f960f6c71e676e0b148d1c12b51055ef1eacb",
  "6201b7887526c5092edbcf2884667c31e558501d",
  "5dfcda8b41a972e491450e63c673d369fb969076",
  "d745254141b38a2fb10f8c291fb1a9019f07d64a",
  "6562dc3701f3bfa9400683ae7e3a60708795e7d8",
  "a39b2ac7ce637f43555437c620a89fbb297aefd9",
  "d72989f9312efb72c1f01119fb49c0b3a6e1bc83",
  "b168b3b1ae5208c5dcefd5c8fde63c12a796ff4a",
  "ead0879179a10510a9f8fd1548d4cab21035aee3",
  "a36427bca31f7657a65676323364ba7ed63fd1f2"
)

student.stats.table <- data.frame(
  Feature = c(
    'num.intervals',
    'total.time',
    'avg.per.day',
    'active.eclipse.usage',
    'perc.time.spent.reading',
    'perc.time.spent.typing',
    'time.remaining',
    'perc.time.java.read',
    'perc.time.java.write',
    'time.debugging',
    'perc.time.prod.est',
    'perc.time.prod',
    'perc.time.test.est',
    'perc.time.test',
    'num.test.executions',
    'num.test.failures',
    'avg.test.duration'
  ),
  Description = c(
    'Recorded intervals',
    'Total recorded time',
    'Average recorded time', 
    'Active Eclipse usage time',
    'Reading time',
    'Writing time',
    'Remaining time',
    'Java reading time',
    'Java writing time', 
    'Debugging time',
    'Production code time (estimate)',
    'Production code time (actual)',
    'Test code time (estimate)',
    'Test code time (actual)',
    'Test executions',
    'Test failures',
    'Average test duration'
  ),
  Unit = c(
    'count',
    'hours',
    'hours / day',
    'hours',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '\\%',
    '1 / day',
    '1 / day',
    'seconds'
  )
)

base.dir        <- '..'
plot.location   <- file.path(base.dir, "figs")
latex.location  <- file.path(base.dir, ".")

dir.create(plot.location, showWarnings = F, recursive = T)
dir.create(latex.location, showWarnings = F, recursive = T)

if (!file.exists('student-data.csv')) {
  # Need to import the data analysis code here
  source('project-report.R')

  all.projects <- projects.with.intervals(mongo.conn = mongo.connection())
  student.projects.1000.intervals <- intersect(all.projects, student.project.pids)
  all.project.report.values(pids = student.projects.1000.intervals,
                            cache.file = 'student-data.csv')
}

student.data <- read.csv('student-data.csv')
total.work.time <- sum(student.data$total.time) / 1380 #Dutch work time per year

stats.table(student.data, student.stats.table,
            caption = 'Descriptive statistics for all projects (per project)',
            fname = 'descr-stats.tex',
            label = 'tab:descr-stats')

cor.test(student.data$num.test.executions, student.data$avg.test.duration,
         method = "spearman")
