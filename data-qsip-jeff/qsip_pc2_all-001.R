library(data.table)
qsip.dt <- fread("qsip_pc2_all-001.csv")
dim(qsip.dt)
head(names(qsip.dt), 30)
qsip.dt[1,1:30]
qsip.dt[, .(count=.N), by=experiment]
qsip.dt[, .(count=.N), by=site]
qsip.dt[, .(count=.N), by=treatment]
qsip.dt[, .(count=.N), by=time]
qsip.dt[, .(count=.N), by=isotope]#2 groups.
hist(qsip.dt$eaf)
task.names <- c("isotope", "eaf", grep("^K", names(qsip.dt), value=TRUE))

same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
task.dt <- data.table(qsip.dt[, task.names, with=FALSE])
task <- mlr3::TaskRegr$new(
  "qsip_pc2_all-001", task.dt, target="eaf"
)$set_col_roles("isotope",c("group","stratum"))
cvg <- mlr3learners::LearnerRegrCVGlmnet$new()
cvg$param_set$values$nfolds=5
(reg.learner.list <- list(
  cvg,
  mlr3::LearnerRegrFeatureless$new()))

(reg.bench.grid <- mlr3::benchmark_grid(
  task,
  reg.learner.list,
  same_other_cv))

(debug.grid <- mlr3::benchmark_grid(
  task.list["bacteria.Kaistia"],
  reg.learner.list,
  same_other_cv))
future::plan("sequential")
debug.result <- mlr3::benchmark(debug.grid)


reg.dir <- "qsip_pc2_all-001-reg"
unlink(reg.dir, recursive=TRUE)
reg = batchtools::makeExperimentRegistry(
  file.dir = reg.dir,
  seed = 1,
  packages = "mlr3verse"
)
mlr3batchmark::batchmark(
  reg.bench.grid, store_models = TRUE, reg=reg)
job.table <- batchtools::getJobTable(reg=reg)
chunks <- data.frame(job.table, chunk=1)
batchtools::submitJobs(chunks, resources=list(
  walltime = 24*60*60,#seconds
  memory = 32000,#megabytes per cpu
  ncpus=1,  #>1 for multicore/parallel jobs.
  ntasks=1, #>1 for MPI jobs.
  chunks.as.arrayjobs=TRUE), reg=reg)
batchtools::getStatus(reg=reg)

jobs.after <- batchtools::getJobTable(reg=reg)
table(jobs.after$error)
jobs.after[!is.na(error), .(error, task_id=sapply(prob.pars, "[[", "task_id"))][25:26]
                                                                                 
## 1:                                            Error in approx(lambda, seq(lambda), sfrac) : \n  need at least two non-NA values to interpolate
## 2: Error in elnet(xd, is.sparse, y, weights, offset, type.gaussian, alpha,  : \n  y is constant; gaussian glmnet fails at standardization step
##    task_id
## 1: Kaistia
## 2: Kaistia


ids <- jobs.after[is.na(error), job.id]
bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg)
save(bmr, file="data-2023-12-22-benchmark.RData")
