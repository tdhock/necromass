library(data.table)
data.list <- list()
cons.csv.vec <- Sys.glob("data-2023-12-22/*.csv")
for(cons.csv in cons.csv.vec){
  king.dt <- nc::capture_first_vec(cons.csv, "/Dec22_", kingdom="[^_]+")
  data.list[[king.dt$kingdom]] <- fread(cons.csv)[order(Necrobag_ID)]
}
str(data.list)
names.list <- lapply(data.list, names)
sapply(data.list, dim)
names.meta <- do.call(intersect, unname(names.list))
cat(names.meta, sep=", ")
do.call(identical, unname(lapply(data.list, "[[", "Necrobag_ID")))
meta.long.list <- list()
for(kingdom in names(data.list)){
  king.full.dt <- data.list[[kingdom]]
  king.meta.dt <- king.full.dt[,names.meta,with=FALSE]
  meta.long.list[[kingdom]] <- data.table(kingdom, king.meta.dt)
}
meta.long <- rbindlist(meta.long.list)
meta.longer <- melt(meta.long, id=c("kingdom", "Necrobag_ID"))
meta.wide <- dcast(meta.longer, Necrobag_ID + variable ~ kingdom)
(question <- meta.wide[bacteria != fungi])

meta.names <- c(
  "Necrobag_ID", "sample_ID", "Domain", "Habitat", "Melanization", 
  "Incubation_time", "Plot", "Comp", "OTU_ID")
necro.dt.list <- list()
for(data.name in names(data.list)){
  meta.and.data <- data.list[[data.name]]
  is.meta <- names(meta.and.data) %in% meta.names
  meta.dt <- meta.and.data[, is.meta, with=FALSE]
  necro.dt.list[[data.name]] <- meta.and.data[, !is.meta, with=FALSE]
}
(necro.dt <- do.call(data.table, necro.dt.list))

necro.tall <- melt(necro.dt, measure.vars=names(necro.dt))
library(ggplot2)
ggplot()+
  geom_histogram(aes(
    value),
    data=necro.tall)+
  facet_wrap(~variable)

ggplot()+
  geom_histogram(aes(
    log10(value+1)),
    data=necro.tall)+
  facet_wrap(~variable)

log.necro.dt <- log10(necro.dt+1)

same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
meta.dt[, Samples := ifelse(Habitat=="soil", "Habitat=soil", paste0("Melanization=",Melanization))][, table(Samples)]
task.list <- list()
for(out.i in 1:ncol(log.necro.dt)){
  task.dt <- data.table(log.necro.dt, Samples=meta.dt$Samples)
  task_id <- names(log.necro.dt)[[out.i]]
  task.list[[task_id]] <- mlr3::TaskRegr$new(
    task_id, task.dt, target=task_id
  )$set_col_roles("Samples",c("group","stratum"))
}

cvg <- mlr3learners::LearnerRegrCVGlmnet$new()
cvg$param_set$values$nfolds=5
(reg.learner.list <- list(
  cvg,
  mlr3::LearnerRegrFeatureless$new()))

(reg.bench.grid <- mlr3::benchmark_grid(
  task.list,
  reg.learner.list,
  same_other_cv))

(debug.grid <- mlr3::benchmark_grid(
  task.list["bacteria.Kaistia"],
  reg.learner.list,
  same_other_cv))
future::plan("sequential")
debug.result <- mlr3::benchmark(debug.grid)

if(FALSE){
  future::plan(
    future.batchtools::batchtools_slurm,
    template="~/slurm-future.tmpl",
    resources=list(
      walltime=60*60,#seconds as defined https://mllg.github.io/batchtools/reference/submitJobs
      memory=1000,
      ncpus=1,
      ntasks=1,
      chunks.as.arrayjobs=TRUE))
  ## mlr3::benchmark uses future_map defined in https://github.com/mlr-org/mlr3/blob/545873fbdd7a55be9ca74ef5b264ddaca0de2f8e/R/helper_exec.R#L25 which uses future.apply::future_mapply
  future.apply::future_mapply(function(i)Sys.sleep(60), 1:100)
  (reg.bench.result <- mlr3::benchmark(
    reg.bench.grid, store_models = TRUE))
}

reg.dir <- "data-2023-12-22-benchmark-reg"
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
  walltime = 60*60,#seconds
  memory = 2000,#megabytes per cpu
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
