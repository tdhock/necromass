library(data.table)
qsip.dt <- fread("qsip_pc2_all_new.csv")
dim(qsip.dt)
head(names(qsip.dt), 30)
qsip.dt[1,1:30]
qsip.dt[, .(rows=.N), by=experiment]
qsip.dt[, .(rows=.N), by=site]
qsip.dt[, .(rows=.N), by=treatment]
qsip.dt[, .(rows=.N), by=time]
qsip.dt[, .(rows=.N), by=isotope]#2 groups.

hist(qsip.dt$eaf)
hist(qsip.dt$growth_per_day)
range(qsip.dt$growth_per_day)
hist(log10(qsip.dt$growth_per_day+1))
plot(growth_per_day ~ eaf, qsip.dt)

## Yes, KO numbers are the genes.

## The "growth_per_day" column is the output. Eaf is there, but we normalized by maximum isotope enrichment and the incubation length.

## Yes, I left a lot of metadata there in case we want to explore patterns with some of the taxonomic or functional categories. I guess we can ignore for now.
 
## There are several experiments that occurred in the same system multiple times. For example, MC (mixed conifer) is the Friends & Foes soil, so comparing qme MC controls and dim MC controls would probably have good prediction accuracy. Note that qme experiment doesn't have "controls" but incubation temperatures of 5 to 35 degrees. The dim experiment was performed at room temperature, so we could compare it to the 15 degree treatment.
if(FALSE){
qsip.dt[, .(rows=.N), by=.(site,experiment,treatment)]
(select.dt <- data.table(site="MC", rbind(
  data.table(experiment="dim", treatment="control"),
  data.table(experiment="qme", treatment="15"))))
dim.qme.controls <- qsip.dt[select.dt, on=.(site,experiment,treatment)]
## I think comparing controls to carbon additions (C, CN, detritus, rhizo, cellulose, etc.) would be interesting and would likely have bad prediction accuracy. We are also interested in differences across systems, so comparing controls between sites would be great, too.
control.only <- qsip.dt[treatment=="control"]
control.only[, .(rows=.N), by=site]
qsip.dt[, my.trt := ifelse(experiment=="drp", site, treatment)]
qsip.dt[
  my.trt %in% c("C","CN","Detritus","Rhizo","Cellulose","control"),
  .(rows=.N),
  by=.(experiment, site, treatment, my.trt)]
}
## TODO Another thought... maybe the same substrate added to different soils elicits a predictable response. Within the "dim" experiment, the exact same C was added to all sites along an elevation gradient, so maybe comparing the most similar sites given the same C amendments would have less error, so:
## dim MC +C <=predicts=> dim PP +C
## dim PP +C <= predicts => dim PJ +C
## dim PJ +C <= predicts => dim GL +C
## (Or same but with +CN)
## When looking at the CN additions (2nd figure), training on all, same or different sites are all strongly predictive, and, again, the training on all is sometimes stronger. This seems like it can be interpreted as a consistent pattern in response to CN additions across soils! The C additions show a similar pattern (3rd figure), but not as pronounced.
## This is also true, but not as pronounced, in the SRO exudate (slightly more complex C & N) additions (4th figure), although the "other" trained model looks slightly less predictive. Litter (complex C & N) addition tended to have the same pattern (5th figure). Like DIM, a comparison of SRO controls also shows little predictability across sites (6th figure).
compare <- function(comparison, group.var, DT){
  data.table(comparison, group.var, DT=list(DT))
}
dim.only <- qsip.dt[experiment=="dim"]
dim.only[, table(site, treatment)]
comparison.dt <- rbind(
  ## compare("controls.between.sites", "site", control.only),
  ## compare("control.vs.carbon.additions", "treatment", qsip.dt[treatment %in% c("C","CN","control")]),
  ## compare("controls.between.experiments", "experiment",dim.qme.controls),
  compare("dim.C.between.sites", "site", qsip.dt[experiment=="dim" & treatment=="C"]),
  compare("dim.CN.combine.sites", "site", qsip.dt[experiment=="dim" & treatment=="CN"]),
  compare("dim.compare.sites", "site", dim.only),
  compare("dim.compare.treatments", "treatment", dim.only))
comparison.dt
length(gene.names <- grep("^K", names(qsip.dt), value=TRUE))
if(FALSE){
  gene.dt <- qsip.dt[, gene.names, with=FALSE]
  gene.tab <- table(as.matrix(gene.dt[1:100]))
  str(gene.tab)
  gene.range.mat <- sapply(gene.dt, range)
  table(gene.range.mat)
}

for(comparison.i in 1:nrow(comparison.dt)){
  compare.row <- comparison.dt[comparison.i]
  print(compare.row$DT[[1]][, .(rows=.N), by=eval(compare.row$group.var)])
}


for(comparison.i in 1:nrow(comparison.dt)){
  compare.row <- comparison.dt[comparison.i]
  task.names <- c(
    compare.row$group.var,
    "growth_per_day",
    gene.names)
  same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
  same_other_cv$param_set$values$folds <- 10
  task.dt <- data.table(compare.row$DT[[1]][, task.names, with=FALSE])
  task_id <- paste0("qsip_pc2_all_new_", compare.row$comparison)
  task <- mlr3::TaskRegr$new(
    task_id, task.dt, target="growth_per_day"
  )$set_col_roles(compare.row$group.var,c("group","stratum"))
  cvg <- mlr3learners::LearnerRegrCVGlmnet$new()
  cvg$param_set$values$nfolds=5
  (reg.learner.list <- list(
    cvg,
    mlr3::LearnerRegrFeatureless$new()))
  (reg.bench.grid <- mlr3::benchmark_grid(
    task,
    reg.learner.list,
    same_other_cv))
  reg.dir <- paste0("qsip_pc2_all_new-", compare.row$comparison)
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
    memory = 64000,#megabytes per cpu
    ncpus=1,  #>1 for multicore/parallel jobs.
    ntasks=1, #>1 for MPI jobs.
    chunks.as.arrayjobs=TRUE), reg=reg)
}

for(comparison.i in 1:nrow(comparison.dt)){
  compare.row <- comparison.dt[comparison.i]
  reg.dir <- paste0("qsip_pc2_all_new-", compare.row$comparison)
  reg=batchtools::loadRegistry(reg.dir)
  print(batchtools::getStatus(reg=reg))
  if(TRUE){
    jobs.after <- batchtools::getJobTable(reg=reg)
    table(jobs.after$error)
    ids <- jobs.after[is.na(error), job.id]
    bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg)
    out.RData <- paste0(reg.dir, ".RData")
    save(bmr, file=out.RData)
  }
}

