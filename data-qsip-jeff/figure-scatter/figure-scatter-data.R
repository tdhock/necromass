library(data.table)
library(glmnet)
RData.vec <- Sys.glob("*.RData")
score.out.list <- list()
for(RData.i in seq_along(RData.vec)){
  comparison.RData <- RData.vec[RData.i]
  comparison <- gsub(".RData|.*-", "", comparison.RData)
  tit <- gsub("[.]", " ", comparison)
  (objs <- load(comparison.RData))
  score.dt <- mlr3resampling::score(bmr)
  score.dt[, n.test := sapply(test, length)]
  score.dt[, .(n.data=sum(n.test)), keyby=.(test.group, train.groups)]
  n.dt <- score.dt[train.groups=="same", .(
    n.data=sum(n.test)
  ), by=test.group]
  score.atomic <- score.dt[, sapply(score.dt, is.atomic), with=FALSE]
  score.out.list[[comparison]] <- data.table(comparison, score.atomic)
}
score.out <- rbindlist(score.out.list, fill=TRUE)
fwrite(score.out, "figure-scatter-data.csv")
