library(data.table)
library(ggplot2)
library(glmnet)
RData.vec <- Sys.glob("*.RData")
for(RData.i in seq_along(RData.vec)){
  comparison.RData <- RData.vec[RData.i]
  comparison <- gsub(".RData|.*-", "", comparison.RData)
  tit <- gsub("[.]", " ", comparison)
  (objs <- load(comparison.RData))
  score.dt <- mlr3resampling::score(bmr)
  score.dt[1]
  glmnet.dt <- score.dt[algorithm=="cv_glmnet"]
  weight.dt.list <- list()
  for(glmnet.i in 1:nrow(glmnet.dt)){
    learner <- glmnet.dt$learner[[glmnet.i]]
    weight.mat <- coef(learner$state$model)
    weight.dt.list[[glmnet.i]] <- data.table(
      glmnet.dt[glmnet.i, .(test.group, test.fold, train.groups)],
      coef=as.numeric(weight.mat),
      variable=rownames(weight.mat)
    )[coef != 0 & variable != "(Intercept)"]
  }
  weight.dt <- rbindlist(weight.dt.list)
  (weights.csv <- sub("RData", "weights.csv", comparison.RData))
  fwrite(weight.dt, weights.csv)
  gg <- ggplot()+
    geom_point(aes(
      regr.mse, train.groups, color=algorithm),
      shape=1,
      data=score.dt)+
    facet_grid(test.group ~ ., scales="free")+
    scale_x_log10()
  score.dt[, `:=`(
    "Test group" = gsub("_", "\n", paste0("\n", test.group)),
    "Train\ngroups"=paste0("\n",train.groups)
  )]
  gg <- ggplot()+
    ggtitle(tit)+
    theme(axis.text.x=element_text(angle=30, hjust=1))+
    geom_point(aes(
      regr.mse, algorithm),
      shape=1,
      data=score.dt)+
    facet_grid(
      `Train\ngroups` ~ `Test group`,
      scales="free",
      labeller=label_both)+
    scale_x_log10("Mean squared prediction error (test set)")
  train.list <- list(
    same="same",
    other=c("same","other"),
    all=c("same","other","all"))
  for(suffix in names(train.list)){
    some.scores <- score.dt[train.groups %in% train.list[[suffix]] ]
    gg <- ggplot()+
      ggtitle(tit)+
      theme(axis.text.x=element_text(angle=30, hjust=1))+
      geom_point(aes(
        regr.mse, algorithm),
        shape=1,
        data=some.scores)+
      geom_blank(aes(
        regr.mse, algorithm),
        data=score.dt)+
      facet_grid(
        `Train\ngroups` ~ `Test group`,
        scales="free",
        labeller=label_both)+
      scale_x_log10("Mean squared prediction error (test set)")
    n.test <- length(unique(score.dt$test.group))
    (comparison.png <- sub("RData", paste0(suffix, ".png"), comparison.RData))
    png(comparison.png, height=3, width=(n.test+1)*1.5, units="in", res=200)
    print(gg)
    dev.off()
  }
}
weight.dt <- fread(
  "qsip_pc2_all_new-controls.between.experiments.weights.csv"
)[, `:=`(
  n.nonzero=.N,
  mean.coef=mean(coef),
  experiment=test.group
), by=.(test.group, train.groups, variable)]
weight.same <- weight.dt[
  train.groups=="same"
][
  order(-n.nonzero, mean.coef)
]
always.used <- weight.same[n.nonzero == 10, unique(variable)]
weight.same[, var.fac := factor(variable, always.used)]
weight.show <- weight.same[!is.na(var.fac)]
text.show <- unique(weight.show[, .(
  test.group, experiment, var.fac, n.nonzero)])
gg <- ggplot()+
  theme(panel.spacing=grid::unit(2, "lines"))+
  geom_text(aes(
    ifelse(test.group=="dim", Inf, -Inf),
    var.fac,
    label=n.nonzero,
    hjust=ifelse(test.group=="dim", 1, 0)),
    data=text.show)+
  geom_point(aes(
    coef, var.fac),
    shape=1,
    data=weight.show)+
  facet_grid(. ~ experiment, labeller=label_both)+
  xlab("Linear model weight/coefficient")+
  ylab("Gene with non-zero linear model weight,
for all 10 train/test splits in one experiment")
png(
  "qsip_pc2_all_new-controls.between.experiments.weights.png",
  height=6, width=7, units="in", res=200)
print(gg)
dev.off()
system("cd /projects/genomic-ml/ && unpublish_data necromass && publish_data necromass")


## TODO compute p-value for difference between cv_glmnet and featureless.
