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
  score.dt[, n.test := sapply(test, length)]
  score.dt[, .(n.data=sum(n.test)), keyby=.(test.group, train.groups)]
  n.dt <- score.dt[train.groups=="same", .(
    n.data=sum(n.test)
  ), by=test.group]
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
  add_facet_vars <- function(DT){
    DT[, `:=`(
      "Test group" = gsub("_", "\n", paste0("\n", test.group)),
      "Train\ngroups"=paste0("\n",train.groups)
    )][
      n.dt,
      Rows := n.data,
      on="test.group"
    ]
  }
  add_facet_vars(score.dt)
  p.color <- "red"
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
  score.stats <- dcast(
    score.dt,
    train.groups + test.group + algorithm ~ .,
    list(mean, sd, length),
    value.var="regr.mse")
  add_facet_vars(score.stats)
  score.wide <- dcast(
    score.dt[, log10.regr.mse := log10(regr.mse)],
    test.fold + train.groups + test.group ~ algorithm,
    value.var="log10.regr.mse")
  featureless.p.values <- score.wide[, {
    test.res <- t.test(
      featureless,
      cv_glmnet,
      alternative = "greater",
      paired=TRUE)
    with(test.res, data.table(p.value, estimate))
  }, by=.(train.groups,train.groups,test.group)]
  score.wide.train <- dcast(
    score.dt[algorithm=="cv_glmnet"],
    test.fold + test.group ~ train.groups,
    value.var="log10.regr.mse")
  score.wide.train.compare <- melt(
    score.wide.train,
    measure.vars=c("all","other"),
    variable.name="train.groups")
  min.max.dt <- dcast(
    score.dt,
    test.group ~ .,
    list(min, max),
    value.var="log10.regr.mse"
  )[, log10.regr.mse_mid := (log10.regr.mse_min+log10.regr.mse_max)/2]
  glmnet.same <- score.stats[
    algorithm=="cv_glmnet" & train.groups=="same"
  ][
  , same_mean := regr.mse_mean
  ][]
  vline.dt <- glmnet.same[
  , .(same_mean, `Test group`, Rows)]
  train.p.values <- score.wide.train.compare[, {
    test.res <- t.test(
      value,
      same,
      alternative = "two.sided",
      paired=TRUE)
    with(test.res, data.table(algorithm="cv_glmnet", p.value, estimate))
  }, by=.(train.groups,test.group)
  ][
    score.stats, on=c("train.groups","test.group","algorithm"), nomatch=0L
  ][
    min.max.dt, on=c("test.group"), nomatch=0L
  ][
    glmnet.same[, .(same_mean, test.group)], on=c("test.group")
  ]
  print(train.p.values[order(p.value), .(train.groups, test.group, p.value, estimate)])
  train.list <- list(
    same="same",
    other=c("same","other"),
    all=c("same","other","all"))
  for(suffix in names(train.list)){
    some <- function(DT){
      DT[train.groups %in% train.list[[suffix]] ]
    }
    some.scores <- some(score.dt)
    some.stats <- some(score.stats)
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
    gg <- ggplot()+
      ggtitle(tit)+
      theme_bw()+
      theme(
        panel.spacing=grid::unit(0, "lines"),
        axis.text.x=element_text(angle=30, hjust=1))+
      geom_vline(aes(
        xintercept=same_mean),
        data=vline.dt,
        color="grey50")+
      geom_text(aes(
        regr.mse_mean, algorithm,
        hjust=ifelse(
          log10(regr.mse_mean)<log10.regr.mse_mid,
          0, 1),
        label=ifelse(
          p.value<0.0001,
          "p<0.0001",
          sprintf("p=%.4f",p.value))),
        color=p.color,
        vjust=-0.8,
        size=3,
        data=some(train.p.values))+
      geom_point(aes(
        regr.mse_mean, algorithm),
        shape=1,
        data=some.stats)+
      geom_segment(aes(
        regr.mse_mean+regr.mse_sd, algorithm,
        xend=regr.mse_mean-regr.mse_sd, yend=algorithm),
        linewidth=1,
        data=some.stats)+
      geom_segment(aes(
        same_mean, algorithm,
        xend=regr.mse_mean, yend=algorithm),
        data=some(train.p.values),
        color=p.color)+
      geom_blank(aes(
        regr.mse, algorithm),
        data=score.dt)+
      facet_grid(
        `Train\ngroups` ~ Rows + `Test group`,
        scales="free",
        labeller=label_both)+
      scale_x_log10(
        "Mean squared prediction error on test set\n(mean +/- SD over 10 folds, log scale, paired t-test in red)")
    print(comparison.png <- sub("RData", paste0(suffix, "-stats.png"), comparison.RData))
    png(comparison.png, height=3.5, width=(n.test+1)*1.5, units="in", res=200)
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
