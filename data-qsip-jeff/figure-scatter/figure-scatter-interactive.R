library(data.table)
library(animint2)
score.out <- fread(
  "figure-scatter-data.csv"
)[, `:=`(
  comparison_subset=paste(comparison, test.group),
  log10.regr.mse = log10(regr.mse)
)][]
n.folds <- max(score.out$test.fold)
ord <- c(
  "all",
  "all-same",
  "same",
  "other-same",
  "other")
score.stats <- dcast(
  score.out,
  comparison_subset + comparison + train.groups + test.group + algorithm ~ .,
  list(mean, sd, length),
  value.var="log10.regr.mse"
)[, `:=`(
  log10.regr.mse_lo=log10.regr.mse_mean-log10.regr.mse_sd,
  log10.regr.mse_hi=log10.regr.mse_mean+log10.regr.mse_sd,
  train_subsets=factor(train.groups,ord)
)][]
range.tall <- melt(
  score.stats,
  measure.vars=c("log10.regr.mse_hi","log10.regr.mse_lo"))
range.wide <- dcast(
  range.tall,
  comparison_subset ~ .,
  list(min, max))
getNorm <- function(DT, ...){
  measure.vars <- c(...)
  range.join <- range.wide[DT, on="comparison_subset"]
  tall.join <- melt(
    range.join,
    measure.vars=measure.vars
  )[
  , norm := (value-value_min)/(value_max-value_min)
  ][]
  dcast(
    tall.join, 
    train_subsets + comparison_subset + algorithm ~ variable,
    value.var="norm")
}
wide.join <- getNorm(score.stats, "log10.regr.mse_hi","log10.regr.mse_lo","log10.regr.mse_mean")

score.wide <- dcast(
  score.out[algorithm=="cv_glmnet"],
  algorithm + comparison_subset + comparison + test.fold + test.group ~ train.groups,
  value.var="log10.regr.mse")
score.tall <- melt(
  score.wide,
  measure.vars=c("all","other"),
  variable.name="compare_name",
  value.name="compare_error")
pval.dt <- score.tall[, {
  test.res <- t.test(compare_error, same, paired=TRUE, alternative = "two.sided")
  with(test.res, data.table(
    estimate,
    p.value,
    compare_mean=mean(compare_error,na.rm=TRUE),
    same_mean=mean(same,na.rm=TRUE),
    log10.p=log10(p.value),
    N=.N))
}, by=.(
  algorithm, comparison, test.group, compare_name,
  comparison_subset,
  train_subsets=factor(paste0(compare_name,"-same"),ord)
)]
pval.join <- getNorm(pval.dt, "compare_mean", "same_mean")

ggplot()+
  geom_point(aes(
    log10.p, estimate, color=comparison),
    data=pval.dt)+
  facet_grid(. ~ compare_name)

cross.dt <- dcast(
  pval.dt,
  compare_name + comparison ~ .,
  list(min, max, mean),
  value.var=c("estimate","log10.p"))

ggplot()+
  geom_segment(aes(
    log10.p_min, estimate_mean,
    xend=log10.p_max, yend=estimate_mean,
    color=comparison),
    data=cross.dt)+
  geom_segment(aes(
    log10.p_mean, estimate_min,
    xend=log10.p_mean, yend=estimate_max,
    color=comparison),
    data=cross.dt)+
  facet_grid(. ~ compare_name)

only.same <- score.stats[train_subsets=="same"]
wide.same <- dcast(
  only.same,
  comparison + comparison_subset + test.group ~ algorithm,
  value.var="log10.regr.mse_mean")
ggplot()+
  geom_abline(aes(
    slope=slope,
    intercept=intercept),
    color="grey",
    data=data.table(slope=1, intercept=0))+
  geom_point(aes(
    cv_glmnet, featureless, color=comparison),
    data=wide.same)+
  coord_equal()+
  theme_bw()

seg.size <- 4
seg.alpha <- 0.7
vhline.color <- "grey50"
vhline.size <- 1
point.size <- 5
combine <- function(norm_dt, no_dt)rbind(
  data.table(normalization="Normalized MSE in [0,1]", norm_dt),
  data.table(normalization="No normalization", no_dt[,names(norm_dt),with=FALSE]))
mse.dt <- combine(wide.join, score.stats)
pval.both <- combine(pval.join, pval.dt)
seg.dt <- cross.dt[, data.table(compare_name, comparison, rbind(
  data.table(x=estimate_mean, y=log10.p_min, xend=estimate_mean, yend=log10.p_max),
  data.table(x=estimate_min, y=log10.p_mean, xend=estimate_max, yend=log10.p_mean)))]
viz <- animint(
  title="Accuracy of predicting qSIP data",
  source="https://github.com/tdhock/necromass/blob/main/data-qsip-jeff/figure-scatter/figure-scatter-interactive.R",
  out.dir="figure-scatter-interactive",
  overview=ggplot()+
    ggtitle("Overview, click to select test set")+
    theme_bw()+
    theme_animint(height=400, width=900)+
    theme(legend.position="none")+
    geom_vline(aes(
      xintercept=diff),
      color=vhline.color,
      size=vhline.size,
      help="Vertical line for no difference when training on same and compare subset.",
      data=data.table(diff=0))+
    geom_hline(aes(
      yintercept=log10.p),
      color=vhline.color,
      size=vhline.size,
      help="Horizontal line for traditional P-value threshold=0.05.",
      data=data.table(log10.p=log10(0.05)))+
    geom_text(aes(
      x, y, label=label),
      color=vhline.color,
      hjust=0,
      help="Text labels explain meaning of grey lines.",
      data=data.table(
        x=c(0.01, 0.5),
        y=c(-11.8, -1.1),
        label=c("No difference", "P=0.05")))+
    geom_segment(aes(
      x, y, 
      color=comparison,
      yend=yend, xend=xend),
      size=seg.size,
      clickSelects="comparison",
      alpha=seg.alpha,
      help="Segments show min/mean/max error differences across comparison subsets.",
      data=seg.dt)+
    geom_text(aes(
      estimate_mean, log10.p_mean, 
      label=comparison),
      help="Text shows comparison name.",
      showSelected="comparison",
      data=cross.dt)+
    geom_point(aes(
      estimate, log10.p, color=comparison),
      help="One dot for each comparison subset in selected comparison.",
      showSelected="comparison",
      clickSelects="comparison_subset",
      alpha=1,
      alpha_off=0.1,
      size=point.size,
      data=pval.dt)+
    geom_point(aes(
      estimate, log10.p,
      key=1,
      color=comparison),
      help="One dot for selected comparison subset.",
      showSelected="comparison_subset",
      clickSelects="comparison",
      alpha=1,
      alpha_off=0.5,
      size=point.size,
      data=pval.dt)+
    geom_text(aes(
      estimate, log10.p, label=test.group),
      help="Text shows comparison subset names.",
      clickSelects="comparison_subset",
      showSelected="comparison",
      data=pval.dt)+
    scale_x_continuous(
      "Error difference, cv_glmnet trained on same and compare subset")+
    scale_y_continuous(
      "log10(P-value), two-sided paired t-test")+
    facet_grid(. ~ compare_name, labeller=label_both),
  detailsFacet=ggplot()+
    ggtitle("Log MSE for selected test set")+
    theme_bw()+
    theme_animint(height=250, width=650)+
    scale_y_discrete(drop=FALSE)+
    geom_segment(aes(
      compare_mean, train_subsets,
      key=train_subsets,
      xend=same_mean, yend=train_subsets),
      help="Red segments show differences between same and other/all.",
      color="red",
      showSelected="comparison_subset",
      data=pval.both)+
    scale_x_continuous(
      "log10(Mean Squared Error)")+
    geom_text(aes(
      x=-3.5, y=train_subsets,
      key=train_subsets,
      label=sprintf(
        "Diff Log MSE %.4f %s",
        compare_mean-same_mean,
        ifelse(
          p.value<0.0001,
          "p<0.0001",
          sprintf("p=%.4f", p.value)))),
      help="Text shows error difference and P-value in two-sided paired t-test.",
      showSelected="comparison_subset",
      data=data.table(normalization="No normalization", pval.dt))+
    geom_segment(aes(
      log10.regr.mse_lo, train_subsets,
      key=train_subsets,
      xend=log10.regr.mse_hi, yend=train_subsets),
      showSelected="comparison_subset",
      help=paste(
        "Black segments show mean +/- SD of test MSE, over",
        n.folds,
        "train/test splits."),
      data=mse.dt)+
    facet_grid(algorithm ~ normalization, scales="free")+
    geom_point(aes(
      log10.regr.mse_mean, train_subsets,
      key=train_subsets),
      showSelected="comparison_subset",
      help=paste(
        "Black dot for mean of test MSE, over",
        n.folds,
        "train/test splits."),
      fill=NA,
      data=mse.dt),
  absErr=ggplot()+
    ggtitle("Log MSE, train=same")+
    geom_abline(aes(
      slope=slope,
      intercept=intercept),
      help="Diagonal line shows equality between cv_glmnet and featureless.",
      color="grey",
      data=data.table(slope=1, intercept=0))+
    geom_point(aes(
      cv_glmnet, featureless, fill=comparison),
      clickSelects="comparison_subset",
      alpha_off=0.3,
      alpha=1,
      help="One dot for each comparison subset.",
      size=point.size,
      color="black",
      color_off=NA,
      data=wide.same)+
    coord_equal()+
    theme_bw()+
    theme_animint(height=250, width=250)+
    theme(legend.position="none"),
  video="https://vimeo.com/1052541453",
  selector.types=list(
    comparison="single"),
  duration=list(
    comparison_subset=1000)
)
viz
if(FALSE){
  animint2pages(viz, "2025-02-01-error-predict-qsip")
}
