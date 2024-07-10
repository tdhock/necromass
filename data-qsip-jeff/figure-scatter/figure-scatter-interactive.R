library(data.table)
library(animint2)
score.out <- fread(
  "figure-scatter-data.csv"
)[, `:=`(
  comparison_group=paste(comparison, test.group),
  log10.regr.mse = log10(regr.mse)
)][]
ord <- c(
  "all",
  "all-same",
  "same",
  "other-same",
  "other")
score.stats <- dcast(
  score.out,
  comparison_group + comparison + train.groups + test.group + algorithm ~ .,
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
  comparison_group ~ .,
  list(min, max))
getNorm <- function(DT, ...){
  measure.vars <- c(...)
  range.join <- range.wide[DT, on="comparison_group"]
  tall.join <- melt(
    range.join,
    measure.vars=measure.vars
  )[
  , norm := (value-value_min)/(value_max-value_min)
  ][]
  dcast(
    tall.join, 
    train_subsets + comparison_group + algorithm ~ variable,
    value.var="norm")
}
wide.join <- getNorm(score.stats, "log10.regr.mse_hi","log10.regr.mse_lo","log10.regr.mse_mean")

score.wide <- dcast(
  score.out[algorithm=="cv_glmnet"],
  algorithm + comparison_group + comparison + test.fold + test.group ~ train.groups,
  value.var="log10.regr.mse")
score.tall <- melt(
  score.wide,
  measure.vars=c("all","other"),
  variable.name="compare_name",
  value.name="compare_error")
pval.dt <- score.tall[, {
  test.res <- t.test(compare_error, same, paired=TRUE)
  with(test.res, data.table(
    estimate,
    p.value,
    compare_mean=mean(compare_error,na.rm=TRUE),
    same_mean=mean(same,na.rm=TRUE),
    log10.p=log10(p.value),
    N=.N))
}, by=.(
  algorithm, comparison, test.group, compare_name,
  comparison_group,
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
  comparison + comparison_group + test.group ~ algorithm,
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
      data.table(diff=0))+
    geom_hline(aes(
      yintercept=log10.p),
      color=vhline.color,
      size=vhline.size,
      data.table(log10.p=log10(0.05)))+
    geom_segment(aes(
      estimate_mean, log10.p_min, 
      color=comparison,
      yend=log10.p_max, xend=estimate_mean),
      size=seg.size,
      clickSelects="comparison",
      alpha=seg.alpha,
      data=cross.dt)+
    geom_segment(aes(
      estimate_min, log10.p_mean, 
      color=comparison,
      yend=log10.p_mean, xend=estimate_max),
      size=seg.size,
      clickSelects="comparison",
      alpha=seg.alpha,
      data=cross.dt)+
    geom_text(aes(
      estimate_mean, log10.p_mean, 
      label=comparison),
      showSelected="comparison",
      data=cross.dt)+
    geom_point(aes(
      estimate, log10.p, color=comparison),
      showSelected="comparison",
      clickSelects="comparison_group",
      alpha=1,
      alpha_off=0.1,
      size=point.size,
      data=pval.dt)+
    geom_text(aes(
      estimate, log10.p, label=test.group),
      clickSelects="comparison_group",
      showSelected="comparison",
      data=pval.dt)+
    scale_x_continuous(
      "Error difference, cv_glmnet trained on same and compare subset")+
    scale_y_continuous(
      "log10(P-value), two-sided paired t-test")+
    facet_grid(. ~ compare_name, labeller=label_both),
  details=ggplot()+
    ggtitle("Log MSE for selected test set")+
    theme_bw()+
    theme_animint(height=250)+
    scale_y_discrete(drop=FALSE)+
    geom_segment(aes(
      compare_mean, train_subsets,
      xend=same_mean, yend=train_subsets),
      color="red",
      showSelected="comparison_group",
      data=pval.dt)+
    scale_x_continuous(
      "log10(Mean Squared Error)")+
    geom_text(aes(
      x=-3.5, y=train_subsets,
      label=sprintf(
        "Diff Log MSE %.4f %s",
        compare_mean-same_mean,
        ifelse(
          p.value<0.0001,
          "p<0.0001",
          sprintf("p=%.4f", p.value)))),
      showSelected="comparison_group",
      data=pval.dt)+
    geom_segment(aes(
      log10.regr.mse_lo, train_subsets,
      xend=log10.regr.mse_hi, yend=train_subsets),
      showSelected="comparison_group",
      data=score.stats)+
    facet_grid(algorithm ~ .)+
    geom_point(aes(
      log10.regr.mse_mean, train_subsets),
      showSelected="comparison_group",
      fill=NA,
      data=score.stats),
  detailsNorm=ggplot()+
    ggtitle("Norm MSE for selected test set")+
    theme_bw()+
    theme_animint(height=250)+
    scale_y_discrete(drop=FALSE)+
    scale_x_continuous(
      "Normalized log Mean Squared Error")+
    geom_segment(aes(
      compare_mean, train_subsets,
      xend=same_mean, yend=train_subsets),
      color="red",
      showSelected="comparison_group",
      data=pval.join)+
    geom_segment(aes(
      log10.regr.mse_lo, train_subsets,
      xend=log10.regr.mse_hi, yend=train_subsets),
      showSelected="comparison_group",
      data=wide.join)+
    facet_grid(algorithm ~ .)+
    geom_point(aes(
      log10.regr.mse_mean, train_subsets),
      showSelected="comparison_group",
      fill=NA,
      data=wide.join),
  absErr=ggplot()+
    ggtitle("Log MSE, train=same")+
    geom_abline(aes(
      slope=slope,
      intercept=intercept),
      color="grey",
      data=data.table(slope=1, intercept=0))+
    geom_point(aes(
      cv_glmnet, featureless, fill=comparison),
      clickSelects="comparison_group",
      alpha_off=0.3,
      alpha=1,
      size=point.size,
      color="black",
      color_off=NA,
      data=wide.same)+
    coord_equal()+
    theme_bw()+
    theme_animint(height=250, width=250)+
    theme(legend.position="none"),
  ## overviewFlip=ggplot()+
  ##   theme_bw()+
  ##   theme_animint(height=500, width=800)+
  ##   theme(legend.position="none")+
  ##   geom_hline(aes(
  ##     yintercept=diff),
  ##     color=vhline.color,
  ##     size=vhline.size,
  ##     data.table(diff=0))+
  ##   geom_vline(aes(
  ##     xintercept=log10.p),
  ##     color=vhline.color,
  ##     size=vhline.size,
  ##     data.table(log10.p=log10(0.05)))+
  ##   geom_segment(aes(
  ##     log10.p_min, estimate_mean,
  ##     color=comparison,
  ##     xend=log10.p_max, yend=estimate_mean),
  ##     size=seg.size,
  ##     clickSelects="comparison",
  ##     alpha=seg.alpha,
  ##     data=cross.dt)+
  ##   geom_segment(aes(
  ##     log10.p_mean, estimate_min,
  ##     color=comparison,
  ##     xend=log10.p_mean, yend=estimate_max),
  ##     size=seg.size,
  ##     clickSelects="comparison",
  ##     alpha=seg.alpha,
  ##     data=cross.dt)+
  ##   geom_text(aes(
  ##     log10.p_mean, estimate_mean,
  ##     label=comparison),
  ##     clickSelects="comparison",
  ##     data=cross.dt)+
  ##   geom_point(aes(
  ##     log10.p, estimate, color=comparison),
  ##     showSelected="comparison",
  ##     size=point.size,
  ##     data=pval.dt)+
  ##   facet_grid(. ~ compare_name),
  selector.types=list(
    comparison="single")
)
if(FALSE){
  animint2pages(viz, "2024-07-10-error-predict-qsip")
}
