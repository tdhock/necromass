library(data.table)
library(ggplot2)
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

seg.size <- 1.5
vhline.color <- "grey50"
vhline.size <- 1
point.size <- 3
text.both <-   rbind(
  cross.dt[, .(what="cross", comparison, compare_name, estimate=estimate_mean, log10.p=log10.p_mean, lab=comparison)],
  pval.dt[, .(what="pval", comparison, compare_name, estimate, log10.p, lab=test.group)])
gg <- ggplot()+
  theme_bw()+
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
    yend=log10.p_max, xend=estimate_mean),
    size=seg.size,
    data=cross.dt)+
  geom_segment(aes(
    estimate_mean, log10.p_min, 
    color=comparison,
    yend=log10.p_max, xend=estimate_mean),
    size=seg.size-1,
    data=cross.dt)+
  geom_segment(aes(
    estimate_min, log10.p_mean, 
    yend=log10.p_mean, xend=estimate_max),
    size=seg.size,
    data=cross.dt)+
  geom_segment(aes(
    estimate_min, log10.p_mean, 
    color=comparison,
    yend=log10.p_mean, xend=estimate_max),
    size=seg.size-1,
    data=cross.dt)+
  geom_point(aes(
    estimate, log10.p, fill=comparison),
    shape=21,
    color="black",
    size=point.size,
    data=pval.dt)+
  ## geom_text(aes(
  ##   estimate_mean, log10.p_mean, 
  ##   label=comparison),
  ##   showSelected="comparison",
  ##   data=cross.dt)+
  ## geom_text(aes(
  ##   estimate, log10.p, label=test.group),
  ##   clickSelects="comparison_group",
  ##   showSelected="comparison",
  ##   data=pval.dt)+
  ggrepel::geom_label_repel(aes(
    estimate, log10.p, label=lab, color=comparison),
    alpha=0.8,
    data=text.both[what=="cross"])+
  scale_x_continuous(
    "Error difference, cv_glmnet trained on same and compare subset")+
  scale_y_continuous(
    "log10(P-value), two-sided paired t-test")+
  facet_grid(. ~ compare_name, labeller=label_both)
png("figure-scatter-print.png", width=10, height=5, units="in", res=200)
print(gg)
dev.off()
