library(data.table)
library(animint2)
score.out <- fread(
  "figure-scatter-data.csv"
)[
, log10.regr.mse := log10(regr.mse)
][]
score.stats <- dcast(
  score.out,
  comparison + train.groups + test.group + algorithm ~ .,
  list(mean, sd, length),
  value.var="log10.regr.mse")
score.wide <- dcast(
  score.out[algorithm=="cv_glmnet"],
  comparison + test.fold + test.group ~ train.groups,
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
}, by=.(comparison,test.group,compare_name)]

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

seg.size <- 4
seg.alpha <- 0.7
vhline.color <- "grey50"
vhline.size <- 1
viz <- animint(
  title="Accuracy of predicting qSIP data",
  out.dir="figure-scatter-interactive",
  source="https://github.com/tdhock/necromass/blob/main/data-qsip-jeff/figure-scatter/figure-scatter.R",
  overview=ggplot()+
    ggtitle("no color: no auto showSelected=comparison")+
    theme_bw()+
    geom_hline(aes(
      yintercept=diff),
      color=vhline.color,
      size=vhline.size,
      data.table(diff=0))+
    geom_vline(aes(
      xintercept=log10.p),
      color=vhline.color,
      size=vhline.size,
      data.table(log10.p=log10(0.05)))+
    geom_point(aes(
      log10.p, estimate, color=comparison),
      showSelected="comparison",
      data=pval.dt)+
    geom_segment(aes(
      log10.p_min, estimate_mean,
      xend=log10.p_max, yend=estimate_mean),
      size=seg.size,
      clickSelects="comparison",
      alpha=seg.alpha,
      data=cross.dt)+
    geom_segment(aes(
      log10.p_mean, estimate_min,
      xend=log10.p_mean, yend=estimate_max),
      size=seg.size,
      clickSelects="comparison",
      alpha=seg.alpha,
      data=cross.dt)+
    facet_grid(. ~ compare_name),
  noLegend=ggplot()+
    ggtitle("no legend: no auto showSelected=comparison")+
    theme_bw()+
    theme(legend.position="none")+
    geom_hline(aes(
      yintercept=diff),
      color=vhline.color,
      size=vhline.size,
      data.table(diff=0))+
    geom_vline(aes(
      xintercept=log10.p),
      color=vhline.color,
      size=vhline.size,
      data.table(log10.p=log10(0.05)))+
    geom_point(aes(
      log10.p, estimate, color=comparison),
      showSelected="comparison",
      data=pval.dt)+
    geom_segment(aes(
      log10.p_min, estimate_mean,
      color=comparison,
      xend=log10.p_max, yend=estimate_mean),
      size=seg.size,
      clickSelects="comparison",
      showSelected=character(),
      alpha=seg.alpha,
      data=cross.dt)+
    geom_segment(aes(
      log10.p_mean, estimate_min,
      color=comparison,
      xend=log10.p_mean, yend=estimate_max),
      size=seg.size,
      clickSelects="comparison",
      showSelected=character(),
      alpha=seg.alpha,
      data=cross.dt)+
    facet_grid(. ~ compare_name),
  color=ggplot()+
    ggtitle("limitation: legend forces showSelected")+
    theme_bw()+
    geom_hline(aes(
      yintercept=diff),
      color=vhline.color,
      size=vhline.size,
      data.table(diff=0))+
    geom_vline(aes(
      xintercept=log10.p),
      color=vhline.color,
      size=vhline.size,
      data.table(log10.p=log10(0.05)))+
    geom_point(aes(
      log10.p, estimate, color=comparison),
      showSelected="comparison",
      data=pval.dt)+
    geom_segment(aes(
      log10.p_min, estimate_mean,
      color=comparison,
      xend=log10.p_max, yend=estimate_mean),
      size=seg.size,
      clickSelects="comparison",
      showSelected=character(),
      alpha=seg.alpha,
      data=cross.dt)+
    geom_segment(aes(
      log10.p_mean, estimate_min,
      color=comparison,
      xend=log10.p_mean, yend=estimate_max),
      size=seg.size,
      clickSelects="comparison",
      showSelected=character(),
      alpha=seg.alpha,
      data=cross.dt)+
    facet_grid(. ~ compare_name),
  selector.types=list(
    comparison="single")
)
animint2pages(viz, "2024-07-10-figure-scatter-qsip")
