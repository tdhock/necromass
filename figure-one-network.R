library(animint2)
library(data.table)
data.name <- "bacteria_fungi_conservative"
source_target.csv <- sprintf("necromass_%s_Pearson_source_target.csv", data.name)
weight.dt <- fread(source_target.csv)
data.list <- list()
for(data.type in c("power_transformed", "raw")){
  data.csv <- sprintf("%s_%s.csv", data.name, data.type)
  data.list[[data.type]] <- fread(data.csv)
}
norm01 <- function(x)(x-min(x))/(max(x)-min(x))
scatter.dt <- do.call(data.table, lapply(data.list[["power_transformed"]], norm01))
taxa.vec <- names(scatter.dt)
N.taxa <- ncol(scatter.dt)
weight.dt[source<target]
get_pair <- function(i, j){
  sprintf("%s, %s", taxa.vec[i], taxa.vec[j])
}
weight.dt[, pair := get_pair(target+1, source+1)]
##<20 is bacteria, >=20 is fungi.
(taxa.dt <- data.table(
  taxon=taxa.vec, taxon.i=seq(0, N.taxa-1)
)[, kingdom := {
  ifelse(taxon.i<19, "bacteria", "fungi")
}][, i.pi := {
  2*pi*taxon.i/.N
}][, `:=`(
  x=sin(i.pi), y=cos(i.pi)
)][])
source.xy <- taxa.dt[weight.dt, .(
  pair, source_x=x, source_y=y, target, weight), on=.(taxon.i=source)]
both.xy <- taxa.dt[source.xy, .(
  pair, target_x=x, target_y=y, source_x, source_y, target, weight), on=.(taxon.i=target)]
both.xy[, sign := ifelse(weight<0, "negative", "positive")]
pair.dt.list <- list()
text.dt.list <- list()
for(var.x in seq(1, N.taxa-1)){
  for(var.y in seq(var.x+1, N.taxa)){
    pair <- get_pair(var.x, var.y)
    pair.dt.list[[pair]] <- data.table(
      pair,
      data.x=scatter.dt[[var.x]],
      data.y=scatter.dt[[var.y]])
    text.dt.list[[pair]] <- data.table(
      pair,
      taxon.x=taxa.vec[var.x],
      taxon.y=taxa.vec[var.y])
  }
}
pair.dt <- rbindlist(pair.dt.list)
text.dt <- rbindlist(text.dt.list)

animint(
  title=paste(data.name,"visualization"),
  network=ggplot()+
    ggtitle("Click edge to select pair")+
    theme(
      axis.line=element_blank(),
      axis.text=element_blank(), 
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      panel.border=element_blank(),
      panel.background=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank())+
    coord_equal()+
    geom_segment(aes(
      target_x, target_y,
      xend=source_x, yend=source_y,
      color=sign),
      alpha=0.6,
      clickSelects="pair",
      data=both.xy)+
    geom_point(aes(
      x, y, fill=kingdom, tooltip=taxon),
      shape=21,
      size=4,
      data=taxa.dt)+
    scale_fill_manual(values=c(fungi="black", bacteria="white")),
  scatter=ggplot()+
    ggtitle("Normalized abundance for selected pair")+
    geom_point(aes(
      data.x, data.y),
      showSelected="pair",
      data=pair.dt)+
    geom_text(aes(
      0.5, -0.05, label=taxon.x),
      showSelected="pair",
      data=text.dt)+
    geom_text(aes(
      -0.05, 0.5, label=taxon.y),
      angle=90,
      showSelected="pair",
      data=text.dt))

