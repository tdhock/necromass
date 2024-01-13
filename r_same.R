library(data.table)
data.list <- list()
cons.csv.vec <- Sys.glob("r_same/*.csv")
for(cons.csv in cons.csv.vec){
  king.dt <- nc::capture_first_vec(cons.csv, "/", kingdom=".*?", "_")
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
meta.long <- rbindlist(meta.long.list)[, Sample_Num := sub("16S-|ITS[.]", "", Sample_ID)]
meta.longer <- melt(meta.long, id=c("kingdom", "Necrobag_ID"))
meta.wide <- dcast(meta.longer, Necrobag_ID + variable ~ kingdom)
(question <- meta.wide[bacteria != fungi & !variable %in% c("Domain","Sample_ID")])
fwrite(question, "r_same_meta_data_differences.csv")
## bacteria file meta-data look more reasonable.
lapply(data.list, function(DT)DT[,.(Melanization,Plot,Comp)])
