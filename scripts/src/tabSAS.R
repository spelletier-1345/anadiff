###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.tabSAS <- function(annot, tab_sens, tab_antisens, color_sens, color_antisens, 
                    sensLabel, expNameTabDble, expNameColDble, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabSAS")}
  byY <- ifelse(sensLabel=="sens", "probe.y", "probe.x")
  byX <- ifelse(sensLabel=="sens", "probe.x", "probe.y")
  tab <- merge(tab_sens[,1:6], annot, by.x="probe_id", by.y=byY, all.y=T)
  tab <- merge(tab, tab_antisens, by.x=byX, by.y="probe_id", all.x=T)
  tab <- tab[,c(2:7,1,8:12,15:ncol(tab))]
  if(sum(duplicated(tab))!=0) {tab <- tab[-which(duplicated(tab)),]}
  ent <- colnames(tab)
  ent[1] <- "transcript.sens"
  ent[7] <- "transcript.antisens"
  ent[2:6] <- paste(substr(ent[2:6], 1, nchar(ent[2:6])-1), "sens", sep="")
  ent[8:12] <- paste(substr(ent[8:12], 1, nchar(ent[8:12])-1), "antisens", sep="")
  colnames(tab) <- ent
  tab <- tab[order(tab$transcript.sens),]
  write.table(tab, expNameTabDble, row.names=F, col.names=T, quote=F, sep="\t")
  
  tc <- data.frame(ts=color_sens[3:(length(color_sens)-1)], ta=color_antisens[3:(length(color_antisens)-1)])
  lchar <- nchar(as.character(tc[1,1]))
  tc$tc <- paste(substr(tc$ts,1, lchar-5), substr(tc$ta,14, lchar), sep="<td></td>")
  t1 <- data.frame(tf=color_antisens[1:2,1])
  t2 <- data.frame(tf=tc$tc)
  t3 <- data.frame(tf=color_antisens[nrow(color_antisens),1])
  tf <- rbind(t1, t2, t3)
  write.table(tf, expNameColDble, row.names=F, col.names=F, quote=F)
  return(tab)
}