rm(list = ls())

get_unique_cna_count <- function(data) {
  data_key <- data[,c(2,3)]
  data_key <- unique(data_key)
  
  for (i in 1:nrow(data_key)) {
    chr <- data_key[i,c("chr")]
    startpos <- data_key[i,c("startpos")]
    count <- nrow(data[data$chr == chr & data$startpos == startpos,])
    print(paste("chr:",chr,"startpos:",startpos,"count:",count))         
  }
}

cna_count_summary <- function(ppcg) {
  ppcg_gain <- ppcg[ppcg$CNA == "cGain" | ppcg$CNA == "sGain",]
  ppcg_loh <- ppcg[ppcg$CNA == "cLOH" | ppcg$CNA == "sLOH",]
  ppcg_hd <- ppcg[ppcg$CNA == "cHD" | ppcg$CNA == "sHD",]
  
  ppcg_total_count <- nrow(ppcg)
  ppcg_gain_count <- nrow(ppcg_gain)
  ppcg_loh_count <- nrow(ppcg_loh)
  ppcg_hd_count <- nrow(ppcg_hd)
  
  print(paste("GAIN",ppcg_gain_count))
  get_unique_cna_count(ppcg_gain)
  print(paste("LOH",ppcg_loh_count))
  get_unique_cna_count(ppcg_loh)
  print(paste("HD",ppcg_hd_count))
  get_unique_cna_count(ppcg_hd)
}

data <- read.csv("PPCG_mergedseg.txt", sep = "\t")

cna_count_summary(data)



