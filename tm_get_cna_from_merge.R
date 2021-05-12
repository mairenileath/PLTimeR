rm(list = ls())

df_A <- read.csv("PPCGA_mergedseg.txt", sep = "\t")
df_B <- read.csv("PPCGB_mergedseg.txt", sep = "\t")
df_C <- read.csv("PPCGC_mergedseg.txt", sep = "\t")

get_cna <- function(data,letter) {
  results <- paste0(data$CNA,".",data$chr,".",data$startpos,".",data$endpos)
  results <- substr(results,2,nchar(results))
  results <- unique(results)
  
  start_pos <- trimws(format(data$startpos,big.mark=",",scientific=FALSE))
  end_pos <- trimws(format(data$endpos,big.mark=",",scientific=FALSE))
  
  results_chr <- paste0("chr",data$chr,":",start_pos,"-",end_pos)
  results_chr <- unique(results_chr)

  write.table(results, file = paste0("CNA_",letter,".txt"), sep="\t",quote=F, col.names=FALSE, row.names=FALSE)
  write.table(results_chr, file = paste0("CNA_chr_",letter,".txt"), sep="\t",quote=F, col.names=FALSE, row.names=FALSE)
}

cna_a <- get_cna(df_A,"A")
cna_b <- get_cna(df_B,"B")
cna_c <- get_cna(df_C,"C")
