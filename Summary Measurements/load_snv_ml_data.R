rm(list = ls())

library(dplyr)

snv_path="~/data/further/SNVs/"
snv_file=".filtered.extended.pon.vcf"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

snv_summary <- data.frame(matrix(vector(),0,2,dimnames=list(c(), c("Sample_ID","number_of_snvs"))), stringsAsFactors = F)  

for (i in 1:nrow(df_files)) {

  df_snv <- data.frame(matrix(vector(),0,11,dimnames=list(c(), c("CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NORMAL","TUMOUR"))), stringsAsFactors = F)  
  sample <- substr(df_files[i,],0,30)

  snv_file_path <- paste0(snv_path,sample,snv_file) 
  
  snv_conn <- file(snv_file_path, "rt")
  snv_contents <- readLines(snv_conn)
  
  data_section <- c()
  is_data_section = FALSE

  spare_count = 0
  data_count = 0
  
  for (j in 1:length(snv_contents)) {
    #check line for chrom
    line <- snv_contents[j]
    
    if (is_data_section) {
      data_section <- rbind(data_section, line)
      data_count = data_count + 1
    } else {
      if (startsWith(line, "#CHROM")) {
        is_data_section = TRUE
      }
      spare_count = spare_count + 1
    }
  }
  
  #for (k in 1:nrow(data_section)){
  #  
  #  snv_values = strsplit(data_section[k],"\t")
  #  row <- data.frame(snv_values[[1]][1],snv_values[[1]][2],snv_values[[1]][3],snv_values[[1]][4],snv_values[[1]][5],snv_values[[1]][6],snv_values[[1]][7],snv_values[[1]][8],snv_values[[1]][9],snv_values[[1]][10],snv_values[[1]][11])
  #  
  #  df_snv <- rbind(df_snv,row)
  #}
  
  
  #row <- data.frame(substr(sample,0,13), nrow(df_snv))
  row <- data.frame(substr(sample,0,13), nrow(data_section))

  snv_summary <- rbind(snv_summary,row)
}

colnames(snv_summary) <- c("Sample_ID","number_of_snvs")

write.table(snv_summary, file = "snv_summary.txt", sep="\t",quote=F, row.names = FALSE)


