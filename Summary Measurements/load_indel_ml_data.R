rm(list = ls())

library(dplyr)

indel_path="~/data/further/InDels/"
indel_file="_Tier_1.vcf"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

indel_summary <- data.frame(matrix(vector(),0,5,dimnames=list(c(), c("Sample_ID","number_of_indels", "number_ins", "number_del", "number_complex"))), stringsAsFactors = F)

for (i in 1:nrow(df_files)) {

  df_indel <- data.frame(matrix(vector(),0,11,dimnames=list(c(), c("CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NORMAL","TUMOUR"))), stringsAsFactors = F)  
  sample <- substr(df_files[i,],0,13)

  indel_file_path <- paste0(indel_path,sample,indel_file) 
  
  indel_conn <- file(indel_file_path, "rt")
  indel_contents <- readLines(indel_conn)
  
  data_section <- c()
  is_data_section = FALSE

  spare_count = 0
  data_count = 0
  
  for (j in 1:length(indel_contents)) {
    #check line for chrom
    line <- indel_contents[j]
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
  
  for (k in 1:nrow(data_section)){
    
    indel_values = strsplit(data_section[k],"\t")
    row <- data.frame(indel_values[[1]][1],indel_values[[1]][2],indel_values[[1]][3],indel_values[[1]][4],indel_values[[1]][5],indel_values[[1]][6],indel_values[[1]][7],indel_values[[1]][8],indel_values[[1]][9],indel_values[[1]][10],indel_values[[1]][11])
    
    df_indel <- rbind(df_indel,row)
    
  }
  colnames(df_indel) <- c("CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT","NORMAL","TUMOUR")

  total_count=0
  in_count=0
  del_count=0
  comp_count=0
  
  for (m in 1:nrow(df_indel)) {
    indel_record <- df_indel[m,]
    if (indel_record$FILTER == "PASS") {
      ref <- indel_record$REF
      alt <- indel_record$ALT
      total_count <- total_count + 1

      if (substring(ref,1,1) != substring(alt,1,1)){
        comp_count <- comp_count + 1
      } else if (nchar(ref) > nchar(alt)) {
	      del_count <- del_count + 1
      } else if (nchar(ref) < nchar(alt)) {
	      in_count <- in_count + 1
      }
    }
  }
  
  row <- data.frame(substr(sample,0,13), total_count, in_count, del_count, comp_count)
  indel_summary <- rbind(indel_summary,row)
}
colnames(indel_summary) <- c("Sample_ID","number_of_indels", "number_ins", "number_del", "number_complex")

write.table(indel_summary, file = "indel_summary.txt", sep="\t",quote=F, row.names = FALSE)