rm(list = ls())

library(dplyr)

sv_bedepe_path="~/data/further/PPCG_SV_Data_Release_15_July_2020__bedpe/"
sv_bedepe_file="_ppcg_consensus_annotated.somatic.sv.bedpe"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

df_summary <- data.frame(matrix(vector(),0,6,dimnames=list(c(), c("Sample_ID","number_of_rearrangements", "rearrangements_inversion", "rearrangements_deletion", "rearrangements_tandemDuplication","rearrangements_translocation"))), stringsAsFactors = F)

for (i in 1:nrow(df_files)) {
  
  sample <- substr(df_files[i,],0,13)
  
  sv_tot <- 0
  sv_inv <- 0
  sv_del <- 0
  sv_dup <- 0
  sv_tra <- 0

  sv_file <- paste0(sv_bedepe_path,sample,sv_bedepe_file) 

  if (file.exists(sv_file)) {
    df_sv <- read.csv(sv_file, sep = "\t")
  
    #2 number_of_rearrangements
    sv_tot <- nrow(df_sv)
  
    #3 rearrangements_inversion
    df_sv_inv <- df_sv[df_sv$svclass=="t2tINV" | df_sv$svclass=="h2hINV",]
    sv_inv <- nrow(df_sv_inv)
  
    #4 rearrangements_deletion
    df_sv_del <- df_sv[df_sv$svclass=="DEL",]
    sv_del <- nrow(df_sv_del)
  
    #5 rearrangements_tandemDuplication
    df_sv_dup <- df_sv[df_sv$svclass=="DUP",]
    sv_dup <- nrow(df_sv_dup)
  
    #6 rearrangements_translocation
    df_sv_tra <- df_sv[df_sv$svclass=="TRA",]
    sv_tra <- nrow(df_sv_tra)
  }
    
  row <- data.frame(sample, sv_tot, sv_inv, sv_del, sv_dup, sv_tra)
  df_summary <- rbind(df_summary,row)
}

colnames(df_summary) <- c("Sample_ID","number_of_rearrangements", "rearrangements_inversion", "rearrangements_deletion", "rearrangements_tandemDuplication","rearrangements_translocation")

write.table(df_summary, file = "sv_summary.txt", sep="\t",quote=F, row.names = FALSE)

