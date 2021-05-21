rm(list = ls())

library(dplyr)

subclone_path="~/data/tm/All/Subclonal_SCNA_01_June_2020/"
subclone_file="_Subclonal_SCNA.txt"

wgd_path="~/data/further/PPCG_WGD_21_Jan_2021.tsv"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

#load wgd
df_wgd <- read.csv(wgd_path, sep="\t")

wgd_values <- c()

pga_summary <- data.frame(matrix(vector(),0,5,dimnames=list(c(), c("Sample_ID","ploidy","pga_clonal","pga_subclonal","pga_total"))), stringsAsFactors = F) 

for (i in 1:nrow(df_files)) {
  wgd_sample_ID = substr(df_files[i,],0,13)
  
  df_wgd_samples <- df_wgd[df_wgd$PPCG_Sample_ID == wgd_sample_ID,]
  df_wgd_sample <- df_wgd_samples[1,]
  
  if (df_wgd_sample$WGD == TRUE) {
    wgd_occured <- 1
  } else {
    wgd_occured <- 0
  }
  
  #load subclone file
  subclone_sample_ID <- substr(df_files[i,],0,30)
  subclone_file_path <- paste0(subclone_path,subclone_sample_ID,subclone_file) 
  df_subclone <- read.csv(subclone_file_path, sep="\t")
  
  total_length <- 0
  altered_length <- 0 
  clonal_altered_length <- 0 
  subclonal_altered_length <- 0 
  
  for (j in 1:nrow(df_subclone)) {
    
    s_row <- df_subclone[j,]
    length <- s_row$endpos - s_row$startpos
    total_length <- total_length + length
    is_altered = FALSE
    
    
    if (!(is.na(s_row$nMaj1_A) | is.na(s_row$nMin1_A))){
      if (!((wgd_occured == 1 & s_row$nMaj1_A == 2 & s_row$nMin1_A == 2) | (wgd_occured == 0 & s_row$nMaj1_A == 1 & s_row$nMin1_A == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_A) | is.na(s_row$nMin2_A))){
      if (!((wgd_occured == 1 & s_row$nMaj2_A == 2 & s_row$nMin2_A == 2) | (wgd_occured == 0 & s_row$nMaj2_A == 1 & s_row$nMin2_A == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj1_B) | is.na(s_row$nMin1_B))){
      if (!((wgd_occured == 1 & s_row$nMaj1_B == 2 & s_row$nMin1_B == 2) | (wgd_occured == 0 & s_row$nMaj1_B == 1 & s_row$nMin1_B == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_B) | is.na(s_row$nMin2_B))){
      if (!((wgd_occured == 1 & s_row$nMaj2_B == 2 & s_row$nMin2_B == 2) | (wgd_occured == 0 & s_row$nMaj2_B == 1 & s_row$nMin2_B == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj1_C) | is.na(s_row$nMin1_C))){
      if (!((wgd_occured == 1 & s_row$nMaj1_C == 2 & s_row$nMin1_C == 2) | (wgd_occured == 0 & s_row$nMaj1_C == 1 & s_row$nMin1_C == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_C) | is.na(s_row$nMin2_C))){
      if (!((wgd_occured == 1 & s_row$nMaj2_C == 2 & s_row$nMin2_C == 2) | (wgd_occured == 0 & s_row$nMaj2_C == 1 & s_row$nMin2_C == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj1_D) | is.na(s_row$nMin1_D))){
      if (!((wgd_occured == 1 & s_row$nMaj1_D == 2 & s_row$nMin1_D == 2) | (wgd_occured == 0 & s_row$nMaj1_D == 1 & s_row$nMin1_D == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_D) | is.na(s_row$nMin2_D))){
      if (!((wgd_occured == 1 & s_row$nMaj2_D == 2 & s_row$nMin2_D == 2) | (wgd_occured == 0 & s_row$nMaj2_D == 1 & s_row$nMin2_D == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj1_E) | is.na(s_row$nMin1_E))){
      if (!((wgd_occured == 1 & s_row$nMaj1_E == 2 & s_row$nMin1_E == 2) | (wgd_occured == 0 & s_row$nMaj1_E == 1 & s_row$nMin1_E == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_E) | is.na(s_row$nMin2_E))){
      if (!((wgd_occured == 1 & s_row$nMaj2_E == 2 & s_row$nMin2_E == 2) | (wgd_occured == 0 & s_row$nMaj2_E == 1 & s_row$nMin2_E == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj1_F) | is.na(s_row$nMin1_F))){
      if (!((wgd_occured == 1 & s_row$nMaj1_F == 2 & s_row$nMin1_F == 2) | (wgd_occured == 0 & s_row$nMaj1_F == 1 & s_row$nMin1_F == 1))) {
        is_altered = TRUE
      }
    }
    if (!(is.na(s_row$nMaj2_F) | is.na(s_row$nMin2_F))){
      if (!((wgd_occured == 1 & s_row$nMaj2_F == 2 & s_row$nMin2_F == 2) | (wgd_occured == 0 & s_row$nMaj2_F == 1 & s_row$nMin2_F == 1))) {
        is_altered = TRUE
      }
    }
    
    if (is_altered) {
      altered_length <- altered_length + length
      if (s_row$frac1_A == 1) {
        clonal_altered_length <- clonal_altered_length + length
      } else {
        subclonal_altered_length <- subclonal_altered_length + length
      }
    }
  }
  
  pga_total <- (altered_length/total_length)*100
  pga_clon <- (clonal_altered_length/total_length)*100
  pga_sub <- (subclonal_altered_length/total_length)*100
  
  row <- data.frame(wgd_sample_ID, wgd_occured, pga_clon, pga_sub, pga_total)
  pga_summary <- rbind(pga_summary,row)
  
}

colnames(pga_summary) = c("Sample_ID","ploidy","pga_clonal","pga_subclonal","pga_total")
write.table(pga_summary, file = "pga_summary.txt", sep="\t",quote=F, row.names = FALSE)
