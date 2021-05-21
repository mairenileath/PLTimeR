rm(list = ls())

library(dplyr)

# This loads the data for ploidy, kataegis and chromothripsis

chromothripsis_path="~/data/further/PPCG_Chromothripsis_Calls_21_Jan_2021_fixed.tsv"
kataegis_path="~/data/further/PPCG_Kataegis_Calls_21_Jan_2021.tsv"
wgd_path="~/data/further/PPCG_WGD_21_Jan_2021.tsv"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

df_chromothripsis <- read.csv(chromothripsis_path, sep="\t")

df_other_summary <- data.frame(matrix(vector(),0,5,dimnames=list(c(), c("Sample_ID", "CT2", "CT_prop", "CT_per_sample", "CT_max_size"))), stringsAsFactors = F)

#load chromothripsis
for (i in 1:nrow(df_files)) {

  ct_exists = 0 
  ct_prop = 0
  ct_per_sample = 0 
  ct_max_size = 0
  sample_ID = substr(df_files[i,],0,13)

  df_samples <- df_chromothripsis[df_chromothripsis$PPCG_Sample_ID == sample_ID,]

  if (nrow(df_samples) > 0) {
    ct_exists = 1
    ct_per_sample = nrow(df_samples)   
    sv_count = 0
    cluster_size = 0
    ct_max_size = 0
    sample_ID = ""
    # loop through all chromothripsis records.
    for (j in 1:nrow(df_samples)) {
      df_sample = df_samples[j,]
      sample_ID = df_sample$PPCG_Sample_ID
      # set sv count size
      sv_count = df_sample$number_SVs_sample
      #cumulatively add cluster sizes
      cluster_size = cluster_size + df_sample$clusterSize_including_TRA
      #find size
      size = df_sample$end-df_sample$start
      #Check max size
      if (size > ct_max_size) {
        ct_max_size = size
      }
    }
    if (sv_count > 0) {
      ct_prop = cluster_size/sv_count
    }
  }
  row <- data.frame(sample_ID, ct_exists, ct_prop, ct_per_sample, ct_max_size)
  df_other_summary <- rbind(df_other_summary,row)
}

#load kataegis
df_kat <- read.csv(kataegis_path, sep="\t")

kat_values <- c()

for (i in 1:nrow(df_files)) {
  sample_ID = substr(df_files[i,],0,13)
  kat_occured = 0
  df_kat_samples <- df_kat[df_kat$PPCG_Sample_ID == sample_ID,]
  
  if (nrow(df_kat_samples) > 0) {
    kat_occured = 1
  }

  kat_values <- rbind(kat_values,kat_occured)
}
df_other_summary <- cbind(df_other_summary, kat_values)

names(df_other_summary) = c("Sample_ID", "CT2", "CT_prop", "CT_per_sample", "CT_max_size", "kataegis")
write.table(df_other_summary, file = "other_summary.txt", sep="\t",quote=F, row.names = FALSE)

