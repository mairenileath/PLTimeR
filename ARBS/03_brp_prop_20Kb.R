
## Step 3

#setwd("/gpfs2/well/wedge/atesah/Per_chr_BSA/ARBS_analyses/Groups/Input")

gl = as.data.frame(read.table("Groups_Atef.txt",header=F,sep='\t'))

output1 = c("Sample", "Color", "20kb_brp_prop", "Obs-Sim", "Obs-CI", "Obs-Sim_adj")

output2 = c("Sample", "Color", "20kb_brp_prop", "Obs-Sim", "Obs-CI", "Obs-Sim_adj")

for (sample in gl[,1]) {

all_path = paste0("./", sample, "/Output/all_chr/", sample, "_all_chr_Per_bin_plot_data.csv")
min_21_path = paste0("./", sample, "/Output/min_21/", sample, "_min_21_Per_bin_plot_data.csv")

if (file.exists(all_path)) {
  data1 = as.data.frame(read.table(all_path,header=T,sep=","))

  c1 = as.character(data1[2,7])
  p1 = round((as.numeric(data1[2,2])/sum(as.numeric(data1[,2]))), digits= 2)
  os1 = data1[2,6]

  if (as.numeric(data1[2,6]) > 0) {
    oc1 = round((as.numeric(data1[2,2]) - as.numeric(data1[2,5])), digits= 0)
  } else if (as.numeric(data1[2,6]) < 0) {
    oc1 = round((as.numeric(data1[2,2]) - as.numeric(data1[2,4])), digits= 0)
  } else if (as.numeric(data1[2,6]) == 0) {
    oc1 = min(round((as.numeric(data1[2,2]) - as.numeric(data1[2,5])), digits= 0), round((as.numeric(data1[2,2]) - as.numeric(data1[2,4])), digits= 0))
  }

  osa1 = round((as.numeric(data1[2,6])/sum(as.numeric(data1[,2]))), digits= 2)

} else {
  c1 = "grey"
  p1 = 0
  os1 = 0
  oc1 = 0
  osa1 = 0
}

output1 = rbind(output1, c(sample, c1, p1, os1, oc1, osa1))

if (file.exists(min_21_path)) {
  data2 = as.data.frame(read.table(min_21_path,header=T,sep=","))

  c2 = as.character(data2[2,7])
  p2 = round((as.numeric(data2[2,2])/sum(as.numeric(data2[,2]))), digits= 2)
  os2 = data2[2,6]

  if (as.numeric(data2[2,6]) > 0) {
    oc2 = round((as.numeric(data2[2,2]) - as.numeric(data2[2,5])), digits= 0)
  }

  else if (as.numeric(data2[2,6]) < 0) {
    oc2 = round((as.numeric(data2[2,2]) - as.numeric(data2[2,4])), digits= 0)
  }

  else if (as.numeric(data2[2,6]) == 0) {
    oc2 = min(round((as.numeric(data2[2,2]) - as.numeric(data2[2,5])), digits= 0), round((as.numeric(data2[2,2]) - as.numeric(data2[2,4])), digits= 0))
  }

  osa2 = round((as.numeric(data2[2,6])/sum(as.numeric(data2[,2]))), digits= 2)

} else {
  c1 = "grey"
  p1 = 0
  os1 = 0
  oc1 = 0
  osa1 = 0
}

output2 = rbind(output2, c(sample, c2, p2, os2, oc2, osa2))

}

write.table(output1, file = "./Per_group_20kb_brp_prop_all_chr.csv", sep =",", col.names = FALSE, row.names = FALSE)

write.table(output2, file = "./Per_group_20kb_brp_prop_min_21.csv", sep =",", col.names = FALSE, row.names = FALSE)

