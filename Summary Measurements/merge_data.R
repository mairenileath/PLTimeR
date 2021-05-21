rm(list = ls())

snv <- read.csv("snv_summary.txt", header=TRUE, sep="\t")
indel <- read.csv("indel_summary.txt", header=TRUE, sep="\t")
cna <- read.csv("cna_summary.txt", header=TRUE, sep="\t")
pga <- read.csv("pga_summary.txt", header=TRUE, sep="\t")
sv <- read.csv("sv_summary.txt", header=TRUE, sep="\t")
other <- read.csv("other_summary.txt", header=TRUE, sep="\t")

combined <- merge(snv, indel, by="Sample_ID")
combined <- merge(combined, sv, by="Sample_ID")
combined <- merge(combined, pga, by="Sample_ID")
combined <- merge(combined, other, by="Sample_ID")
combined <- merge(combined, cna, by="Sample_ID")

rownames(combined) <- combined[,1]
combined <- combined[,-1]

#Rank and scale appropriate for these methods. 

write.table(combined, file = "combined_summary.txt", sep="\t",quote=F)
