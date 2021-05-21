rm(list = ls())

source("functions_subtyping.R")

cohort_name = "PPCGl"

subclone_codes <- read.csv(paste0("PPCG_Subclone_codes.txt"), sep = "\t", header = FALSE, row.names = NULL)
ppcga <- read.csv(paste0(cohort_name,"_samples_c1.txt"), sep = "\t", header = FALSE, row.names = NULL)
ppcgb <- read.csv(paste0(cohort_name,"_samples_c2.txt"), sep = "\t", header = FALSE, row.names = NULL)
#ppcgc <- read.csv(paste0(cohort_name,"_samples_c3.txt"), sep = "\t", header = FALSE, row.names = NULL)

colnames(subclone_codes) <- c("Code", "Subclone")
colnames(ppcga) <- c("Code")
colnames(ppcgb) <- c("Code")
#colnames(ppcgc) <- c("Code")

samples_A <- inner_join(ppcga, subclone_codes, by="Code")
samples_B <- inner_join(ppcgb, subclone_codes, by="Code")
#samples_C <- inner_join(ppcgc, subclone_codes, by="Code")

write.table(samples_A$Subclone, file = paste0(cohort_name,"_samples_A.txt"), sep="\t",quote=F,row.names=FALSE,col.names=FALSE)
write.table(samples_B$Subclone, file = paste0(cohort_name,"_samples_B.txt"), sep="\t",quote=F,row.names=FALSE,col.names=FALSE)
#write.table(samples_C$Subclone, file = paste0(cohort_name,"_samples_C.txt"), sep="\t",quote=F,row.names=FALSE,col.names=FALSE)