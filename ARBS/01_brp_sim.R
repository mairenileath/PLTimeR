
## Step 1

#setwd("/gpfs2/well/wedge/atesah/Per_chr_BSA/ARBS_analyses/Groups/Input")

library(regioneR)
args = commandArgs(TRUE)
sample = toString(args[1])
chr = toString(args[2])

iterations = 1000

hg19 = as.data.frame(read.table("hg19.csv",header=T,sep=","))

mask = as.data.frame(read.table("mask2.csv",header=T,sep=","))

ARfile = as.data.frame(read.table("Tumour_AR_regions.csv",header=T,sep=","))

SVfile = as.data.frame(read.table(paste0(sample, "_SVs.csv"),header=T,sep=","))

chrm = paste0("chr", chr) 

output_dir <- paste0("./", sample, "/Per_chr/", chrm)

if(!file.exists(output_dir)){
	dir.create(output_dir)
}

setwd(output_dir)

genome = subset(hg19, hg19[,1] == chrm)

genome = toGRanges(genome)

chrmask = subset(mask, mask[,1] == chrm)

chrmask = toGRanges(chrmask)

write.table(chrmask, file = paste0("mask", chr,".csv"), sep =",", col.names = TRUE, row.names = FALSE)

ARreg = subset(ARfile, ARfile[,1] == chrm)

write.table(ARreg, file = paste0("ARreg", chr,".csv"), sep =",", col.names = TRUE, row.names = FALSE)

ARreg = toGRanges(ARreg)

SVreg = SVfile[which(SVfile[,1] == chrm),]

write.table(SVreg, file = paste0("SVreg", chr, ".csv"), sep =",", col.names = TRUE, row.names = FALSE)

SVreg = toGRanges(SVreg)

##################################################################################################################

sim_data1 = cbind(c("Brp", 1:length(SVreg[,1])), c("chr", rep(chrm, times= length(SVreg[,1]))))

sim_data2 = cbind(c("Brp", 1:length(SVreg[,1])), c("chr", rep(chrm, times= length(SVreg[,1]))))

disto = distanceToNearest(SVreg, ARreg)

write.table(disto, file = "tmp00.csv", sep =",", col.names = TRUE, row.names = FALSE)

do = as.data.frame(read.table("tmp00.csv",header=T,sep=","))

for (i in 1:iterations) {

randreg = createRandomRegions(nregions=length(SVreg[,1]), length.mean= 1, length.sd= 0, genome= genome, mask= chrmask, non.overlapping= FALSE)

dist = distanceToNearest(randreg, ARreg)

write.table(randreg, file = "tmp01.csv", sep =",", col.names = TRUE, row.names = FALSE)

write.table(dist, file = "tmp02.csv", sep =",", col.names = TRUE, row.names = FALSE)

rr = as.data.frame(read.table("tmp01.csv",header=T,sep=","))

d = as.data.frame(read.table("tmp02.csv",header=T,sep=","))

sim_data1 = cbind(sim_data1, c(paste("Pos_",i,sep=""), rr[,2]))

sim_data2 = cbind(sim_data2, c(paste("dist_",i,sep=""), d[,3]))

}

write.table(sim_data1, file = paste0("Simulated_data_brp_", chr, ".csv"), sep =",", col.names = FALSE, row.names = FALSE)

write.table(sim_data2, file = paste0("Simulated_data_dist_", chr, ".csv"), sep =",", col.names = FALSE, row.names = FALSE)

disto = distanceToNearest(SVreg, ARreg)

write.table(disto, file = "tmp00.csv", sep =",", col.names = TRUE, row.names = FALSE)


###############################################################################################################################################################
