rm(list = ls())

library(data.table)
source("functions_subtyping.R")

cohort_name = "PPCGl"

cluster_count = 2

#Import classification datadata
df_distances <- read.csv(paste0(cohort_name,"_distance_summary.txt"), sep = "\t", header = TRUE, row.names = NULL)

#Converts to matrix
df_mat <- acast(df_distances, A~B, value.var="Distance", mean)
df_dist <- as.dist(df_mat)

#Hierarchical clustering
hclust_result <- hclust(df_dist, method = 'average')

#Cut off a cluster number
cut_tree <- cutree(hclust_result, k=cluster_count)

df_groups <- setDT(as.data.frame(cut_tree),keep.rownames = TRUE)
colnames(df_groups) = c('sample', 'cluster')

#Get each unique cluster value

clusters <- unique(df_groups$cluster)

for (i in 1:length(clusters)) {
  cluster_samples <- df_groups[df_groups$cluster == clusters[i],]
  #write.table(cluster_samples$sample, file = paste0(cohort_name,"_samples_c",clusters[i],".txt"), sep="\t",quote=F,row.names=FALSE,col.names=FALSE)
}
