library(mclust)

df_ppcg <- read.csv("PPCG.txt", sep = "\t")

df_ppcg <- df_ppcg[,1]

ca_ae_distance <- read.csv("umap_distance_summary.txt", sep = "\t")

df_mat <- acast(ca_ae_distance, A~B, value.var="Distance", mean)
df_dist <- as.dist(df_mat)

#Hierarchical clustering
hclust_result <- hclust(df_dist, method = 'average')

#Cut off a cluster number
cut_tree <- cutree(hclust_result, k=cluster_count)

df_groups <- setDT(as.data.frame(cut_tree),keep.rownames = TRUE)
colnames(df_groups) = c('sample', 'cluster')

df_combined <- cbind(df_ppcg, df_groups[,2])
colnames(df_combined) <- c("Sample_ID", "Cluster")

write.table(df_combined, file = paste0("CA_umap.txt"), sep="\t",quote=F, row.names = FALSE)
