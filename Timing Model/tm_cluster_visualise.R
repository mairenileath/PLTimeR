rm(list = ls())

library(ggdendro)
source("functions_subtyping.R")

cohort_name = "PPCGl"

#Import data
df_distances <- read.csv(paste0(cohort_name,"_distance_summary.txt"), sep = "\t", header = TRUE, row.names = NULL)

#Converts to matrix
df_mat <- acast(df_distances, A~B, value.var="Distance", mean)
df_dist <- as.dist(df_mat)

#Hierarchical clustering
hclust_result <- hclust(df_dist, method = 'average')

hier_clust_plot <- ggdendrogram(hclust_result, rotate = FALSE, size = 2)
hier_clust_plot
ggsave(paste0(cohort_name,"_Hierarchical_Clustering_Plot.pdf"), plot = hier_clust_plot)