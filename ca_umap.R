rm(list = ls())

library(umap)
library(factoextra)
source("functions_subtyping.R")

produce_umap_plot <- function(data, title, x=NULL, y=NULL, sl=NULL, inter=NULL) {
  g <- ggplot(data, aes(x=V1, y=V2)) + 
    geom_point(size=0.25) +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")
    )
  
  # if (!is.null(x)) {
  #   g <- g + geom_vline(xintercept = x, colour="red")
  # } else if (!is.null(y)) {
  #   g <- g + geom_hline(yintercept = y, colour="red")
  # } else if ((!is.null(sl))|(!is.null(inter))) {
  #   g <- g + geom_abline(slope = sl, intercept = inter, colour="red")
  # }
  return(g)
}

for (i in 1:100) {
  
  set.seed(i)
  #data load and preparation
  df_feat <- read.csv("PPCG.txt", sep = "\t")
  
  #Remove sample ID column and set the row names to those values.
  df_samples <- df_feat[,1]
  rownames(df_feat) <- df_feat[,1]
  df_feat <- df_feat[,-1]
  df_feat <- scale(df_feat)
  
  data_umap <- umap(df_feat)
  data_umap_plot <- data_umap$layout
  
  #p <- produce_umap_plot(as.data.frame(data_umap_plot),"PPCG")
  #p
  
  #Determine how many cluster based on within sum of squares
  vis_plot <- fviz_nbclust(data_umap_plot, kmeans, method="silhouette") + labs(title="Silhouette - kmeans")
  ggsave(paste0("umap_optimal_",i,".png"),plot=vis_plot)
  
  cluster_count = 2
  clust <- kmeans(data_umap_plot, centers = cluster_count, nstart = 10, iter.max = 100)
  
  data_clustering <- cbind(df_samples, clust$cluster)
  
  colnames(data_clustering) <- c("Sample_ID","Cluster")
  write.table(data_clustering, file = paste0("CA_umap",i,".txt"), sep="\t",quote=F, row.names = FALSE)
}
