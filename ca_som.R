#https://www.polarmicrobes.org/tutorial-self-organizing-maps-in-r/

library(kohonen)
library(factoextra)
source("functions_subtyping.R")

normalise <- function(x){(x-min(x))/(max(x)-min(x))}
#Visualization of distributions of different variables.
property_plots <- function(som.model,clust,run) {
  for (i in 1:99) {
    png(paste0(run,"-",i,".png"))
    plot_prop <- plot(som.model, type = "property", property = getCodes(som.model)[,i], main=colnames(getCodes(som.model))[i], palette.name=coolBlueHotRed)
    add.cluster.boundaries(som.model, clust$cluster)
    dev.off()
  }
}
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}


for (j in 1:100) {
  
  df_feat <- read.csv("PPCG.txt", sep = "\t")
  
  # Apply normalisation to each column
  feat.data <- as.matrix(sapply(df_feat[-1], normalise))
  
  run <- j
  
  feat.grid <- somgrid(xdim = 12, ydim = 12, topo = "hexagonal", toroidal = TRUE)
  som.model <- som(feat.data, grid=feat.grid, rlen=500, radius=4) # , alpha = c(0.05, 0.01), rlen=500, keep.data = TRUE, radius = 1
  
  som.events <- som.model$codes[[1]]
  som.dist <- as.matrix(dist(som.events))
  
  #Check on training progress - distance should reach a minimum plateau, else more iterations required
  plot(som.model, type='changes')
  
  #Mapping plot showing
  plot(som.model, type = "mapping")
  
  #Distance between each node and its neighbours
  plot(som.model, type="dist.neighbours")
  
  #Determine how many cluster based on within sum of squares
  vis_plot <- fviz_nbclust(som.events, kmeans, method="silhouette") + labs(title="Silhouette - kmeans")
  
  ggsave(paste0("som_optimal_",run,".png"),plot=vis_plot)
  
  cluster_count = 2
  
  clust <- kmeans(som.events, centers = cluster_count, nstart = 10, iter.max = 100)
  
  png(paste0("som_cluster_",run,".png"))
  plot(som.model, type = "mapping")
  add.cluster.boundaries(som.model, clust$cluster)
  dev.off()
  
  property_plots(som.model,clust,run)
  
  cluster_assignment <- clust$cluster[som.model$unit.classif]
  df_feat$Cluster <- cluster_assignment
  samples_clustered <- subset(df_feat, select = c("Sample_ID", "Cluster"))
  
  ca_data_a_count <- nrow(samples_clustered[samples_clustered$Cluster == 1,])
  ca_data_b_count <- nrow(samples_clustered[samples_clustered$Cluster == 2,])
  
  print(paste0("A: ",ca_data_a_count))
  print(paste0("B: ",ca_data_b_count))
  
  write.table(samples_clustered, file = paste0("CA_som",run,".txt"), sep="\t",quote=F, row.names=FALSE)
}



