clusterGenerateDistanceSummary = function(cohort_name) {

#Import data
data <- read.csv(paste0(cohort_name,"_mixture_model_classification_1000its.txt"), sep = "\t", header = TRUE, row.names = NULL)
data <- subset(data, select = -row.names)

#Set up empty distances frame
df_distances <- data.frame(matrix(vector(),0,3,dimnames=list(c(), c("A", "B", "Distance"))), stringsAsFactors = F)

df_cohort <- read.csv(paste0(cohort_name,"_mergedseg.txt"), sep = "\t", header = TRUE)
df_cohort <- df_cohort[,1]
df_cohort_unique <- unique(df_cohort)
df_cohort_unique = substr(df_cohort_unique,0,13)

colnames(data) = df_cohort_unique

i_count <- 1
j_count <- 1

for (i in colnames(data)) {
  for (j in colnames(data)) {
    #if same column skip
   # if (i==j) {
      #continue
   # } else {
      #get vectors of first and second column for comparison
      a <- data[i]
      b <- data[j]
      #get count of matches of rows of the vectors for the distance value.
      distance <- (1000-sum(a==b))

      row <- data.frame("A"=i, "B"=j, "Distance"=distance)

      #df_filt <- filter(df_distances, (A == i & B == j) | (A == j & B == i))
      df_filt <- filter(df_distances, (A == i & B == j))

      if (!(nrow(df_filt) > 0)) {
        df_distances <- rbind(df_distances,row)
      }
   # }
      j_count = j_count + 1 
      #print(paste(i_count,j_count))
  }
  j_count <- 1
  i_count = i_count + 1
  
  #print(paste0(j_count, i_count))
}

write.table(df_distances, file = paste0(cohort_name,"_distance_summary.txt"), sep="\t",quote=F, row.names = FALSE)
}

clusterVisualizeAndGroup = function(cohort_name, cluster_count=2, hclust.method='average') {

  library(ggdendro)
  library(data.table)

  source("functions_subtyping.R")

  #Import data
  df_distances <- read.csv(paste0(cohort_name,"_distance_summary.txt"), sep = "\t", header = TRUE, row.names = NULL)

  #Converts to matrix
  df_mat <- acast(df_distances, A~B, value.var="Distance", mean)
  df_dist <- as.dist(df_mat)
  
  #Hierarchical clustering
  hclust_result <- hclust(df_dist, method = hclust.method)

  hier_clust_plot <- ggdendrogram(hclust_result, rotate = FALSE, size = 2)
  hier_clust_plot
  ggsave(paste0(cohort_name,"_Hierarchical_Clustering_Plot.pdf"), plot = hier_clust_plot)

  #Cut off a cluster number
  cut_tree <- cutree(hclust_result, k=cluster_count)

  df_groups <- setDT(as.data.frame(cut_tree),keep.rownames = TRUE)
  colnames(df_groups) = c('sample', 'cluster')

  #Get each unique cluster value
  clusters <- unique(df_groups$cluster)
  for (i in 1:length(clusters)) {
    cluster_samples <- df_groups[df_groups$cluster == clusters[i],]
    write.table(cluster_samples$sample, file = paste0(cohort_name,"_samples_c",clusters[i],".txt"), sep="\t",quote=F,row.names=FALSE,col.names=FALSE)
  }
}  
