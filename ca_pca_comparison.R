library(mclust)

print_cluster_counts <- function(data, title) {
  c1 <- nrow(data[data$Cluster == "1",])
  c2 <- nrow(data[data$Cluster == "2",])
  print(paste(title, "C1", c1))
  print(paste(title, "C2", c2))
}

convert_cluster_names <- function(data,flip) {
  if (flip) { 
    data[data$Cluster == 1,2] <- "B"
    data[data$Cluster == 2,2] <- "A" 
  } else {
    data[data$Cluster == 1,2] <- "A"
    data[data$Cluster == 2,2] <- "B" 
  }
  return(data)
}

ca_pca_1 <- read.csv("CA_pca1.txt", sep = "\t")
ca_pca_2 <- read.csv("CA_pca2.txt", sep = "\t")
ca_pca_3 <- read.csv("CA_pca3.txt", sep = "\t")
ca_pca_4 <- read.csv("CA_pca4.txt", sep = "\t")
ca_pca_5 <- read.csv("CA_pca5.txt", sep = "\t")
ca_pca_6 <- read.csv("CA_pca6.txt", sep = "\t")
ca_pca_7 <- read.csv("CA_pca7.txt", sep = "\t")
ca_pca_8 <- read.csv("CA_pca8.txt", sep = "\t")
ca_pca_9 <- read.csv("CA_pca9.txt", sep = "\t")
ca_pca_10 <- read.csv("CA_pca10.txt", sep = "\t")

print_cluster_counts(ca_pca_1, "1")
print_cluster_counts(ca_pca_2, "2")
print_cluster_counts(ca_pca_3, "3")
print_cluster_counts(ca_pca_4, "4")
print_cluster_counts(ca_pca_5, "5")
print_cluster_counts(ca_pca_6, "6")
print_cluster_counts(ca_pca_7, "7")
print_cluster_counts(ca_pca_8, "8")
print_cluster_counts(ca_pca_9, "9")
print_cluster_counts(ca_pca_10, "10")

c_1 <- ca_pca_1$Cluster
c_2 <- ca_pca_2$Cluster
c_3 <- ca_pca_3$Cluster
c_4 <- ca_pca_4$Cluster
c_5 <- ca_pca_5$Cluster
c_6 <- ca_pca_6$Cluster
c_7 <- ca_pca_7$Cluster
c_8 <- ca_pca_8$Cluster
c_9 <- ca_pca_9$Cluster
c_10 <- ca_pca_10$Cluster

#comp_tsn_plot <- comparison_heatmap(ca_pca_3)

options <- list(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10)

for (i in 1:length(options)) {
  for (j in 1:length(options)) {
    a <- options[[i]]
    #print(a)
    b <- options[[j]]
    #print(b)
    value <- adjustedRandIndex(a,b)
    print(paste0("C",i," - C",j,": ",value))
  }
}

write.table(ca_pca_1, file = "CA_pca.txt", sep="\t",quote=F, row.names=FALSE)
