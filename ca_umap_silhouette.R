umap_sil <- read.csv("umap_silhouette.txt", sep = "\t", header=F)
colnames(umap_sil) <- c("Count")

umap_sil$Count <- as.integer(umap_sil$Count)

ct_1 <- 0
ct_2 <- 0
ct_3 <- 0
ct_4 <- 0
ct_5 <- 0
ct_6 <- 0
ct_7 <- 0
ct_8 <- 0
ct_9 <- 0
ct_10 <- 0

ct_1 <- length(umap_sil[umap_sil$Count == 1,])
ct_2 <- length(umap_sil[umap_sil$Count == 2,])
ct_3 <- length(umap_sil[umap_sil$Count == 3,])
ct_4 <- length(umap_sil[umap_sil$Count == 4,])
ct_5 <- length(umap_sil[umap_sil$Count == 5,])
ct_6 <- length(umap_sil[umap_sil$Count == 6,])
ct_7 <- length(umap_sil[umap_sil$Count == 7,])
ct_8 <- length(umap_sil[umap_sil$Count == 8,])
ct_9 <- length(umap_sil[umap_sil$Count == 9,])
ct_10 <- length(umap_sil[umap_sil$Count == 10,])

hist(umap_sil$Count)

print(paste0("1: ",ct_1))
print(paste0("2: ",ct_2))
print(paste0("3: ",ct_3))
print(paste0("4: ",ct_4))
print(paste0("5: ",ct_5))
print(paste0("6: ",ct_6))
print(paste0("7: ",ct_7))
print(paste0("8: ",ct_8))
print(paste0("9: ",ct_9))
print(paste0("10: ",ct_10))