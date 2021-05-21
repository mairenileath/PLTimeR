library(data.table)
library(factoextra)
source("functions_subtyping.R")

run_pca <- function() {
  
df_feat <- read.csv("PPCG.txt", sep = "\t")
df_feat <- update_column_names(df_feat)

#Remove sample ID column and set the row names to those values.
rownames(df_feat) <- df_feat[,1]
df_feat <- df_feat[,-1]

#Run pca, centering and scaling
feat.pca <- prcomp(df_feat, center=TRUE, scale=TRUE)
summary(feat.pca)

#Draw screeplot
screeplot(feat.pca, type="l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col = "red", lty=5)

#Draw cumulative variance plot
cumpro <- cumsum(feat.pca$sdev^2 / sum(feat.pca$sdev^2))
p_cumvar <-  plot(cumpro[0:99], xlab="Principal component", ylab = "Amount of explained variance")
abline(v = 62, col = "blue", lty=5)
abline(h = 0.85, col="blue", lty=5)

#ggsave("plot_pca_cumvar.png",plot=p_cumvar)

#Sample plot of PC1 and PC2
#plot(feat.pca$x[,1], feat.pca$x[,2], xlab = "PC1", ylab = "PC2", main = "PC1 / PC2")

feat.var <- get_pca_var(feat.pca)
feat_var_contrib <- feat.var$contrib

#Remove PCs to get 85% attribution
feat_var_contrib <- feat_var_contrib[,1:62]
feat_var_contrib <- t(feat_var_contrib)

colsums <- colSums(feat_var_contrib)

colsums <- sort(colsums, decreasing = TRUE)
colsums <- colsums[1:40]

#print(colsums)

#Convert Named num list to 
reduced_feat <- names(colsums)
feat_top_15 <- names(colsums[1:15])

#Filter PPCG columns by the ones identified
df_feat <- subset(df_feat, select = c(reduced_feat))

# #Re-add Sample_ID column
setDT(df_feat, keep.rownames = TRUE)
colnames(df_feat)[1] <- "Sample_ID"

#Identify optimal number of clusters
fviz_nbclust(scale(df_feat[,-1]), kmeans, method="silhouette")

cluster_count <- 2
out_k <- kmeans(df_feat[,-1],centers = cluster_count, nstart=10, iter.max = 100)


samples_clustered <- cbind(df_feat$Sample_ID, out_k$cluster)
colnames(samples_clustered) <- c("Sample_ID", "Cluster")

df_feat_clustered <- merge(df_feat, samples_clustered, by="Sample_ID")

#row is 
df_pca_sum <- data.frame(matrix(vector(),0,5,dimnames=list(c(), c("ID","Feature","Type","Count","PVal"))), stringsAsFactors = F)

for (i in 1:length(feat_top_15)) {
  feat <- feat_top_15[i]
  #print(paste0("Name:",feat))
  
  
  df_feat_A <- df_feat_clustered[df_feat_clustered$Cluster == 1,]
  df_feat_B <- df_feat_clustered[df_feat_clustered$Cluster == 2,]
  
  #Get count of all positive and negatives for a column
  col_values_A <- select(df_feat_A,feat)
  col_values_B <- select(df_feat_B,feat)
  #print(paste0(i,col_values))
  count_A <- sum(col_values_A == 1)
  count_B <- sum(col_values_B == 1)
  
  row_A <- data.frame(paste0(feat,"_A"), feat, "A", count_A, 0)
  colnames(row_A) <- c("ID", "Feature", "Type", "Count", "PVal")
  df_pca_sum <- rbind(df_pca_sum, row_A)
  colnames(df_pca_sum) <- c("ID", "Feature", "Type", "Count", "PVal")
  row_B <- data.frame(paste0(feat,"_B"), feat, "B", count_B, 0)
  colnames(row_B) <- c("ID", "Feature", "Type", "Count", "PVal")
  df_pca_sum <- rbind(df_pca_sum, row_B)
  colnames(df_pca_sum) <- c("ID", "Feature", "Type", "Count", "PVal")
}

df_pca_sum$Feature <- factor(df_pca_sum$Feature, levels=unique(df_pca_sum$Feature))

pl <- ggplot(data = df_pca_sum, aes(x = Feature, y = Count, fill = Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_bar(position=position_fill()) +
  coord_flip() +
  theme_bw() +
  ylab("PCA assigned sample count") +
  xlab("Top 15 Features") +
  scale_x_discrete(limits=rev) + 
  scale_y_continuous(breaks = seq(0,14,by=1)) +
  theme(
      axis.text.x = element_text(size=rel(1.5)),
      axis.text.y = element_text(size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.5)),
      axis.title.y = element_text(size=rel(1.5)),
      legend.position = c(0.9,0.9)
)
pl

ggsave("pca_top_15.png",plot = pl)

return(samples_clustered)
}

pca1 <- run_pca()
pca2 <- run_pca()
pca3 <- run_pca()
pca4 <- run_pca()
pca5 <- run_pca()
pca6 <- run_pca()
pca7 <- run_pca()
pca8 <- run_pca()
pca9 <- run_pca()
pca10 <- run_pca()

##Output PCA CA file.
write.table(pca1, file = "CA_pca1.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca2, file = "CA_pca2.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca3, file = "CA_pca3.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca4, file = "CA_pca4.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca5, file = "CA_pca5.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca6, file = "CA_pca6.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca7, file = "CA_pca7.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca8, file = "CA_pca8.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca9, file = "CA_pca9.txt", sep="\t",quote=F, row.names=FALSE)
write.table(pca10, file = "CA_pca10.txt", sep="\t",quote=F, row.names=FALSE)
