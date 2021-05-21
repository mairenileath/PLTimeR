source("functions_subtyping.R")

feature_names <- read.csv(paste0("PPCG_feature_names.txt"), sep = "\t", header = F)
colnames(feature_names) <- c("Feature")

# df_tot <- read.csv(paste0("PPCG_feature_proportion_details.txt"), sep = "\t")
# colnames(df_tot) <- c("Feature", "PPCG")

df_ft1 <- read.csv(paste0("PPCG_Clust_FinType1_feature_proportion_details.txt"), sep = "\t")
colnames(df_ft1) <- c("Feature", "FinType1")

df_ft2 <- read.csv(paste0("PPCG_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_ft2) <- c("Feature", "FinType2")

df_arbs <- read.csv(paste0("PPCG_CA_arbs_feature_proportion_details.txt"), sep = "\t")
colnames(df_arbs) <- c("Feature", "arbs")

df_tm1 <- read.csv(paste0("PPCG_CA_tm_ppcg1_feature_proportion_details.txt"), sep = "\t")
colnames(df_tm1) <- c("Feature", "tm1")

df_tm2 <- read.csv(paste0("PPCG_CA_tm_ppcg2_feature_proportion_details.txt"), sep = "\t")
colnames(df_tm2) <- c("Feature", "tm2")

df_typ <- read.csv(paste0("PPCG_Clust_Type_feature_proportion_details.txt"), sep = "\t")
colnames(df_typ) <- c("Feature", "type")

df_pca <- read.csv(paste0("PPCG_CA_pca_feature_proportion_details.txt"), sep = "\t")
colnames(df_pca) <- c("Feature", "pca")

df_tsne <- read.csv(paste0("PPCG_CA_tsne_feature_proportion_details.txt"), sep = "\t")
colnames(df_tsne) <- c("Feature", "tsne")

df_umap <- read.csv(paste0("PPCG_CA_umap_feature_proportion_details.txt"), sep = "\t")
colnames(df_umap) <- c("Feature", "umap")

df_som <- read.csv(paste0("PPCG_CA_som_feature_proportion_details.txt"), sep = "\t")
colnames(df_som) <- c("Feature", "som")

df_ae <- read.csv(paste0("PPCG_CA_ae_feature_proportion_details.txt"), sep = "\t")
colnames(df_ae) <- c("Feature", "ae")

#df <- merge(feature_names,df_tot,by="Feature",all = TRUE)

df <- merge(df_ft1,df_ft2,by="Feature",all = TRUE)
df <- merge(df,df_arbs,by="Feature",all = TRUE)
df <- merge(df,df_tm1,by="Feature",all = TRUE)
df <- merge(df,df_tm2,by="Feature",all = TRUE)
df <- merge(df,df_typ,by="Feature",all = TRUE)
df <- merge(df,df_pca,by="Feature",all = TRUE)
df <- merge(df,df_tsne,by="Feature",all = TRUE)
df <- merge(df,df_umap,by="Feature",all = TRUE)
df <- merge(df,df_som,by="Feature",all = TRUE)
df <- merge(df,df_ae,by="Feature",all = TRUE)

df[is.na(df)] <- "N"

df <- df[df$FinType1 != "N" | df$FinType2 != "N" | df$arbs != "N" | df$tm1 != "N" | df$tm2 != "N" | df$type != "N" | df$pca != "N" | df$tsne != "N" | df$umap != "N" | df$som != "N" | df$ae != "N",]

df[df == "A"] <- "A"
df[df == "B"] <- "B"
df[df == "N"] <- "Neither"

feature_names$ordering <- 1:nrow(feature_names)

df <- merge(feature_names,df,by="Feature")
df <- df[order(df$ordering),]

df <- df[c(1,3:13)]

#df <- df[feature_names$Feature]
#df$Feature <- factor(df$Feature, levels=feature_names)

df_melt <- melt(df, id.vars = "Feature", variable.name = "methods", value.name = "Significant")

# Conversion to factor necessary to maintain order for GGPlot
df_melt$Feature <- factor(df_melt$Feature, levels=unique(df_melt$Feature))

pl <- ggplot(df_melt, aes(x = Feature, methods)) + 
  geom_tile(aes(fill = Significant, width = 0.9, height = 0.9)) +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "WhiteSmoke")) +
  xlab("Summary Measurements") +
  ylab("Methods") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pl

ggsave(paste0("PPCG_method_heatmap.png"),plot = pl)
