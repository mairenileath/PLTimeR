source("functions_subtyping.R")

feature_names <- read.csv(paste0("PPCG_feature_names.txt"), sep = "\t", header = F)
colnames(feature_names) <- c("Feature")

df_tot <- read.csv(paste0("PPCG_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_tot) <- c("Feature", "PPCG")

df_aus <- read.csv(paste0("PPCGAustralia_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_aus) <- c("Feature", "Australia")

df_can <- read.csv(paste0("PPCGCanada_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_can) <- c("Feature", "Canada")

df_den <- read.csv(paste0("PPCGDenmark_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_den) <- c("Feature", "Denmark")

df_fra <- read.csv(paste0("PPCGFrance_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_fra) <- c("Feature", "France")

df_ger <- read.csv(paste0("PPCGGermany_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_ger) <- c("Feature", "Germany")

df_uk <- read.csv(paste0("PPCGUK_Clust_FinType2_feature_proportion_details.txt"), sep = "\t")
colnames(df_uk) <- c("Feature", "UK")

#df <- merge(feature_names,df_tot,by="Feature",all = TRUE)
df <- merge(df_tot,df_aus,by="Feature",all = TRUE)
df <- merge(df,df_can,by="Feature",all = TRUE)
df <- merge(df,df_den,by="Feature",all = TRUE)
df <- merge(df,df_fra,by="Feature",all = TRUE)
df <- merge(df,df_ger,by="Feature",all = TRUE)
df <- merge(df,df_uk,by="Feature",all = TRUE)

df[is.na(df)] <- "N"

df <- df[df$PPCG != "N" | df$Australia != "N" | df$Canada != "N" | df$Germany != "N" | df$UK != "N",]

df[df == "A"] <- "A"
df[df == "B"] <- "B"
df[df == "N"] <- "Neither"

feature_names$ordering <- 1:nrow(feature_names)

df <- merge(feature_names,df,by="Feature")
df <- df[order(df$ordering),]

df <- df[c(1,3:9)]

#df <- df[feature_names$Feature]
#df$Feature <- factor(df$Feature, levels=feature_names)

df_melt <- melt(df, id.vars = "Feature", variable.name = "regions", value.name = "Significant")

# Conversion to factor necessary to maintain order for GGPlot
df_melt$Feature <- factor(df_melt$Feature, levels=unique(df_melt$Feature))

pl <- ggplot(df_melt, aes(x = Feature, regions)) + 
  geom_tile(aes(fill = Significant, width = 0.9, height = 0.9)) +
  theme_bw() +
  #coord_flip() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "WhiteSmoke")) +
  xlab("Summary Measurements") +
  ylab("Regions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

pl

ggsave(paste0("PPCG_region_heatmap.png"),plot = pl)
