source("functions_subtyping.R")

method = "ae"

df_feat <- read.csv("PPCG.txt", sep = "\t")

df_feat <- update_column_names(df_feat)
df_meth <- read.csv(paste0("CA_",method,".txt"), sep = "\t")

one_count <- nrow(df_meth[df_meth$Cluster == 1,])
two_count <- nrow(df_meth[df_meth$Cluster == 2,])

# Set sample name list
df_feat_samples <- df_feat[1]

# Declare normalisation method
normalise <- function(x){(x-min(x))/(max(x)-min(x))}

# Apply normalisation to each column
df_feat_scaled <- sapply(df_feat[-1], normalise)
df_feat_restored <- cbind(df_feat_samples,df_feat_scaled)
df_feat_filtered <- df_feat_restored

feature_list <- melt(df_feat_filtered, id.vars = "Sample_ID", variable.name = "feature", value.name = "value")

# Conversion to factor necessary to maintain order for GGPlot
feature_list$Sample_ID <- factor(feature_list$Sample_ID, levels=unique(feature_list$Sample_ID))

one_list <- feature_list[(feature_list$Sample_ID %in% df_meth[df_meth$Cluster == 1,1]),]
two_list <- feature_list[(feature_list$Sample_ID %in% df_meth[df_meth$Cluster == 2,1]),]

one_plot <- ggplot(one_list, aes(x = Sample_ID, feature, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="#4169E1") +
  xlab(paste0("1 (",one_count,")")) +
  ylab("Summary measurement") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
  )

two_plot <- ggplot(two_list, aes(x = Sample_ID, feature, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="#4169E1") +
  xlab(paste0("2 (",two_count,")")) +
  ylab("Summary measurement") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
  )

gt1 <- ggplotGrob(one_plot)
gt2 <- ggplotGrob(two_plot)

com_plot <- grid.arrange(arrangeGrob(gt1, ncol=1, nrow=1),arrangeGrob(gt2, ncol=1, nrow=1),widths=c(3,2))
com_plot

ggsave(paste0("PPCG_",method,"_heatmap.png"),plot=com_plot)
