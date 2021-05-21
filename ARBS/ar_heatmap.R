rm(list = ls())

source("functions_subtyping.R")

df_feat <- read.csv("PPCG.txt", sep = "\t")

df_feat <- update_column_names(df_feat)

#Get list of arbs blue and red in order by obj_sim_adj

df_arbs <- read.csv("PPCG_Per_group_20kb_brp_prop_all_chr.csv")
names(df_arbs) = c("Sample_ID","color","brp_prop","obs_sim","obs_ci","obs_sim_adj")

#Sort by descending Obs_sim_adj
df_arbs <- df_arbs[order(-df_arbs$obs_sim_adj),]

#Filter to only most extreme blue or red
df_arbs <- df_arbs[df_arbs$color == "blue" | df_arbs$color == "red",]

#Get top 25 blue
df_arbs_blu <- df_arbs[df_arbs$color == "blue",]
df_arbs_blu <- df_arbs_blu[order(-df_arbs_blu$obs_sim_adj),]
df_arbs_blu <- df_arbs_blu[1:50,]

#Get top 25 red
df_arbs_red <- df_arbs[df_arbs$color == "red",]
df_arbs_red <- df_arbs_red[order(df_arbs_red$obs_sim_adj),]
df_arbs_red <- df_arbs_red[1:50,]
df_arbs_red <- df_arbs_red[order(-df_arbs_red$obs_sim_adj),]

df_arbs <- rbind(df_arbs_blu,df_arbs_red)

#Order data frame by samples 
df_joined <- inner_join(df_arbs[1], df_feat, by="Sample_ID")

#If var is 0 for a variable remove it.
df_joined_var <- sapply(df_joined[-1], var)
invalid_feat <- df_joined_var[df_joined_var == 0]
invalid_names <- names(invalid_feat)
df_feat <- df_joined[,!(names(df_joined) %in% invalid_names)]

val_count <- function(feat_values) {
  count <- length(feat_values[feat_values != 0])
  return(count)
}

#If less than three samples with any value in this set remove.
df_joined_val_count <- sapply(df_feat[-1], val_count)
#df_joined_val_count <- 
invalid_feat <- df_joined_val_count[df_joined_val_count < 3]
invalid_names <- names(invalid_feat)
df_feat <- df_feat[,!(names(df_feat) %in% invalid_names)]
  
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

enr_list <- feature_list[(feature_list$Sample_ID %in% df_arbs[df_arbs$color == "blue",1]),]
int_list <- feature_list[(feature_list$Sample_ID %in% df_arbs[df_arbs$color == "grey",1]),]
dep_list <- feature_list[(feature_list$Sample_ID %in% df_arbs[df_arbs$color == "red",1]),]

hm_enr_plot <- ggplot(enr_list, aes(x = Sample_ID, feature, fill = value)) + 
geom_tile() +
scale_fill_gradient(low="white", high="#008000") +
xlab("Enriched Samples") +
ylab("Summary measurement") +
theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      #axis.text.y = element_text(size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.2)),
      axis.title.y = element_text(size=rel(1.2)),
      legend.position = "none"
      )

hm_int_plot <- ggplot(int_list, aes(x = Sample_ID, feature, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="#008000") +
  xlab("Intermediate Samples") +
  ylab("Summary measurement") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
  )


hm_dep_plot <- ggplot(dep_list, aes(x = Sample_ID, feature, fill = value)) + 
  geom_tile() +
  #scale_fill_gradient(low="white", high="#4169E1") +
  scale_fill_gradient(low="white", high="#008000") +
  xlab("Depleted Samples") +
  ylab("Summary measurement") +
  labs(color = "Value") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=rel(1.2))
        )


gt1 <- ggplotGrob(hm_enr_plot)
gt2 <- ggplotGrob(hm_dep_plot)

com_plot <- grid.arrange(arrangeGrob(gt1, ncol=1, nrow=1),arrangeGrob(gt2, ncol=1, nrow=1),widths=c(3,2))
#com_plot <- grid.arrange(hm_enr_plot, hm_int_plot, hm_dep_plot, ncol=3)
com_plot

ggsave("ARBS_PPCG_heatmap.png",plot=com_plot)
