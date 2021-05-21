library(tidyverse)
library(gridExtra)
library(reshape2)
library(stats)

genelist <- read.csv("driver_mutations.txt", sep = "\t")
colnames(genelist) <- c("gene_name","gene_chr","gene_start","gene_end")

set_gene_name <- function(region_chr, region_start, region_end) {
  modified_name <- ""
  
  for (i in 1:nrow(genelist)) {
    
    gene_chr <- as.integer(genelist$gene_chr[i])

    if (gene_chr == region_chr) {
      gene_start <- as.integer(genelist$gene_start[i])
      gene_end <- as.integer(genelist$gene_end[i])
      gene_name <- genelist$gene_name[i]
      
      if (!((gene_start > region_start & gene_start > region_end & gene_end > region_start & gene_end > region_end) | (gene_start < region_start & gene_start < region_end & gene_end < region_start & gene_end < region_end))) {
        #If name is currently not empty add (; + space)
        if (nchar(modified_name) > 0) {
          modified_name <- paste0(modified_name,"; ")
        }
        #Add gene
        modified_name <- paste0(modified_name,gene_name)
      }
    }
  }
  return(modified_name)
}


feature_name_prettify <- function(old_name) {
  
  new_name <- ""
  
  if (old_name == "number_of_snvs") {
    new_name <- "SNVs"
  } else if (old_name == "number_of_indels") {
    new_name <- "InDels"
  } else if (old_name == "number_ins") {
    new_name <- "Insertions (InDels)"
  } else if (old_name == "number_del") {
    new_name <- "Deletions (InDels)"
  } else if (old_name == "number_of_rearrangements") {
    new_name <- "SVs"
  } else if (old_name == "rearrangements_inversion") {
    new_name <- "Inversions (SVs)"
  } else if (old_name == "rearrangements_deletion") {
    new_name <- "Deletions (SVs)"
  } else if (old_name == "rearrangements_tandemDuplication") {
    new_name <- "Tandem Duplications (SVs)"
  } else if (old_name == "rearrangements_translocation") {
    new_name <- "Translocations (SVs)"
  } else if (old_name == "ploidy") {
    new_name <- "Ploidy"
  } else if (old_name == "pga_clonal") {
    new_name <- "PGA (clonal)"
  } else if (old_name == "pga_subclonal") {
    new_name <- "PGA (subclonal)"
  } else if (old_name == "pga_total") {
    new_name <- "PGA"
  } else if (old_name == "CT2") {
    new_name <- "Chromothripsis"
  } else if (old_name == "CT_prop") {
    new_name <- "Proportion of CT"
  } else if (old_name == "CT_per_sample") {
    new_name <- "CT count"
  } else if (old_name == "CT_max_size") {
    new_name <- "Max CT region"
  } else if (old_name == "kataegis") {
    new_name <- "Kataegis"
  } else if (old_name == "Sample_ID") {
    new_name <- "Sample_ID"
  } else {
    new_name <- set_cna_name(old_name)
  }
  return(new_name)
}

set_cna_name <- function(cna_name) {
  cna_split <- strsplit(cna_name,"\\.")
  cna_type <- cna_split[[1]][1]
  cna_chr <- as.integer(cna_split[[1]][2])
  cna_start <- as.integer(cna_split[[1]][3])
  cna_end <- as.integer(cna_split[[1]][4])

  
  genes_names <- set_gene_name(cna_chr,cna_start,cna_end)
  #start_pos_pret <- trimws(format(as.integer(cna_start),big.mark=",",scientific=FALSE))
  #end_pos_pret <- trimws(format(as.integer(cna_end),big.mark=",",scientific=FALSE))
  #mod_name <- paste0(cna_type, " ", cna_chr,":",start_pos_pret,"-",end_pos_pret, ifelse(genes_names!="",paste0(" (",genes_names,")"),""))
  
  cna_start_mb <- format(round((as.integer(cna_start)/1000000),2), nsmall = 2)
  cna_end_mb <- format(round((as.integer(cna_end)/1000000),2), nsmall = 2)
  
  mod_name <- paste0(cna_type, " ", cna_chr,":",cna_start_mb,"-",cna_end_mb, "Mb", ifelse(genes_names!="",paste0(" (",genes_names,")"),""))
  #mod_name <- paste0(cna_type, " ", cna_chr,":",cna_start_mb,"-",cna_end_mb, "Mb")
  
  return(mod_name)
}

update_column_names <- function(df_data) {
  
  col_features <- colnames(df_data)
  #print(col_features)
  for (i in 1:length(col_features)) {
    col_features[i] <- feature_name_prettify(col_features[i])
  }
  
  colnames(df_data) <- col_features
  
  return(df_data)
}

comparison_heatmap <- function(ca_data) {
  
  df_feat <- read.csv("PPCG.txt", sep = "\t")
  df_feat <- update_column_names(df_feat)
  
  #If var is 0 for a variable remove it.
  df_feat_var <- sapply(df_feat[-1], var)
  invalid_feat <- df_feat_var[df_feat_var == 0]
  invalid_names <- names(invalid_feat)
  df_feat <- df_feat[,!(names(df_feat) %in% invalid_names)]
  
  val_count <- function(feat_values) {
    count <- length(feat_values[feat_values != 0])
    return(count)
  }
  
  #If less than three samples with any value in this set remove.
  df_joined_val_count <- sapply(df_feat[-1], val_count)
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

  caa_list <- feature_list[(feature_list$Sample_ID %in% ca_data[ca_data$Cluster == 1,1]),]
  cab_list <- feature_list[(feature_list$Sample_ID %in% ca_data[ca_data$Cluster == 2,1]),]
  
  a_plot <- ggplot(caa_list, aes(x = Sample_ID, feature, fill = value)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="#4169E1") +
    xlab("Cluster A") +
    ylab("Summary measurement") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none"
    )
  
  b_plot <- ggplot(cab_list, aes(x = Sample_ID, feature, fill = value)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="#4169E1") +
    xlab("Cluster B") +
    ylab("Summary measurement") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank()
    )
  
  
  gt1 <- ggplotGrob(a_plot)
  gt2 <- ggplotGrob(b_plot)
  
  com_plot <- grid.arrange(arrangeGrob(gt1, ncol=1, nrow=1),arrangeGrob(gt2, ncol=1, nrow=1),widths=c(3,2))
  return(com_plot)
}

plot_tm_ordering = function(data, title)
{
  unique_cna_name <- unique(data$ID)
  data[,"gene_names"] <- NA
  data[,"ID_chr"] <- NA
  
  unique_cna_name <- unique_cna_name[unique_cna_name != "Whole_Genome_Duplication"]
  
  for (i in 1:length(unique_cna_name)) {
    
    #print(unique_cna_name[i])
    name_split <- strsplit(unique_cna_name[i],"_")
    name_chr <- as.integer(substr(name_split[[1]][1],4,nchar(name_split[[1]][1])))
    name_start <- as.integer(name_split[[1]][2])
    name_end <- as.integer(name_split[[1]][3])
    
    
    modified_name <- set_gene_name(name_chr,name_start,name_end)
    data[data$ID == unique_cna_name[i],c("gene_names")] <- modified_name
    #start_pos_pret <- trimws(format(as.integer(name_start),big.mark=",",scientific=FALSE))
    #end_pos_pret <- trimws(format(as.integer(name_end),big.mark=",",scientific=FALSE))
    
    start_pos_mb <- format(round((as.integer(name_start)/1000000),2), nsmall = 2)
    end_pos_mb <- format(round((as.integer(name_end)/1000000),2), nsmall = 2)
    
    data[data$ID == unique_cna_name[i],c("ID_chr")] <- paste0(name_chr,":",start_pos_mb,"Mb-",end_pos_mb, "Mb", ifelse(modified_name!="",paste0(" (",modified_name,")"),""))
  }
  
  #Adjust plots for readability, if >15 set to 15
  #data$value[data$value > 15] <- 15
  #data$value[data$value > 5] <- 5
  data$CNA[data$CNA == "GD"] <- "WGD"
  data[data$CNA == "WGD",c("ID_chr")] <- "Whole Genome Doubling"
  
  
  unique_cna <- unique(data[,c("CNA","ID_chr")])
  write.table(unique_cna, file = paste0("tm_",title,"_CNA_list.txt"), sep="\t",quote=F, row.names = F, col.names = F)
  
  HD_count <- nrow(data[data$CNA == "HD",])
  
  if (HD_count == 0){
    colours <- c("#CD5C5C","#1E90FF","#663399")
  } else {
    #colours "indianred","gold","dodgerblue","rebeccapurple")
    colours <- c("#CD5C5C","#FFD700","#1E90FF","#663399")
  }
  
  ggplot(data, aes(x = ID_chr, y = -value, col = CNA, fill = CNA)) +
    #geom_point() +
    geom_violin() +
    #geom_label(aes(label=ID_chr),hjust=0,vjust=0) +
    #geom_text(aes(x = 0, y = -value, label=ID_chr),data) +
    coord_flip() +
    labs(x="CNA event",y="Timing scale") +
    ggtitle(paste0("Ordering-",title)) +
    scale_color_manual(values=colours) +
    scale_fill_manual(values=colours)
}

plot_arbs_bars <- function(df_data,title) {
  
  #df_data <- update_column_names(df_data)
  
  if ("158" %in% title) {
    break_ticks <- c(-0.30,-0.25, -0.20, -0.15, -0.10, -0.05, 0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
  } else {
    break_ticks <- c(-0.50, -0.40, -0.30, -0.20, -0.10, 0.00, 0.10, 0.20, 0.30, 0.40, 0.50)
  }
  
  p <- ggplot(data=df_data, aes(x=reorder(sample, -obs_sim_adj), y=obs_sim_adj, fill=color)) +
    geom_bar(stat="identity", width=1) +
    ylab("Normalised Proportion") +
    xlab(title) +
    scale_fill_manual("Legend", labels=c("Enriched", "Indeterminate", "Depleted"), values = c("blue" = "#4169E1", "grey" = "#808080", "red" = "#FF4500")) +
    theme_classic() +
    scale_y_continuous(breaks = break_ticks) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=rel(1.5)),
          axis.title.y = element_text(size=rel(1.2)),
          axis.title.x = element_text(size=rel(1.2)),
          legend.position = "none")#+
          #legend.position = c(0.9,0.9)) #+
    #ggtitle(title)
  
  p <- p + geom_hline(yintercept = 0, color = "black")
  
  return(p)
}

#Our own confusion matrix method including fishers exact
confused_matrix <- function(title, pred_vec, ref_vec, inc_fisher) {
  
  comb <- cbind(pred_vec, ref_vec)
  comb <- as.data.frame(comb)
  colnames(comb) <- c("Pred","Ref")
  
  TL <- nrow(comb[comb$Pred == 1 & comb$Ref == 1,])
  TM <- nrow(comb[comb$Pred == 1 & comb$Ref == 2,])
  TR <- nrow(comb[comb$Pred == 1 & comb$Ref == 3,])
  ML <- nrow(comb[comb$Pred == 2 & comb$Ref == 1,])
  MM <- nrow(comb[comb$Pred == 2 & comb$Ref == 2,])
  MR <- nrow(comb[comb$Pred == 2 & comb$Ref == 3,])
  BL <- nrow(comb[comb$Pred == 3 & comb$Ref == 1,])
  BM <- nrow(comb[comb$Pred == 3 & comb$Ref == 2,])
  BR <- nrow(comb[comb$Pred == 3 & comb$Ref == 3,])
  
  cont <- matrix(c(TL,ML,BL,ML,MM,MR,TR,BM,BR), nrow=3, ncol=3)
  
  if (inc_fisher) {
    ftest <- fisher.test(cont)
    test_stat <- ftest$p.value
  } else {
    test_stat <- "NA"
  }
  
  print(paste0(title, " - ", test_stat))
  print(paste0(TL,"  ",TM,"  ",TR))
  print(paste0(ML,"  ",MM,"  ",MR))
  print(paste0(BL,"  ",BM,"  ",BR))
}