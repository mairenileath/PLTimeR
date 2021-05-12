rm(list = ls())

library(stats)
library(ggrepel)
source("functions_subtyping.R")

plot_tm_proportions <- function(name,samples_count,scale_min,scale_max,hd_included, isDiscontinuous, d_split=0.05, d_start=-0.25, d_end=0, d_int=0.005) {
  
  data <- read.csv(paste0(name,"_wgd_ordering_plot_file.txt"), sep = "\t")
  merge <- read.csv(paste0(name,"_mergedseg.txt"), sep = "\t")
  
  merge[,c("ID_cna")] <- paste0(substr(merge[,c("CNA")],2,nchar(merge[,c("CNA")])),"_chr",merge[,c("chr")],"_",merge[,c("startpos")],"_",merge[,c("endpos")])
  
  data <- data[data$ID != "Whole_Genome_Duplication",]
  
  data[,c("ID_cna")] <- paste0(data[,c("CNA")],"_",data[,c("ID")])
  
  unique_cna_name <- unique(data$ID_cna)
  
  df_tm_summary <- data.frame(matrix(vector(),0,8,dimnames=list(c(), c("ID", "Name", "CNA", "Lower", "Order", "Upper", "Count", "Proportion"))), stringsAsFactors = F)
  
  for (i in 1:length(unique_cna_name)) {
    
    name_split <- strsplit(unique_cna_name[i],"_")
    name_cna <- name_split[[1]][1]
    name_chr <- as.integer(substr(name_split[[1]][2],4,nchar(name_split[[1]][2])))
    name_start <- as.integer(name_split[[1]][3])
    name_end <- as.integer(name_split[[1]][4])
    
    modified_name <- set_gene_name(name_chr,name_start,name_end)
    #data[data$ID == unique_cna_name[i],c("gene_names")] <- modified_name
    start_pos_mb <- format(round((as.integer(name_start)/1000000),0), nsmall = 0)
    end_pos_mb <- format(round((as.integer(name_end)/1000000),0), nsmall = 0)
    
    ID_val <- unique_cna_name[i]
    #Name_val <- paste0(name_chr,":",start_pos_mb,"Mb-",end_pos_mb, "Mb", ifelse(modified_name!="",paste0(" (",modified_name,")"),""))
    Name_val <- paste0(name_chr,":",start_pos_mb,"-",end_pos_mb,ifelse(modified_name!="",paste0("(",modified_name,")"),""))
    
    cna_data <- data[data$ID_cna == unique_cna_name[i],]
    
    Lower_val <- quantile(cna_data$value, prob=c(.25))
    Order_val <- median(cna_data$value)
    Upper_val <-quantile(cna_data$value, prob=c(.75))
    
    Count_val <- nrow(merge[merge$ID_cna == unique_cna_name[i],])
    Proportion_val <- (Count_val/samples_count)
    
    if (Proportion_val >= 0.15) {
      
      if (Name_val == "23:1-155") {
        Name_val <- ""
      } else {
        Name_val <- Name_val
      }
    } else {
      Name_val <- ""
    }
    
    
    if (Proportion_val >= 0.05) {
      df_tm_summary <- rbind(df_tm_summary, data.frame(ID_val, Name_val, name_cna, Lower_val, Order_val, Upper_val, Count_val, Proportion_val))
    }
    
  
  }
  colnames(df_tm_summary) <- c("ID", "Name", "CNA", "Lower", "Order", "Upper", "Count", "Proportion") 
  write.table(df_tm_summary, file = paste0("prop_summary_",name,".txt"), sep="\t",quote=F, row.names = FALSE)
  
  #point colours
  if (hd_included){
    #colours "tomato","medium seagreen","dodgerblue")
    p_colours <- c("#FF6347","#1E90FF","#3CB371")
  } else {
    p_colours <- c("#FF6347","#3CB371")
  }
  
  #text colours
  if (hd_included){
    #colours "tomato","medium seagreen","dodgerblue")
    t_colours <- c("#8B0000","#000080","#006400")
  } else {
    t_colours <- c("#8B0000","#006400")
  }
  
  df_tm_summary$facet <- df_tm_summary$Order < d_split
  
  p <- ggplot() + 
    geom_pointrange(data=df_tm_summary, aes(xmin = -Upper, xmax = -Lower, x = -Order, y = Proportion, col = CNA)) +
    labs(x="Timing Scale",y="Proportion of samples") +
    ylim(0,1) +
    theme_bw() +
    scale_color_manual(values=p_colours) +
    scale_fill_manual(values=p_colours) + 
    geom_label_repel(data=df_tm_summary,aes(x = -Order, y = Proportion,label=Name, col = CNA),max.overlaps = Inf,box.padding=0.5,point.padding=0.5)
    if (isDiscontinuous) {
      p <- p + facet_grid(.~facet, scales = "free", space = "free") +
        scale_x_continuous(breaks = seq(d_start, d_end, d_int)) + 
        theme(strip.text.x = element_blank())
    }

    return(p)
}

# p_uk_1 <- plot_tm_proportions("PPCGUKA",79,-0.25,0,F,T,0.05,-0.25,0,0.005)
# p_uk_2 <- plot_tm_proportions("PPCGUKB",79,-0.25,0,F,F)
# com_uk_plot <- grid.arrange(p_uk_1, p_uk_2, nrow = 2)
# 
# p_ppcg_1 <- plot_tm_proportions("PPCGA",431,-0.25,0,T,F)
# p_ppcg_2 <- plot_tm_proportions("PPCGB",138,-0.25,0,F,T,0.10,-0.20,0,0.01)
# com_ppcg_plot1 <- grid.arrange(p_ppcg_1, p_ppcg_2, nrow = 2)
# 
# p_ppcg_3 <- plot_tm_proportions("PPCGC",249,-0.25,0,T,T,0.15,-0.25,0,0.01)
# com_ppcg_plot2 <- grid.arrange(p_ppcg_3, p_ppcg_1, nrow = 2)

#ggsave("PPCGUK_prop_plot.png", plot = com_uk_plot)
#ggsave("PPCG_prop_plot1.png", plot = com_ppcg_plot1)
#ggsave("PPCG_prop_plot2.png", plot = com_ppcg_plot2)


p_ppcgl_1 <- plot_tm_proportions("PPCGAl",491,-0.25,0,T,F)
p_ppcgl_2 <- plot_tm_proportions("PPCGBl",327,-0.25,0,F,F)
com_ppcgl_plot1 <- grid.arrange(p_ppcgl_1, p_ppcgl_2, nrow = 2)

#ggsave("PPCGl_prop_plot1.png", plot = com_ppcgl_plot1)
