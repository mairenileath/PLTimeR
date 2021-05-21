setwd("C:\Users\willi\OneDrive\University\RP1\Cluster Analysis - Workspace")

source("functions_subtyping.R")

get_cluster_split <- function(ppcg, country, df) {

  ppcg_tot <- ppcg
  ppcg_reg <- ppcg[ppcg$Country == country, c("Sample_ID","Cluster")]
  
  all_count_tot <- nrow(ppcg_tot)
  all_count_A <- nrow(ppcg_tot[ppcg_tot$Cluster == 1,])
  all_count_B <- nrow(ppcg_tot[ppcg_tot$Cluster == 2,])
  
  reg_count_tot <- nrow(ppcg_reg)
  reg_count_A <- nrow(ppcg_reg[ppcg_reg$Cluster == 1,])
  reg_count_B <- nrow(ppcg_reg[ppcg_reg$Cluster == 2,])
  print(country)
  print(paste0("A: ", reg_count_A))
  print(paste0("B: ", reg_count_B))
  
  #Percentage of sample
  perc <- reg_count_A/(reg_count_A + reg_count_B)
  perc_all <- all_count_A/(all_count_A + all_count_B)
  
  print(paste0("Perc: ", perc))
  
  TL <- reg_count_A
  BL <- all_count_A
  TR <- reg_count_B
  BR <- all_count_B
  
  cont <- matrix(c(TL,BL,TR,BR), nrow=2, ncol=2)
  ftest <- fisher.test(cont)
  test_stat <- ftest$p.value
  
  print(paste0("Test: ", test_stat))
  
  # if (test_stat < 0.05) {
  #   if (perc > perc_all) {
  #     sig <- "A"
  #   } else {
  #     sig <- "B"
  #   }
  # } else {
  #   sig <- "N"
  # }
  # 
  if (country == "PPCG") {
    count_A <- all_count_A
    count_B <- all_count_B
  } else {
    count_A <- reg_count_A
    count_B <- reg_count_B
  }
  print("A")
  row_A <- data.frame(paste0(country,"_A"), paste0(country," (",(count_A + count_B),")"), "A", count_A, (count_A/(count_A + count_B)), test_stat)
  colnames(row_A) <- c("ID", "Region", "Type", "Count", "Prop", "PVal")
  df <- rbind(df, row_A)
  colnames(df) <- c("ID", "Region", "Type", "Count", "Prop", "PVal")
  
  row_B <- data.frame(paste0(country,"_B"), paste0(country," (",(count_A + count_B),")"), "B", count_B, (count_B/(count_A + count_B)), test_stat)
  colnames(row_B) <- c("ID", "Region", "Type", "Count", "Prop", "PVal")
  df <- rbind(df, row_B)
  colnames(df) <- c("ID", "Region", "Type", "Count", "Prop", "PVal")
  
  #rowSig <- data.frame(country, test_stat, sig)
  #df_sig <- rbind(df_sig, rowSig)
  
  return(df)
}

region <- read.csv("PPCG_region.txt", sep = "\t", header=FALSE)
colnames(region) <- c("Sample_ID","Country")

types <- read.csv("Clust_FinType.txt", sep = "\t")
region <- merge(region,types,by="Sample_ID")

ppcg_fra <- region[region$Country == "France",]
ppcg_ger <- region[region$Country == "Germany",]
ppcg_uki <- region[region$Country == "UK",]
ppcg_aus <- region[region$Country == "Australia",]
ppcg_can <- region[region$Country == "Canada",]
ppcg_den <- region[region$Country == "Denmark",] 

#write.table(ppcg_fra, file = paste0("PPCG_fra.txt"), sep="\t",quote=F)
#write.table(ppcg_ger, file = paste0("PPCG_ger.txt"), sep="\t",quote=F)
#write.table(ppcg_uki, file = paste0("PPCG_uki.txt"), sep="\t",quote=F)
#write.table(ppcg_aus, file = paste0("PPCG_aus.txt"), sep="\t",quote=F)
#write.table(ppcg_can, file = paste0("PPCG_can.txt"), sep="\t",quote=F)
#write.table(ppcg_den, file = paste0("PPCG_den.txt"), sep="\t",quote=F)

ppcg_fra <- ppcg_fra[, c("Sample_ID","Cluster")]
ppcg_ger <- ppcg_ger[, c("Sample_ID","Cluster")]
ppcg_uki <- ppcg_uki[, c("Sample_ID","Cluster")]
ppcg_aus <- ppcg_aus[, c("Sample_ID","Cluster")]
ppcg_can <- ppcg_can[, c("Sample_ID","Cluster")]
ppcg_den <- ppcg_den[, c("Sample_ID","Cluster")] 

df_plot <- data.frame(matrix(vector(),0,6,dimnames=list(c(), c("ID", "Region", "Type", "Count", "Prop", "PVal"))), stringsAsFactors = F)
df_sig <- data.frame(matrix(vector(),0,3,dimnames=list(c(), c("Region","PVal", "Sig"))), stringsAsFactors = F)


df_plot <- get_cluster_split(region,"PPCG",df_plot)
df_plot <- get_cluster_split(region,"Australia",df_plot)
df_plot <- get_cluster_split(region,"Canada",df_plot)
df_plot <- get_cluster_split(region,"Denmark",df_plot)
df_plot <- get_cluster_split(region,"France",df_plot)
df_plot <- get_cluster_split(region,"Germany",df_plot)
df_plot <- get_cluster_split(region,"UK",df_plot)







# if (test_stat < 0.05) {
#   if (perc > perc_all) {
#     sig <- "A"
#   } else {
#     sig <- "B"
#   }
# } else {
#   sig <- "N"
# }



# sig_row_fra <- data.frame(country, df_plot[df_plot$Country == "France", c("test_stat")], sig)
# colnames(sig_row_fra) <- c("Region","PVal", "Sig")
# sig_row_ger <- data.frame(country, df_plot[df_plot$Country == "Germany", c("test_stat")], sig)
# colnames(sig_row_ger) <- c("Region","PVal", "Sig")
# sig_row_uki <- data.frame(country, df_plot[df_plot$Country == "UK", c("test_stat")], sig)
# colnames(sig_row_uki) <- c("Region","PVal", "Sig")
# sig_row_aus <- data.frame(country, df_plot[df_plot$Country == "Australia", c("test_stat")], sig)
# colnames(sig_row_aus) <- c("Region","PVal", "Sig")
# sig_row_can <- data.frame(country, df_plot[df_plot$Country == "Canada", c("test_stat")], sig)
# colnames(sig_row_can) <- c("Region","PVal", "Sig")
# sig_row_den <- data.frame(country, df_plot[df_plot$Country == "Denmark", c("test_stat")], sig)
# colnames(sig_row_den) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_fra)
# colnames(df_sig) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_ger)
# colnames(df_sig) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_uki)
# colnames(df_sig) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_aus)
# colnames(df_sig) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_can)
# colnames(df_sig) <- c("Region","PVal", "Sig")
# df_sig <- rbind(df_sig, sig_row_den)
# colnames(df_sig) <- c("Region","PVal", "Sig")


# get_colo <- function(x) {
#   
#   if(x == "B") {
#     return ("#00BFC4")
#   } else if (x == "A") { 
#     return("#F8766D") 
#   } else {
#     return("black") 
#   }
# }
# #a <- sapply(df_sig$Sig, get_colo)

df_plot$Region <- factor(df_plot$Region, levels=unique(df_plot$Region))

pl <- ggplot(data = df_plot, aes(x = reorder(Region,-Region), y = Prop, fill = Type)) +
  geom_bar(stat="identity", position=position_fill()) +
  coord_flip() +
  theme_bw() +
  xlab("Region") +
  scale_y_continuous("Proportion of samples", breaks=c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00))
pl

pl
ggsave(paste0("PPCG_region_split.png"),plot = pl)