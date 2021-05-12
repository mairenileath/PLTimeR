library(mclust)
source("functions_subtyping.R")

format_evo <- function() {
  evo <- read.csv("evotype_groups.csv")
  colnames(evo) <- c("PD_Code", "Metaclusters", "AR_group", "Ordering","Evotype")
  ppcg_pd <- read.csv("PPCG_PD_evotypes.txt", sep = "\t", header=FALSE)
  colnames(ppcg_pd) <- c("Sample_ID", "PD_Code")
  evo_results <- merge(evo, ppcg_pd, by="PD_Code")
  evo_results <- evo_results[,c(6,2,3,4,5)]
  write.table(evo_results, file = "evo_results.txt", sep="\t",quote=F, row.names = FALSE)
}

format_tm <- function() {
  ppcg_1 <- read.csv("PPCG_samples_c1.txt", sep = "\t", header=FALSE)
  colnames(ppcg_1) <- c("Sample_ID")
  ppcg_2 <- read.csv("PPCG_samples_c2.txt", sep = "\t", header=FALSE)
  colnames(ppcg_2) <- c("Sample_ID")
  ppcg_3 <- read.csv("PPCG_samples_c3.txt", sep = "\t", header=FALSE)
  colnames(ppcg_3) <- c("Sample_ID")

  ppcg_1$Cluster <- 1
  ppcg_2$Cluster <- 2
  ppcg_3$Cluster <- 3

  ppcg <- rbind(ppcg_1,ppcg_2)
  ppcg <- rbind(ppcg,ppcg_3)

  ppcg <- ppcg[order(ppcg$Sample_ID),]
  
  write.table(ppcg, file = "CA_tm_ppcg.txt", sep="\t",quote=F, row.names = FALSE)
  
  ppcguk_1 <- read.csv("PPCGUK_samples_c1.txt", sep = "\t", header=FALSE)
  colnames(ppcguk_1) <- c("Sample_ID")
  ppcguk_2 <- read.csv("PPCGUK_samples_c2.txt", sep = "\t", header=FALSE)
  colnames(ppcguk_2) <- c("Sample_ID")
  
  ppcguk_1$Cluster <- 1
  ppcguk_2$Cluster <- 2
  
  ppcguk <- rbind(ppcguk_1,ppcguk_2)
  
  ppcguk <- ppcguk[order(ppcguk$Sample_ID),]
  
  write.table(ppcguk, file = "CA_tm_ppcguk.txt", sep="\t",quote=F, row.names = FALSE)
}

format_arbs <- function() {
  arbs <- read.csv("PPCG_Per_group_20kb_brp_prop_all_chr.csv")
  
  arbs[arbs$Color == "blue",c("Color")] <- 1
  arbs[arbs$Color == "red",c("Color")] <- 2 
  arbs[arbs$Color == "grey",c("Color")] <- 3
  colnames(arbs) <- c("Sample_ID","Color")
  arbs <- arbs[,1:2]
  
  write.table(arbs, file = "CA_arbs.txt", sep="\t",quote=F, row.names = FALSE)
}

arbs <- read.csv("CA_arbs.txt", sep = "\t")

ppcg <- read.csv("CA_tm_ppcg.txt", sep = "\t")
ppcguk <- read.csv("CA_tm_ppcguk.txt", sep = "\t")

evo <- read.csv("evo_results.txt", sep = "\t")

pca <- read.csv("CA_pca.txt", sep = "\t")
tsne <- read.csv("CA_tsne.txt", sep = "\t")
som <- read.csv("CA_som.txt", sep = "\t")
ae <- read.csv("CA_ae.txt", sep = "\t")
umap <- read.csv("CA_umap.txt", sep = "\t")


som[som$Cluster == "A",2] <- 1
som[som$Cluster == "B",2] <- 2

#Will probably need to flip the cluster values.

type_comp <- merge(pca, tsne, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group")
type_comp <- merge(type_comp, som, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group","SOM_Group")
type_comp <- merge(type_comp, ae, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group","SOM_Group","AE_Group")
type_comp <- merge(type_comp, umap, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group","SOM_Group","AE_Group","UMAP_Group")

type_comp$Type_Group <- NA

#Set Clustering Type
for (i in 1:nrow(type_comp)) {
  pca_val <- type_comp[i,c("PCA_Group")]
  tsne_val <- type_comp[i,c("TSNE_Group")]
  som_val <- type_comp[i,c("SOM_Group")]
  ae_val <- type_comp[i,c("AE_Group")]
  umap_val <- type_comp[i,c("UMAP_Group")]
  
  # Three methods
  #If any two agree
  if ((pca_val == 1 & som_val == 2) | (ae_val == 1 & som_val == 2) | (pca_val == 1 & ae_val == 1) ) {
    type_comp[i,c("Type_Group")] <- 1
  } else {
    type_comp[i,c("Type_Group")] <- 2
  }

  #pca som ae#ae som umap#pca ae umap#pca som umap
  #Four methods
  #if ((pca_val == 1 & som_val == 2 & ae_val == 1) | (ae_val == 1 & som_val == 2 & umap_val == 2) | (pca_val == 1 & ae_val == 1 & umap_val == 2) | (pca_val == 1 & som_val == 2 & umap_val == 2)) {
  #  type_comp[i,c("Type_Group")] <- 1
  # } else if (pca_val == 2 & som_val == 1 & ae_val == 2 & umap_val == 1) {
  #   type_comp[i,c("Type_Group")] <- 3
  #} else {
  #  type_comp[i,c("Type_Group")] <- 2
  #}
  
  #pca tsne som#pca tsne ae#pca tsne umap#pca som ae#pca som umap#pca ae umap#tsne som ae#tsne som umap#tsne ae umap#som ae umap
  #Five methods
  # if ((pca_val == 1 & tsne_val == 1 & som_val == 2) | (pca_val == 1 & tsne_val == 1 & ae_val == 1) | (pca_val == 1 & tsne_val == 1 & umap_val == 2) | (pca_val == 1 & som_val == 2 & ae_val == 1) | (pca_val == 1 & som_val == 2 & umap_val == 2) | (pca_val == 1 & ae_val == 1 & umap_val == 2) | (tsne_val == 1 & som_val == 2 & ae_val == 1) | (tsne_val == 1 & som_val == 2 & umap_val == 2) | (tsne_val == 1 & ae_val == 1 & umap_val == 2) | (som_val == 2 & ae_val == 1 & umap_val == 2)) {
  #   type_comp[i,c("Type_Group")] <- 1
  # } else {
  #   type_comp[i,c("Type_Group")] <- 2
  # }
}

type_comp <- merge(type_comp, ppcg, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group","SOM_Group","AE_Group", "UMAP_Group", "Type_Group","PPCG_Group")
type_comp <- merge(type_comp, arbs, by="Sample_ID")
colnames(type_comp) <- c("Sample_ID", "PCA_Group","TSNE_Group","SOM_Group","AE_Group", "UMAP_Group", "Type_Group", "PPCG_Group", "ARBS_Group")

type_comp$FinType_Group <- NA

#Set PPCG Type
for (i in 1:nrow(type_comp)) {
  type_val <- type_comp[i,c("Type_Group")]
  ppcg_val <- type_comp[i,c("PPCG_Group")]
  arbs_val <- type_comp[i,c("ARBS_Group")]

  if ((type_val == 2 & ppcg_val == 2) | (arbs_val == 2 & type_val == 2) | (ppcg_val == 2 & arbs_val == 2)) {
       type_comp[i,c("FinType_Group")] <- 1
  } else {
     type_comp[i,c("FinType_Group")] <- 2
  }
}

evo <- merge(evo, ppcguk, by="Sample_ID")
colnames(evo) <- c("Sample_ID", "Metaclusters","AR_group", "Ordering","Evotype","TM_UK")
evo <- merge(evo, type_comp, by="Sample_ID")
colnames(evo) <- c("Sample_ID", "Metaclusters","AR_group", "Ordering","Evotype","TM_UK","ca_pca","ca_tsne","ca_som","ca_ae", "ca_umap","type_com","TM_PPCG","ca_arbs","fintype_com")

#Clusterings

print("PCA")
print(paste("1:",nrow(type_comp[type_comp$PCA_Group == 1,])))
print(paste("2:",nrow(type_comp[type_comp$PCA_Group == 2,])))
print("TSNE_Group")
print(paste("1:",nrow(type_comp[type_comp$TSNE_Group == 1,])))
print(paste("2:",nrow(type_comp[type_comp$TSNE_Group == 2,])))
print("SOM_Group")
print(paste("1:",nrow(type_comp[type_comp$SOM_Group == 1,])))
print(paste("2:",nrow(type_comp[type_comp$SOM_Group == 2,])))
print("AE_Group")
print(paste("1:",nrow(type_comp[type_comp$AE_Group == 1,])))
print(paste("2:",nrow(type_comp[type_comp$AE_Group == 2,])))
print("UMAP_Group")
print(paste("1:",nrow(type_comp[type_comp$UMAP_Group == 1,])))
print(paste("2:",nrow(type_comp[type_comp$UMAP_Group == 2,])))
print(paste("3:",nrow(type_comp[type_comp$UMAP_Group == 3,])))
print(paste("4:",nrow(type_comp[type_comp$UMAP_Group == 4,])))
print(paste("5:",nrow(type_comp[type_comp$UMAP_Group == 5,])))
print(paste("6:",nrow(type_comp[type_comp$UMAP_Group == 6,])))
print(paste("7:",nrow(type_comp[type_comp$UMAP_Group == 7,])))
print(paste("8:",nrow(type_comp[type_comp$UMAP_Group == 8,])))
print(paste("9:",nrow(type_comp[type_comp$UMAP_Group == 9,])))
print("Type_Group")
print(paste("C1:",nrow(type_comp[type_comp$Type_Group == 1,])))
print(paste("C2:",nrow(type_comp[type_comp$Type_Group == 2,])))
print(paste("U:",nrow(type_comp[type_comp$Type_Group == 3,])))
print("PPCG_Group")
print(paste("PPCGA:",nrow(type_comp[type_comp$PPCG_Group == 1,])))
print(paste("PPCGB:",nrow(type_comp[type_comp$PPCG_Group == 2,])))
print(paste("PPCGC:",nrow(type_comp[type_comp$PPCG_Group == 3,])))
print("ARBS_Group")
print(paste("Enriched:",nrow(type_comp[type_comp$ARBS_Group == 1,])))
print(paste("Depleted:",nrow(type_comp[type_comp$ARBS_Group == 2,])))
print(paste("Intermediate:",nrow(type_comp[type_comp$ARBS_Group == 3,])))
print("FinType_Group")
print(paste("A:",nrow(type_comp[type_comp$FinType_Group == 1,])))
print(paste("B:",nrow(type_comp[type_comp$FinType_Group == 2,])))

print("Metaclusters")
print(paste("MC-B2:",nrow(evo[evo$Metaclusters == 1,])))
print(paste("MC-B1:",nrow(evo[evo$Metaclusters == 2,])))
print(paste("MC-A:",nrow(evo[evo$Metaclusters == 3,])))
print("AR_group")
print(paste("Enriched:",nrow(evo[evo$AR_group == 1,])))
print(paste("Intermediate:",nrow(evo[evo$AR_group == 2,])))
print(paste("Depleted:",nrow(evo[evo$AR_group == 3,])))
print("Ordering")
print(paste("Ordering-I:",nrow(evo[evo$Ordering == 1,])))
print(paste("Ordering-II:",nrow(evo[evo$Ordering == 2,])))
print("Evotype")
print(paste("Alternative:",nrow(evo[evo$Evotype == 1,])))
print(paste("Canonical:",nrow(evo[evo$Evotype == 2,])))
print("TM_UK")
print(paste("PPCGUKA:",nrow(evo[evo$TM_UK == 1,])))
print(paste("PPCGUKB:",nrow(evo[evo$TM_UK == 2,])))
print("ca_pca")
print(paste("1:",nrow(evo[evo$ca_pca == 1,])))
print(paste("2:",nrow(evo[evo$ca_pca == 2,])))
print("ca_tsne")
print(paste("1:",nrow(evo[evo$ca_tsne == 1,])))
print(paste("2:",nrow(evo[evo$ca_tsne == 2,])))
print("ca_som")
print(paste("1:",nrow(evo[evo$ca_som == 1,])))
print(paste("2:",nrow(evo[evo$ca_som == 2,])))
print("ca_ae")
print(paste("1:",nrow(evo[evo$ca_ae == 1,])))
print(paste("2:",nrow(evo[evo$ca_ae == 2,])))
print("ca_umap")
print(paste("1:",nrow(evo[evo$ca_umap == 1,])))
print(paste("2:",nrow(evo[evo$ca_umap == 2,])))
print(paste("3:",nrow(evo[evo$ca_umap == 3,])))
print(paste("4:",nrow(evo[evo$ca_umap == 4,])))
print(paste("5:",nrow(evo[evo$ca_umap == 5,])))
print(paste("6:",nrow(evo[evo$ca_umap == 6,])))
print(paste("7:",nrow(evo[evo$ca_umap == 7,])))
print(paste("8:",nrow(evo[evo$ca_umap == 8,])))
print(paste("9:",nrow(evo[evo$ca_umap == 9,])))
print("type_com")
print(paste("C1:",nrow(evo[evo$type_com == 1,])))
print(paste("C2:",nrow(evo[evo$type_com == 2,])))
print("TM_PPCG")
print(paste("PPCGA:",nrow(evo[evo$TM_PPCG == 1,])))
print(paste("PPCGB:",nrow(evo[evo$TM_PPCG == 2,])))
print(paste("PPCGC:",nrow(evo[evo$TM_PPCG == 3,])))
print("ca_arbs")
print(paste("Enriched:",nrow(evo[evo$ca_arbs == 1,])))
print(paste("Depleted:",nrow(evo[evo$ca_arbs == 2,])))
print(paste("Intermediate:",nrow(evo[evo$ca_arbs == 3,])))
print("fintype_com")
print(paste("A:",nrow(evo[evo$fintype_com == 1,])))
print(paste("B:",nrow(evo[evo$fintype_com == 2,])))


#Adjusted Rand Index (ARI)

#Comparison of Wk methods
print(paste("eAR/eO",adjustedRandIndex(evo$AR_group,evo$Ordering)))
print(paste("eO/eM",adjustedRandIndex(evo$Ordering,evo$Metaclusters)))
print(paste("eAR/eM",adjustedRandIndex(evo$AR_group,evo$Metaclusters)))

#Comparison of Wk methods with Evotype
print(paste("EVO/eAR",adjustedRandIndex(evo$Evotype,evo$AR_group)))
print(paste("EVO/eO",adjustedRandIndex(evo$Evotype,evo$Ordering)))
print(paste("EVO/eM",adjustedRandIndex(evo$Evotype,evo$Metaclusters)))

#Comparing our orderings with Wk orderings
print(paste("PPCG/UK",adjustedRandIndex(evo$TM_PPCG,evo$TM_UK)))
print(paste("eO/UK",adjustedRandIndex(evo$Ordering,evo$TM_UK)))
print(paste("eO/PPCG",adjustedRandIndex(evo$Ordering,evo$TM_PPCG)))

#Comparing PPCG orderings with evotypes
print(paste("EVO/UK",adjustedRandIndex(evo$Evotype,evo$TM_UK)))
print(paste("EVO/PPCG",adjustedRandIndex(evo$Evotype,evo$TM_PPCG)))

#Comparing our methods with Wk metaclustering
print(paste("eM/pca",adjustedRandIndex(evo$Metaclusters,evo$ca_pca)))
print(paste("eM/tsne",adjustedRandIndex(evo$Metaclusters,evo$ca_tsne)))
print(paste("eM/som",adjustedRandIndex(evo$Metaclusters,evo$ca_som)))
print(paste("eM/ae",adjustedRandIndex(evo$Metaclusters,evo$ca_ae)))

#Comparing our methods with Wk evotypes
print(paste("EVO/pca",adjustedRandIndex(evo$Evotype,evo$ca_pca)))
print(paste("EVO/tsne",adjustedRandIndex(evo$Evotype,evo$ca_tsne)))
print(paste("EVO/som",adjustedRandIndex(evo$Evotype,evo$ca_som)))
print(paste("EVO/ae",adjustedRandIndex(evo$Evotype,evo$ca_ae)))

#Comparing PPCG arbs with Wk ARBS
print(paste("eAR/arbs",adjustedRandIndex(evo$AR_group,evo$ca_arbs)))

#Comparing PPCG arbs with Wk evotypes
print(paste("EVO/arbs",adjustedRandIndex(evo$Evotype,evo$ca_arbs)))

#Comparing our composite cluster with Wk metaclustering
print(paste("eM/type",adjustedRandIndex(evo$Metaclusters,evo$type_com)))

#Comparing our composite cluster with Wk evotype
print(paste("EVO/type",adjustedRandIndex(evo$Evotype,evo$type_com)))

#Comparison of all our methods
print(paste("pca/tsne:",adjustedRandIndex(type_comp$PCA_Group,type_comp$TSNE_Group)))
print(paste("pca/som:",adjustedRandIndex(type_comp$PCA_Group,type_comp$SOM_Group)))
print(paste("pca/ae:",adjustedRandIndex(type_comp$PCA_Group,type_comp$AE_Group)))
print(paste("tsne/som:",adjustedRandIndex(type_comp$TSNE_Group,type_comp$SOM_Group)))
print(paste("tsne/ae:",adjustedRandIndex(type_comp$TSNE_Group,type_comp$AE_Group)))
print(paste("som/ae:",adjustedRandIndex(type_comp$SOM_Group,type_comp$AE_Group)))
print(paste("umap/pca:",adjustedRandIndex(type_comp$UMAP_Group,type_comp$PCA_Group)))
print(paste("umap/tsne:",adjustedRandIndex(type_comp$UMAP_Group,type_comp$TSNE_Group)))
print(paste("umap/som:",adjustedRandIndex(type_comp$UMAP_Group,type_comp$SOM_Group)))
print(paste("umap/ae:",adjustedRandIndex(type_comp$UMAP_Group,type_comp$AE_Group)))

#Comparison of Clustering, PPCG Ordering and PPCG ARBS
print(paste("clust/ordering:",adjustedRandIndex(type_comp$Type_Group,type_comp$PPCG_Group)))
print(paste("ordering/arbs:",adjustedRandIndex(type_comp$PPCG_Group,type_comp$ARBS_Group)))
print(paste("clust/arbs:",adjustedRandIndex(type_comp$Type_Group,type_comp$ARBS_Group)))
            
#Comparison            
print(paste("EVO/PPCGTYPE:",adjustedRandIndex(evo$Evotype,evo$fintype_com)))

print(paste("arbs/umap:",adjustedRandIndex(type_comp$ARBS_Group,type_comp$UMAP_Group)))
print(paste("arbs/pca:",adjustedRandIndex(type_comp$ARBS_Group,type_comp$PCA_Group)))
print(paste("arbs/tsne:",adjustedRandIndex(type_comp$ARBS_Group,type_comp$TSNE_Group)))
print(paste("arbs/som:",adjustedRandIndex(type_comp$ARBS_Group,type_comp$SOM_Group)))
print(paste("arbs/ae:",adjustedRandIndex(type_comp$ARBS_Group,type_comp$AE_Group)))





#Output PPCG Type Summary
ppcg_type <- type_comp[,c("Sample_ID","FinType_Group")]

colnames(ppcg_type) <- c("Sample_ID","Cluster")
write.table(ppcg_type, file = "Clust_FinType.txt", sep="\t",quote=F, row.names = FALSE)

#Reordering labels ahead of confusion
type_comp[type_comp$PCA_Group == 1, c("PCA_Group")] <- "A"
type_comp[type_comp$PCA_Group == 2, c("PCA_Group")] <- "B"

type_comp[type_comp$SOM_Group == 1, c("SOM_Group")] <- "A"
type_comp[type_comp$SOM_Group == 2, c("SOM_Group")] <- "B"

type_comp[type_comp$AE_Group == 1, c("AE_Group")] <- "A"
type_comp[type_comp$AE_Group == 2, c("AE_Group")] <- "B"

type_comp[type_comp$UMAP_Group == 1, c("UMAP_Group")] <- "A"
type_comp[type_comp$UMAP_Group == 2, c("UMAP_Group")] <- "B"

type_comp[type_comp$PCA_Group == "A", c("PCA_Group")] <- 1
type_comp[type_comp$PCA_Group == "B", c("PCA_Group")] <- 2

type_comp[type_comp$SOM_Group == "A", c("SOM_Group")] <- 2
type_comp[type_comp$SOM_Group == "B", c("SOM_Group")] <- 1

type_comp[type_comp$AE_Group == "A", c("AE_Group")] <- 1
type_comp[type_comp$AE_Group == "B", c("AE_Group")] <- 2

type_comp[type_comp$UMAP_Group == "A", c("UMAP_Group")] <- 2
type_comp[type_comp$UMAP_Group == "B", c("UMAP_Group")] <- 1


# confused_matrix("eAR/eO",evo$AR_group,evo$Ordering,F)
# confused_matrix("eO/eM",evo$Ordering,evo$Metaclusters,F)
# confused_matrix("eAR/eM",evo$AR_group,evo$Metaclusters,F)
# 
# #MC-B2 is 1, MC-B1 is 2, MC-A is 3
# 
# confused_matrix("pca/som",type_comp$PCA_Group,type_comp$SOM_Group,T)
# confused_matrix("pca/ae",type_comp$PCA_Group,type_comp$AE_Group,T)
# confused_matrix("som/ae",type_comp$SOM_Group,type_comp$AE_Group,T)
# 
# 
# confused_matrix("clust/ordering",type_comp$Type_Group,type_comp$PPCG_Group,F)
# confused_matrix("ordering/arbs",type_comp$PPCG_Group,type_comp$ARBS_Group,F)
# confused_matrix("clust/arbs",type_comp$Type_Group,type_comp$ARBS_Group,F)

# confused_matrix("eM/type",evo$Metaclusters,evo$type_com,T)
# 
confused_matrix("EVO/PPCGTYPE",evo$Evotype,evo$fintype_com,T)
# 
# confused_matrix("eAR/arbs",evo$AR_group,evo$ca_arbs,T)



# confused_matrix("pca/tsne:",type_comp$PCA_Group,type_comp$TSNE_Group,F)
# confused_matrix("pca/som:",type_comp$PCA_Group,type_comp$SOM_Group,F)
# confused_matrix("pca/ae:",type_comp$PCA_Group,type_comp$AE_Group,F)
# confused_matrix("tsne/som:",type_comp$TSNE_Group,type_comp$SOM_Group,F)
# confused_matrix("tsne/ae:",type_comp$TSNE_Group,type_comp$AE_Group,F)
# confused_matrix("som/ae:",type_comp$SOM_Group,type_comp$AE_Group,F)
# confused_matrix("umap/pca:",type_comp$UMAP_Group,type_comp$PCA_Group,F)
# confused_matrix("umap/tsne:",type_comp$UMAP_Group,type_comp$TSNE_Group,F)
# confused_matrix("umap/som:",type_comp$UMAP_Group,type_comp$SOM_Group,F)
# confused_matrix("umap/ae:",type_comp$UMAP_Group,type_comp$AE_Group,F)



