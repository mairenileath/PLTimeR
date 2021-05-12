library(mclust)

print_cluster_counts <- function(data, title) {
  c1 <- nrow(data[data$Cluster == "1",])
  c2 <- nrow(data[data$Cluster == "2",])
  print(paste(title, "C1", c1))
  print(paste(title, "C2", c2))
}

convert_cluster_names <- function(data,flip) {
  if (flip) { 
    data[data$Cluster == 1,2] <- "B"
    data[data$Cluster == 2,2] <- "A" 
  } else {
    data[data$Cluster == 1,2] <- "A"
    data[data$Cluster == 2,2] <- "B" 
  }
  return(data)
}

ca_umap_1 <- read.csv("CA_umap1.txt", sep = "\t")
ca_umap_2 <- read.csv("CA_umap2.txt", sep = "\t")
ca_umap_3 <- read.csv("CA_umap3.txt", sep = "\t")
ca_umap_4 <- read.csv("CA_umap4.txt", sep = "\t")
ca_umap_5 <- read.csv("CA_umap5.txt", sep = "\t")
ca_umap_6 <- read.csv("CA_umap6.txt", sep = "\t")
ca_umap_7 <- read.csv("CA_umap7.txt", sep = "\t")
ca_umap_8 <- read.csv("CA_umap8.txt", sep = "\t")
ca_umap_9 <- read.csv("CA_umap9.txt", sep = "\t")
ca_umap_10 <- read.csv("CA_umap10.txt", sep = "\t")
ca_umap_11 <- read.csv("CA_umap11.txt", sep = "\t")
ca_umap_12 <- read.csv("CA_umap12.txt", sep = "\t")
ca_umap_13 <- read.csv("CA_umap13.txt", sep = "\t")
ca_umap_14 <- read.csv("CA_umap14.txt", sep = "\t")
ca_umap_15 <- read.csv("CA_umap15.txt", sep = "\t")
ca_umap_16 <- read.csv("CA_umap16.txt", sep = "\t")
ca_umap_17 <- read.csv("CA_umap17.txt", sep = "\t")
ca_umap_18 <- read.csv("CA_umap18.txt", sep = "\t")
ca_umap_19 <- read.csv("CA_umap19.txt", sep = "\t")
ca_umap_20 <- read.csv("CA_umap20.txt", sep = "\t")
ca_umap_21 <- read.csv("CA_umap21.txt", sep = "\t")
ca_umap_22 <- read.csv("CA_umap22.txt", sep = "\t")
ca_umap_23 <- read.csv("CA_umap23.txt", sep = "\t")
ca_umap_24 <- read.csv("CA_umap24.txt", sep = "\t")
ca_umap_25 <- read.csv("CA_umap25.txt", sep = "\t")
ca_umap_26 <- read.csv("CA_umap26.txt", sep = "\t")
ca_umap_27 <- read.csv("CA_umap27.txt", sep = "\t")
ca_umap_28 <- read.csv("CA_umap28.txt", sep = "\t")
ca_umap_29 <- read.csv("CA_umap29.txt", sep = "\t")
ca_umap_30 <- read.csv("CA_umap30.txt", sep = "\t")
ca_umap_31 <- read.csv("CA_umap31.txt", sep = "\t")
ca_umap_32 <- read.csv("CA_umap32.txt", sep = "\t")
ca_umap_33 <- read.csv("CA_umap33.txt", sep = "\t")
ca_umap_34 <- read.csv("CA_umap34.txt", sep = "\t")
ca_umap_35 <- read.csv("CA_umap35.txt", sep = "\t")
ca_umap_36 <- read.csv("CA_umap36.txt", sep = "\t")
ca_umap_37 <- read.csv("CA_umap37.txt", sep = "\t")
ca_umap_38 <- read.csv("CA_umap38.txt", sep = "\t")
ca_umap_39 <- read.csv("CA_umap39.txt", sep = "\t")
ca_umap_40 <- read.csv("CA_umap40.txt", sep = "\t")
ca_umap_41 <- read.csv("CA_umap41.txt", sep = "\t")
ca_umap_42 <- read.csv("CA_umap42.txt", sep = "\t")
ca_umap_43 <- read.csv("CA_umap43.txt", sep = "\t")
ca_umap_44 <- read.csv("CA_umap44.txt", sep = "\t")
ca_umap_45 <- read.csv("CA_umap45.txt", sep = "\t")
ca_umap_46 <- read.csv("CA_umap46.txt", sep = "\t")
ca_umap_47 <- read.csv("CA_umap47.txt", sep = "\t")
ca_umap_48 <- read.csv("CA_umap48.txt", sep = "\t")
ca_umap_49 <- read.csv("CA_umap49.txt", sep = "\t")
ca_umap_50 <- read.csv("CA_umap50.txt", sep = "\t")
ca_umap_51 <- read.csv("CA_umap51.txt", sep = "\t")
ca_umap_52 <- read.csv("CA_umap52.txt", sep = "\t")
ca_umap_53 <- read.csv("CA_umap53.txt", sep = "\t")
ca_umap_54 <- read.csv("CA_umap54.txt", sep = "\t")
ca_umap_55 <- read.csv("CA_umap55.txt", sep = "\t")
ca_umap_56 <- read.csv("CA_umap56.txt", sep = "\t")
ca_umap_57 <- read.csv("CA_umap57.txt", sep = "\t")
ca_umap_58 <- read.csv("CA_umap58.txt", sep = "\t")
ca_umap_59 <- read.csv("CA_umap59.txt", sep = "\t")
ca_umap_60 <- read.csv("CA_umap60.txt", sep = "\t")
ca_umap_61 <- read.csv("CA_umap61.txt", sep = "\t")
ca_umap_62 <- read.csv("CA_umap62.txt", sep = "\t")
ca_umap_63 <- read.csv("CA_umap63.txt", sep = "\t")
ca_umap_64 <- read.csv("CA_umap64.txt", sep = "\t")
ca_umap_65 <- read.csv("CA_umap65.txt", sep = "\t")
ca_umap_66 <- read.csv("CA_umap66.txt", sep = "\t")
ca_umap_67 <- read.csv("CA_umap67.txt", sep = "\t")
ca_umap_68 <- read.csv("CA_umap68.txt", sep = "\t")
ca_umap_69 <- read.csv("CA_umap69.txt", sep = "\t")
ca_umap_70 <- read.csv("CA_umap70.txt", sep = "\t")
ca_umap_71 <- read.csv("CA_umap71.txt", sep = "\t")
ca_umap_72 <- read.csv("CA_umap72.txt", sep = "\t")
ca_umap_73 <- read.csv("CA_umap73.txt", sep = "\t")
ca_umap_74 <- read.csv("CA_umap74.txt", sep = "\t")
ca_umap_75 <- read.csv("CA_umap75.txt", sep = "\t")
ca_umap_76 <- read.csv("CA_umap76.txt", sep = "\t")
ca_umap_77 <- read.csv("CA_umap77.txt", sep = "\t")
ca_umap_78 <- read.csv("CA_umap78.txt", sep = "\t")
ca_umap_79 <- read.csv("CA_umap79.txt", sep = "\t")
ca_umap_80 <- read.csv("CA_umap80.txt", sep = "\t")
ca_umap_81 <- read.csv("CA_umap81.txt", sep = "\t")
ca_umap_82 <- read.csv("CA_umap82.txt", sep = "\t")
ca_umap_83 <- read.csv("CA_umap83.txt", sep = "\t")
ca_umap_84 <- read.csv("CA_umap84.txt", sep = "\t")
ca_umap_85 <- read.csv("CA_umap85.txt", sep = "\t")
ca_umap_86 <- read.csv("CA_umap86.txt", sep = "\t")
ca_umap_87 <- read.csv("CA_umap87.txt", sep = "\t")
ca_umap_88 <- read.csv("CA_umap88.txt", sep = "\t")
ca_umap_89 <- read.csv("CA_umap89.txt", sep = "\t")
ca_umap_90 <- read.csv("CA_umap90.txt", sep = "\t")
ca_umap_91 <- read.csv("CA_umap91.txt", sep = "\t")
ca_umap_92 <- read.csv("CA_umap92.txt", sep = "\t")
ca_umap_93 <- read.csv("CA_umap93.txt", sep = "\t")
ca_umap_94 <- read.csv("CA_umap94.txt", sep = "\t")
ca_umap_95 <- read.csv("CA_umap95.txt", sep = "\t")
ca_umap_96 <- read.csv("CA_umap96.txt", sep = "\t")
ca_umap_97 <- read.csv("CA_umap97.txt", sep = "\t")
ca_umap_98 <- read.csv("CA_umap98.txt", sep = "\t")
ca_umap_99 <- read.csv("CA_umap99.txt", sep = "\t")
ca_umap_100 <- read.csv("CA_umap100.txt", sep = "\t")


# ca_umap_1 <- run_umap()
# ca_umap_2 <- run_umap()
# ca_umap_3 <- run_umap()
# ca_umap_4 <- run_umap()
# ca_umap_5 <- run_umap()
# ca_umap_6 <- run_umap()
# ca_umap_7 <- run_umap()
# ca_umap_8 <- run_umap()
# ca_umap_9 <- run_umap()
# ca_umap_10 <- run_umap()

# ca_umap_1 <- convert_cluster_names(ca_umap_1,FALSE)
# ca_umap_2 <- convert_cluster_names(ca_umap_2,FALSE)
# ca_umap_3 <- convert_cluster_names(ca_umap_3,FALSE)
# ca_umap_4 <- convert_cluster_names(ca_umap_4,TRUE)
# ca_umap_5 <- convert_cluster_names(ca_umap_5,FALSE)
# ca_umap_6 <- convert_cluster_names(ca_umap_6,TRUE)
# ca_umap_7 <- convert_cluster_names(ca_umap_7,FALSE)
# ca_umap_8 <- convert_cluster_names(ca_umap_8,FALSE)
# ca_umap_9 <- convert_cluster_names(ca_umap_9,FALSE)
# ca_umap_10 <- convert_cluster_names(ca_umap_10,FALSE)

print_cluster_counts(ca_umap_1, "1")
print_cluster_counts(ca_umap_2, "2")
print_cluster_counts(ca_umap_3, "3")
print_cluster_counts(ca_umap_4, "4")
print_cluster_counts(ca_umap_5, "5")
print_cluster_counts(ca_umap_6, "6")
print_cluster_counts(ca_umap_7, "7")
print_cluster_counts(ca_umap_8, "8")
print_cluster_counts(ca_umap_9, "9")
print_cluster_counts(ca_umap_10, "10")
print_cluster_counts(ca_umap_11, "11")
print_cluster_counts(ca_umap_12, "12")
print_cluster_counts(ca_umap_13, "13")
print_cluster_counts(ca_umap_14, "14")
print_cluster_counts(ca_umap_15, "15")
print_cluster_counts(ca_umap_16, "16")
print_cluster_counts(ca_umap_17, "17")
print_cluster_counts(ca_umap_18, "18")
print_cluster_counts(ca_umap_19, "19")
print_cluster_counts(ca_umap_20, "20")
print_cluster_counts(ca_umap_21, "21")
print_cluster_counts(ca_umap_22, "22")
print_cluster_counts(ca_umap_23, "23")
print_cluster_counts(ca_umap_24, "24")
print_cluster_counts(ca_umap_25, "25")
print_cluster_counts(ca_umap_26, "26")
print_cluster_counts(ca_umap_27, "27")
print_cluster_counts(ca_umap_28, "28")
print_cluster_counts(ca_umap_29, "29")
print_cluster_counts(ca_umap_30, "30")
print_cluster_counts(ca_umap_31, "31")
print_cluster_counts(ca_umap_32, "32")
print_cluster_counts(ca_umap_33, "33")
print_cluster_counts(ca_umap_34, "34")
print_cluster_counts(ca_umap_35, "35")
print_cluster_counts(ca_umap_36, "36")
print_cluster_counts(ca_umap_37, "37")
print_cluster_counts(ca_umap_38, "38")
print_cluster_counts(ca_umap_39, "39")
print_cluster_counts(ca_umap_40, "40")
print_cluster_counts(ca_umap_41, "41")
print_cluster_counts(ca_umap_42, "42")
print_cluster_counts(ca_umap_43, "43")
print_cluster_counts(ca_umap_44, "44")
print_cluster_counts(ca_umap_45, "45")
print_cluster_counts(ca_umap_46, "46")
print_cluster_counts(ca_umap_47, "47")
print_cluster_counts(ca_umap_48, "48")
print_cluster_counts(ca_umap_49, "49")
print_cluster_counts(ca_umap_50, "50")
print_cluster_counts(ca_umap_51, "51")
print_cluster_counts(ca_umap_52, "52")
print_cluster_counts(ca_umap_53, "53")
print_cluster_counts(ca_umap_54, "54")
print_cluster_counts(ca_umap_55, "55")
print_cluster_counts(ca_umap_56, "56")
print_cluster_counts(ca_umap_57, "57")
print_cluster_counts(ca_umap_58, "58")
print_cluster_counts(ca_umap_59, "59")
print_cluster_counts(ca_umap_60, "60")
print_cluster_counts(ca_umap_61, "61")
print_cluster_counts(ca_umap_62, "62")
print_cluster_counts(ca_umap_63, "63")
print_cluster_counts(ca_umap_64, "64")
print_cluster_counts(ca_umap_65, "65")
print_cluster_counts(ca_umap_66, "66")
print_cluster_counts(ca_umap_67, "67")
print_cluster_counts(ca_umap_68, "68")
print_cluster_counts(ca_umap_69, "69")
print_cluster_counts(ca_umap_70, "70")
print_cluster_counts(ca_umap_71, "71")
print_cluster_counts(ca_umap_72, "72")
print_cluster_counts(ca_umap_73, "73")
print_cluster_counts(ca_umap_74, "74")
print_cluster_counts(ca_umap_75, "75")
print_cluster_counts(ca_umap_76, "76")
print_cluster_counts(ca_umap_77, "77")
print_cluster_counts(ca_umap_78, "78")
print_cluster_counts(ca_umap_79, "79")
print_cluster_counts(ca_umap_80, "80")
print_cluster_counts(ca_umap_81, "81")
print_cluster_counts(ca_umap_82, "82")
print_cluster_counts(ca_umap_83, "83")
print_cluster_counts(ca_umap_84, "84")
print_cluster_counts(ca_umap_85, "85")
print_cluster_counts(ca_umap_86, "86")
print_cluster_counts(ca_umap_87, "87")
print_cluster_counts(ca_umap_88, "88")
print_cluster_counts(ca_umap_89, "89")
print_cluster_counts(ca_umap_90, "90")
print_cluster_counts(ca_umap_91, "91")
print_cluster_counts(ca_umap_92, "92")
print_cluster_counts(ca_umap_93, "93")
print_cluster_counts(ca_umap_94, "94")
print_cluster_counts(ca_umap_95, "95")
print_cluster_counts(ca_umap_96, "96")
print_cluster_counts(ca_umap_97, "97")
print_cluster_counts(ca_umap_98, "98")
print_cluster_counts(ca_umap_99, "99")
print_cluster_counts(ca_umap_100, "100")

c_1 <- ca_umap_1$Cluster
c_2 <- ca_umap_2$Cluster
c_3 <- ca_umap_3$Cluster
c_4 <- ca_umap_4$Cluster
c_5 <- ca_umap_5$Cluster
c_6 <- ca_umap_6$Cluster
c_7 <- ca_umap_7$Cluster
c_8 <- ca_umap_8$Cluster
c_9 <- ca_umap_9$Cluster
c_10 <- ca_umap_10$Cluster
c_11 <- ca_umap_11$Cluster
c_12 <- ca_umap_12$Cluster
c_13 <- ca_umap_13$Cluster
c_14 <- ca_umap_14$Cluster
c_15 <- ca_umap_15$Cluster
c_16 <- ca_umap_16$Cluster
c_17 <- ca_umap_17$Cluster
c_18 <- ca_umap_18$Cluster
c_19 <- ca_umap_19$Cluster
c_20 <- ca_umap_20$Cluster
c_21 <- ca_umap_21$Cluster
c_22 <- ca_umap_22$Cluster
c_23 <- ca_umap_23$Cluster
c_24 <- ca_umap_24$Cluster
c_25 <- ca_umap_25$Cluster
c_26 <- ca_umap_26$Cluster
c_27 <- ca_umap_27$Cluster
c_28 <- ca_umap_28$Cluster
c_29 <- ca_umap_29$Cluster
c_30 <- ca_umap_30$Cluster
c_31 <- ca_umap_31$Cluster
c_32 <- ca_umap_32$Cluster
c_33 <- ca_umap_33$Cluster
c_34 <- ca_umap_34$Cluster
c_35 <- ca_umap_35$Cluster
c_36 <- ca_umap_36$Cluster
c_37 <- ca_umap_37$Cluster
c_38 <- ca_umap_38$Cluster
c_39 <- ca_umap_39$Cluster
c_40 <- ca_umap_40$Cluster
c_41 <- ca_umap_41$Cluster
c_42 <- ca_umap_42$Cluster
c_43 <- ca_umap_43$Cluster
c_44 <- ca_umap_44$Cluster
c_45 <- ca_umap_45$Cluster
c_46 <- ca_umap_46$Cluster
c_47 <- ca_umap_47$Cluster
c_48 <- ca_umap_48$Cluster
c_49 <- ca_umap_49$Cluster
c_50 <- ca_umap_50$Cluster
c_51 <- ca_umap_51$Cluster
c_52 <- ca_umap_52$Cluster
c_53 <- ca_umap_53$Cluster
c_54 <- ca_umap_54$Cluster
c_55 <- ca_umap_55$Cluster
c_56 <- ca_umap_56$Cluster
c_57 <- ca_umap_57$Cluster
c_58 <- ca_umap_58$Cluster
c_59 <- ca_umap_59$Cluster
c_60 <- ca_umap_60$Cluster
c_61 <- ca_umap_61$Cluster
c_62 <- ca_umap_62$Cluster
c_63 <- ca_umap_63$Cluster
c_64 <- ca_umap_64$Cluster
c_65 <- ca_umap_65$Cluster
c_66 <- ca_umap_66$Cluster
c_67 <- ca_umap_67$Cluster
c_68 <- ca_umap_68$Cluster
c_69 <- ca_umap_69$Cluster
c_70 <- ca_umap_70$Cluster
c_71 <- ca_umap_71$Cluster
c_72 <- ca_umap_72$Cluster
c_73 <- ca_umap_73$Cluster
c_74 <- ca_umap_74$Cluster
c_75 <- ca_umap_75$Cluster
c_76 <- ca_umap_76$Cluster
c_77 <- ca_umap_77$Cluster
c_78 <- ca_umap_78$Cluster
c_79 <- ca_umap_79$Cluster
c_80 <- ca_umap_80$Cluster
c_81 <- ca_umap_81$Cluster
c_82 <- ca_umap_82$Cluster
c_83 <- ca_umap_83$Cluster
c_84 <- ca_umap_84$Cluster
c_85 <- ca_umap_85$Cluster
c_86 <- ca_umap_86$Cluster
c_87 <- ca_umap_87$Cluster
c_88 <- ca_umap_88$Cluster
c_89 <- ca_umap_89$Cluster
c_90 <- ca_umap_90$Cluster
c_91 <- ca_umap_91$Cluster
c_92 <- ca_umap_92$Cluster
c_93 <- ca_umap_93$Cluster
c_94 <- ca_umap_94$Cluster
c_95 <- ca_umap_95$Cluster
c_96 <- ca_umap_96$Cluster
c_97 <- ca_umap_97$Cluster
c_98 <- ca_umap_98$Cluster
c_99 <- ca_umap_99$Cluster
c_100 <- ca_umap_100$Cluster

options <- list(c_1,
                c_2,
                c_3,
                c_4,
                c_5,
                c_6,
                c_7,
                c_8,
                c_9,
                c_10,
                c_11,
                c_12,
                c_13,
                c_14,
                c_15,
                c_16,
                c_17,
                c_18,
                c_19,
                c_20,
                c_21,
                c_22,
                c_23,
                c_24,
                c_25,
                c_26,
                c_27,
                c_28,
                c_29,
                c_30,
                c_31,
                c_32,
                c_33,
                c_34,
                c_35,
                c_36,
                c_37,
                c_38,
                c_39,
                c_40,
                c_41,
                c_42,
                c_43,
                c_44,
                c_45,
                c_46,
                c_47,
                c_48,
                c_49,
                c_50,
                c_51,
                c_52,
                c_53,
                c_54,
                c_55,
                c_56,
                c_57,
                c_58,
                c_59,
                c_60,
                c_61,
                c_62,
                c_63,
                c_64,
                c_65,
                c_66,
                c_67,
                c_68,
                c_69,
                c_70,
                c_71,
                c_72,
                c_73,
                c_74,
                c_75,
                c_76,
                c_77,
                c_78,
                c_79,
                c_80,
                c_81,
                c_82,
                c_83,
                c_84,
                c_85,
                c_86,
                c_87,
                c_88,
                c_89,
                c_90,
                c_91,
                c_92,
                c_93,
                c_94,
                c_95,
                c_96,
                c_97,
                c_98,
                c_99,
                c_100)

ar_list <- c()

for (i in 1:length(options)) {
  for (j in 1:length(options)) {
    a <- options[[i]]
    #print(a)
    b <- options[[j]]
    #print(b)
    value <- adjustedRandIndex(a,b)
    ar_list <- append(ar_list,value)
    #print(paste0("C",i," - C",j,": ",value))
  }
}
