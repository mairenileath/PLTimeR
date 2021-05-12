rm(list = ls())

library(mclust)
source("functions_subtyping.R")

print_cluster_counts <- function(data, title) {
  c1 <- nrow(data[data$Cluster == "1",])
  c2 <- nrow(data[data$Cluster == "2",])
  print(paste(title, "C1", c1))
  print(paste(title, "C2", c2))
}

ca_ae_1 <- read.csv("CA_ae1.txt", sep = "\t")
ca_ae_2 <- read.csv("CA_ae2.txt", sep = "\t")
ca_ae_3 <- read.csv("CA_ae3.txt", sep = "\t")
ca_ae_4 <- read.csv("CA_ae4.txt", sep = "\t")
ca_ae_5 <- read.csv("CA_ae5.txt", sep = "\t")
ca_ae_6 <- read.csv("CA_ae6.txt", sep = "\t")
ca_ae_7 <- read.csv("CA_ae7.txt", sep = "\t")
ca_ae_8 <- read.csv("CA_ae8.txt", sep = "\t")
ca_ae_9 <- read.csv("CA_ae9.txt", sep = "\t")
ca_ae_10 <- read.csv("CA_ae10.txt", sep = "\t")
ca_ae_11 <- read.csv("CA_ae11.txt", sep = "\t")
ca_ae_12 <- read.csv("CA_ae12.txt", sep = "\t")
ca_ae_13 <- read.csv("CA_ae13.txt", sep = "\t")
ca_ae_14 <- read.csv("CA_ae14.txt", sep = "\t")
ca_ae_15 <- read.csv("CA_ae15.txt", sep = "\t")
ca_ae_16 <- read.csv("CA_ae16.txt", sep = "\t")
ca_ae_17 <- read.csv("CA_ae17.txt", sep = "\t")
ca_ae_18 <- read.csv("CA_ae18.txt", sep = "\t")
ca_ae_19 <- read.csv("CA_ae19.txt", sep = "\t")
ca_ae_20 <- read.csv("CA_ae20.txt", sep = "\t")
ca_ae_21 <- read.csv("CA_ae21.txt", sep = "\t")
ca_ae_22 <- read.csv("CA_ae22.txt", sep = "\t")
ca_ae_23 <- read.csv("CA_ae23.txt", sep = "\t")
ca_ae_24 <- read.csv("CA_ae24.txt", sep = "\t")
ca_ae_25 <- read.csv("CA_ae25.txt", sep = "\t")
ca_ae_26 <- read.csv("CA_ae26.txt", sep = "\t")
ca_ae_27 <- read.csv("CA_ae27.txt", sep = "\t")
ca_ae_28 <- read.csv("CA_ae28.txt", sep = "\t")
ca_ae_29 <- read.csv("CA_ae29.txt", sep = "\t")
ca_ae_30 <- read.csv("CA_ae30.txt", sep = "\t")
ca_ae_31 <- read.csv("CA_ae31.txt", sep = "\t")
ca_ae_32 <- read.csv("CA_ae32.txt", sep = "\t")
ca_ae_33 <- read.csv("CA_ae33.txt", sep = "\t")
ca_ae_34 <- read.csv("CA_ae34.txt", sep = "\t")
ca_ae_35 <- read.csv("CA_ae35.txt", sep = "\t")
ca_ae_36 <- read.csv("CA_ae36.txt", sep = "\t")
ca_ae_37 <- read.csv("CA_ae37.txt", sep = "\t")
ca_ae_38 <- read.csv("CA_ae38.txt", sep = "\t")
ca_ae_39 <- read.csv("CA_ae39.txt", sep = "\t")
ca_ae_40 <- read.csv("CA_ae40.txt", sep = "\t")
ca_ae_41 <- read.csv("CA_ae41.txt", sep = "\t")
ca_ae_42 <- read.csv("CA_ae42.txt", sep = "\t")
ca_ae_43 <- read.csv("CA_ae43.txt", sep = "\t")
ca_ae_44 <- read.csv("CA_ae44.txt", sep = "\t")
ca_ae_45 <- read.csv("CA_ae45.txt", sep = "\t")
ca_ae_46 <- read.csv("CA_ae46.txt", sep = "\t")
ca_ae_47 <- read.csv("CA_ae47.txt", sep = "\t")
ca_ae_48 <- read.csv("CA_ae48.txt", sep = "\t")
ca_ae_49 <- read.csv("CA_ae49.txt", sep = "\t")
ca_ae_50 <- read.csv("CA_ae50.txt", sep = "\t")
ca_ae_51 <- read.csv("CA_ae51.txt", sep = "\t")
ca_ae_52 <- read.csv("CA_ae52.txt", sep = "\t")
ca_ae_53 <- read.csv("CA_ae53.txt", sep = "\t")
ca_ae_54 <- read.csv("CA_ae54.txt", sep = "\t")
ca_ae_55 <- read.csv("CA_ae55.txt", sep = "\t")
ca_ae_56 <- read.csv("CA_ae56.txt", sep = "\t")
ca_ae_57 <- read.csv("CA_ae57.txt", sep = "\t")
ca_ae_58 <- read.csv("CA_ae58.txt", sep = "\t")
ca_ae_59 <- read.csv("CA_ae59.txt", sep = "\t")
ca_ae_60 <- read.csv("CA_ae60.txt", sep = "\t")
ca_ae_61 <- read.csv("CA_ae61.txt", sep = "\t")
ca_ae_62 <- read.csv("CA_ae62.txt", sep = "\t")
ca_ae_63 <- read.csv("CA_ae63.txt", sep = "\t")
ca_ae_64 <- read.csv("CA_ae64.txt", sep = "\t")
ca_ae_65 <- read.csv("CA_ae65.txt", sep = "\t")
ca_ae_66 <- read.csv("CA_ae66.txt", sep = "\t")
ca_ae_67 <- read.csv("CA_ae67.txt", sep = "\t")
ca_ae_68 <- read.csv("CA_ae68.txt", sep = "\t")
ca_ae_69 <- read.csv("CA_ae69.txt", sep = "\t")
ca_ae_70 <- read.csv("CA_ae70.txt", sep = "\t")
ca_ae_71 <- read.csv("CA_ae71.txt", sep = "\t")
ca_ae_72 <- read.csv("CA_ae72.txt", sep = "\t")
ca_ae_73 <- read.csv("CA_ae73.txt", sep = "\t")
ca_ae_74 <- read.csv("CA_ae74.txt", sep = "\t")
ca_ae_75 <- read.csv("CA_ae75.txt", sep = "\t")
ca_ae_76 <- read.csv("CA_ae76.txt", sep = "\t")
ca_ae_77 <- read.csv("CA_ae77.txt", sep = "\t")
ca_ae_78 <- read.csv("CA_ae78.txt", sep = "\t")
ca_ae_79 <- read.csv("CA_ae79.txt", sep = "\t")
ca_ae_80 <- read.csv("CA_ae80.txt", sep = "\t")
ca_ae_81 <- read.csv("CA_ae81.txt", sep = "\t")
ca_ae_82 <- read.csv("CA_ae82.txt", sep = "\t")
ca_ae_83 <- read.csv("CA_ae83.txt", sep = "\t")
ca_ae_84 <- read.csv("CA_ae84.txt", sep = "\t")
ca_ae_85 <- read.csv("CA_ae85.txt", sep = "\t")
ca_ae_86 <- read.csv("CA_ae86.txt", sep = "\t")
ca_ae_87 <- read.csv("CA_ae87.txt", sep = "\t")
ca_ae_88 <- read.csv("CA_ae88.txt", sep = "\t")
ca_ae_89 <- read.csv("CA_ae89.txt", sep = "\t")
ca_ae_90 <- read.csv("CA_ae90.txt", sep = "\t")
ca_ae_91 <- read.csv("CA_ae91.txt", sep = "\t")
ca_ae_92 <- read.csv("CA_ae92.txt", sep = "\t")
ca_ae_93 <- read.csv("CA_ae93.txt", sep = "\t")
ca_ae_94 <- read.csv("CA_ae94.txt", sep = "\t")
ca_ae_95 <- read.csv("CA_ae95.txt", sep = "\t")
ca_ae_96 <- read.csv("CA_ae96.txt", sep = "\t")
ca_ae_97 <- read.csv("CA_ae97.txt", sep = "\t")
ca_ae_98 <- read.csv("CA_ae98.txt", sep = "\t")
ca_ae_99 <- read.csv("CA_ae99.txt", sep = "\t")
ca_ae_100 <- read.csv("CA_ae100.txt", sep = "\t")

print_cluster_counts(ca_ae_1, "1")
print_cluster_counts(ca_ae_2, "2")
print_cluster_counts(ca_ae_3, "3")
print_cluster_counts(ca_ae_4, "4")
print_cluster_counts(ca_ae_5, "5")
print_cluster_counts(ca_ae_6, "6")
print_cluster_counts(ca_ae_7, "7")
print_cluster_counts(ca_ae_8, "8")
print_cluster_counts(ca_ae_9, "9")
print_cluster_counts(ca_ae_10, "10")
print_cluster_counts(ca_ae_11, "11")
print_cluster_counts(ca_ae_12, "12")
print_cluster_counts(ca_ae_13, "13")
print_cluster_counts(ca_ae_14, "14")
print_cluster_counts(ca_ae_15, "15")
print_cluster_counts(ca_ae_16, "16")
print_cluster_counts(ca_ae_17, "17")
print_cluster_counts(ca_ae_18, "18")
print_cluster_counts(ca_ae_19, "19")
print_cluster_counts(ca_ae_20, "20")
print_cluster_counts(ca_ae_21, "21")
print_cluster_counts(ca_ae_22, "22")
print_cluster_counts(ca_ae_23, "23")
print_cluster_counts(ca_ae_24, "24")
print_cluster_counts(ca_ae_25, "25")
print_cluster_counts(ca_ae_26, "26")
print_cluster_counts(ca_ae_27, "27")
print_cluster_counts(ca_ae_28, "28")
print_cluster_counts(ca_ae_29, "29")
print_cluster_counts(ca_ae_30, "30")
print_cluster_counts(ca_ae_31, "31")
print_cluster_counts(ca_ae_32, "32")
print_cluster_counts(ca_ae_33, "33")
print_cluster_counts(ca_ae_34, "34")
print_cluster_counts(ca_ae_35, "35")
print_cluster_counts(ca_ae_36, "36")
print_cluster_counts(ca_ae_37, "37")
print_cluster_counts(ca_ae_38, "38")
print_cluster_counts(ca_ae_39, "39")
print_cluster_counts(ca_ae_40, "40")
print_cluster_counts(ca_ae_41, "41")
print_cluster_counts(ca_ae_42, "42")
print_cluster_counts(ca_ae_43, "43")
print_cluster_counts(ca_ae_44, "44")
print_cluster_counts(ca_ae_45, "45")
print_cluster_counts(ca_ae_46, "46")
print_cluster_counts(ca_ae_47, "47")
print_cluster_counts(ca_ae_48, "48")
print_cluster_counts(ca_ae_49, "49")
print_cluster_counts(ca_ae_50, "50")
print_cluster_counts(ca_ae_51, "51")
print_cluster_counts(ca_ae_52, "52")
print_cluster_counts(ca_ae_53, "53")
print_cluster_counts(ca_ae_54, "54")
print_cluster_counts(ca_ae_55, "55")
print_cluster_counts(ca_ae_56, "56")
print_cluster_counts(ca_ae_57, "57")
print_cluster_counts(ca_ae_58, "58")
print_cluster_counts(ca_ae_59, "59")
print_cluster_counts(ca_ae_60, "60")
print_cluster_counts(ca_ae_61, "61")
print_cluster_counts(ca_ae_62, "62")
print_cluster_counts(ca_ae_63, "63")
print_cluster_counts(ca_ae_64, "64")
print_cluster_counts(ca_ae_65, "65")
print_cluster_counts(ca_ae_66, "66")
print_cluster_counts(ca_ae_67, "67")
print_cluster_counts(ca_ae_68, "68")
print_cluster_counts(ca_ae_69, "69")
print_cluster_counts(ca_ae_70, "70")
print_cluster_counts(ca_ae_71, "71")
print_cluster_counts(ca_ae_72, "72")
print_cluster_counts(ca_ae_73, "73")
print_cluster_counts(ca_ae_74, "74")
print_cluster_counts(ca_ae_75, "75")
print_cluster_counts(ca_ae_76, "76")
print_cluster_counts(ca_ae_77, "77")
print_cluster_counts(ca_ae_78, "78")
print_cluster_counts(ca_ae_79, "79")
print_cluster_counts(ca_ae_80, "80")
print_cluster_counts(ca_ae_81, "81")
print_cluster_counts(ca_ae_82, "82")
print_cluster_counts(ca_ae_83, "83")
print_cluster_counts(ca_ae_84, "84")
print_cluster_counts(ca_ae_85, "85")
print_cluster_counts(ca_ae_86, "86")
print_cluster_counts(ca_ae_87, "87")
print_cluster_counts(ca_ae_88, "88")
print_cluster_counts(ca_ae_89, "89")
print_cluster_counts(ca_ae_90, "90")
print_cluster_counts(ca_ae_91, "91")
print_cluster_counts(ca_ae_92, "92")
print_cluster_counts(ca_ae_93, "93")
print_cluster_counts(ca_ae_94, "94")
print_cluster_counts(ca_ae_95, "95")
print_cluster_counts(ca_ae_96, "96")
print_cluster_counts(ca_ae_97, "97")
print_cluster_counts(ca_ae_98, "98")
print_cluster_counts(ca_ae_99, "99")
print_cluster_counts(ca_ae_100, "100")

c_1 <- ca_ae_1$Cluster
c_2 <- ca_ae_2$Cluster
c_3 <- ca_ae_3$Cluster
c_4 <- ca_ae_4$Cluster
c_5 <- ca_ae_5$Cluster
c_6 <- ca_ae_6$Cluster
c_7 <- ca_ae_7$Cluster
c_8 <- ca_ae_8$Cluster
c_9 <- ca_ae_9$Cluster
c_10 <- ca_ae_10$Cluster
c_11 <- ca_ae_11$Cluster
c_12 <- ca_ae_12$Cluster
c_13 <- ca_ae_13$Cluster
c_14 <- ca_ae_14$Cluster
c_15 <- ca_ae_15$Cluster
c_16 <- ca_ae_16$Cluster
c_17 <- ca_ae_17$Cluster
c_18 <- ca_ae_18$Cluster
c_19 <- ca_ae_19$Cluster
c_20 <- ca_ae_20$Cluster
c_21 <- ca_ae_21$Cluster
c_22 <- ca_ae_22$Cluster
c_23 <- ca_ae_23$Cluster
c_24 <- ca_ae_24$Cluster
c_25 <- ca_ae_25$Cluster
c_26 <- ca_ae_26$Cluster
c_27 <- ca_ae_27$Cluster
c_28 <- ca_ae_28$Cluster
c_29 <- ca_ae_29$Cluster
c_30 <- ca_ae_30$Cluster
c_31 <- ca_ae_31$Cluster
c_32 <- ca_ae_32$Cluster
c_33 <- ca_ae_33$Cluster
c_34 <- ca_ae_34$Cluster
c_35 <- ca_ae_35$Cluster
c_36 <- ca_ae_36$Cluster
c_37 <- ca_ae_37$Cluster
c_38 <- ca_ae_38$Cluster
c_39 <- ca_ae_39$Cluster
c_40 <- ca_ae_40$Cluster
c_41 <- ca_ae_41$Cluster
c_42 <- ca_ae_42$Cluster
c_43 <- ca_ae_43$Cluster
c_44 <- ca_ae_44$Cluster
c_45 <- ca_ae_45$Cluster
c_46 <- ca_ae_46$Cluster
c_47 <- ca_ae_47$Cluster
c_48 <- ca_ae_48$Cluster
c_49 <- ca_ae_49$Cluster
c_50 <- ca_ae_50$Cluster
c_51 <- ca_ae_51$Cluster
c_52 <- ca_ae_52$Cluster
c_53 <- ca_ae_53$Cluster
c_54 <- ca_ae_54$Cluster
c_55 <- ca_ae_55$Cluster
c_56 <- ca_ae_56$Cluster
c_57 <- ca_ae_57$Cluster
c_58 <- ca_ae_58$Cluster
c_59 <- ca_ae_59$Cluster
c_60 <- ca_ae_60$Cluster
c_61 <- ca_ae_61$Cluster
c_62 <- ca_ae_62$Cluster
c_63 <- ca_ae_63$Cluster
c_64 <- ca_ae_64$Cluster
c_65 <- ca_ae_65$Cluster
c_66 <- ca_ae_66$Cluster
c_67 <- ca_ae_67$Cluster
c_68 <- ca_ae_68$Cluster
c_69 <- ca_ae_69$Cluster
c_70 <- ca_ae_70$Cluster
c_71 <- ca_ae_71$Cluster
c_72 <- ca_ae_72$Cluster
c_73 <- ca_ae_73$Cluster
c_74 <- ca_ae_74$Cluster
c_75 <- ca_ae_75$Cluster
c_76 <- ca_ae_76$Cluster
c_77 <- ca_ae_77$Cluster
c_78 <- ca_ae_78$Cluster
c_79 <- ca_ae_79$Cluster
c_80 <- ca_ae_80$Cluster
c_81 <- ca_ae_81$Cluster
c_82 <- ca_ae_82$Cluster
c_83 <- ca_ae_83$Cluster
c_84 <- ca_ae_84$Cluster
c_85 <- ca_ae_85$Cluster
c_86 <- ca_ae_86$Cluster
c_87 <- ca_ae_87$Cluster
c_88 <- ca_ae_88$Cluster
c_89 <- ca_ae_89$Cluster
c_90 <- ca_ae_90$Cluster
c_91 <- ca_ae_91$Cluster
c_92 <- ca_ae_92$Cluster
c_93 <- ca_ae_93$Cluster
c_94 <- ca_ae_94$Cluster
c_95 <- ca_ae_95$Cluster
c_96 <- ca_ae_96$Cluster
c_97 <- ca_ae_97$Cluster
c_98 <- ca_ae_98$Cluster
c_99 <- ca_ae_99$Cluster
c_100 <- ca_ae_100$Cluster

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
   # print(paste0("C",i," - C",j,": ",value))
  }
}
