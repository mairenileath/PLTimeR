library(mclust)

print_cluster_counts <- function(data, title) {
  c1 <- nrow(data[data$Cluster == "1",])
  c2 <- nrow(data[data$Cluster == "2",])
  print(paste(title, "C1", c1))
  print(paste(title, "C2", c2))
}

ca_som_1 <- read.csv("CA_som1.txt", sep = "\t")
ca_som_2 <- read.csv("CA_som2.txt", sep = "\t")
ca_som_3 <- read.csv("CA_som3.txt", sep = "\t")
ca_som_4 <- read.csv("CA_som4.txt", sep = "\t")
ca_som_5 <- read.csv("CA_som5.txt", sep = "\t")
ca_som_6 <- read.csv("CA_som6.txt", sep = "\t")
ca_som_7 <- read.csv("CA_som7.txt", sep = "\t")
ca_som_8 <- read.csv("CA_som8.txt", sep = "\t")
ca_som_9 <- read.csv("CA_som9.txt", sep = "\t")
ca_som_10 <- read.csv("CA_som10.txt", sep = "\t")
ca_som_11 <- read.csv("CA_som11.txt", sep = "\t")
ca_som_12 <- read.csv("CA_som12.txt", sep = "\t")
ca_som_13 <- read.csv("CA_som13.txt", sep = "\t")
ca_som_14 <- read.csv("CA_som14.txt", sep = "\t")
ca_som_15 <- read.csv("CA_som15.txt", sep = "\t")
ca_som_16 <- read.csv("CA_som16.txt", sep = "\t")
ca_som_17 <- read.csv("CA_som17.txt", sep = "\t")
ca_som_18 <- read.csv("CA_som18.txt", sep = "\t")
ca_som_19 <- read.csv("CA_som19.txt", sep = "\t")
ca_som_20 <- read.csv("CA_som20.txt", sep = "\t")
ca_som_21 <- read.csv("CA_som21.txt", sep = "\t")
ca_som_22 <- read.csv("CA_som22.txt", sep = "\t")
ca_som_23 <- read.csv("CA_som23.txt", sep = "\t")
ca_som_24 <- read.csv("CA_som24.txt", sep = "\t")
ca_som_25 <- read.csv("CA_som25.txt", sep = "\t")
ca_som_26 <- read.csv("CA_som26.txt", sep = "\t")
ca_som_27 <- read.csv("CA_som27.txt", sep = "\t")
ca_som_28 <- read.csv("CA_som28.txt", sep = "\t")
ca_som_29 <- read.csv("CA_som29.txt", sep = "\t")
ca_som_30 <- read.csv("CA_som30.txt", sep = "\t")
ca_som_31 <- read.csv("CA_som31.txt", sep = "\t")
ca_som_32 <- read.csv("CA_som32.txt", sep = "\t")
ca_som_33 <- read.csv("CA_som33.txt", sep = "\t")
ca_som_34 <- read.csv("CA_som34.txt", sep = "\t")
ca_som_35 <- read.csv("CA_som35.txt", sep = "\t")
ca_som_36 <- read.csv("CA_som36.txt", sep = "\t")
ca_som_37 <- read.csv("CA_som37.txt", sep = "\t")
ca_som_38 <- read.csv("CA_som38.txt", sep = "\t")
ca_som_39 <- read.csv("CA_som39.txt", sep = "\t")
ca_som_40 <- read.csv("CA_som40.txt", sep = "\t")
ca_som_41 <- read.csv("CA_som41.txt", sep = "\t")
ca_som_42 <- read.csv("CA_som42.txt", sep = "\t")
ca_som_43 <- read.csv("CA_som43.txt", sep = "\t")
ca_som_44 <- read.csv("CA_som44.txt", sep = "\t")
ca_som_45 <- read.csv("CA_som45.txt", sep = "\t")
ca_som_46 <- read.csv("CA_som46.txt", sep = "\t")
ca_som_47 <- read.csv("CA_som47.txt", sep = "\t")
ca_som_48 <- read.csv("CA_som48.txt", sep = "\t")
ca_som_49 <- read.csv("CA_som49.txt", sep = "\t")
ca_som_50 <- read.csv("CA_som50.txt", sep = "\t")
ca_som_51 <- read.csv("CA_som51.txt", sep = "\t")
ca_som_52 <- read.csv("CA_som52.txt", sep = "\t")
ca_som_53 <- read.csv("CA_som53.txt", sep = "\t")
ca_som_54 <- read.csv("CA_som54.txt", sep = "\t")
ca_som_55 <- read.csv("CA_som55.txt", sep = "\t")
ca_som_56 <- read.csv("CA_som56.txt", sep = "\t")
ca_som_57 <- read.csv("CA_som57.txt", sep = "\t")
ca_som_58 <- read.csv("CA_som58.txt", sep = "\t")
ca_som_59 <- read.csv("CA_som59.txt", sep = "\t")
ca_som_60 <- read.csv("CA_som60.txt", sep = "\t")
ca_som_61 <- read.csv("CA_som61.txt", sep = "\t")
ca_som_62 <- read.csv("CA_som62.txt", sep = "\t")
ca_som_63 <- read.csv("CA_som63.txt", sep = "\t")
ca_som_64 <- read.csv("CA_som64.txt", sep = "\t")
ca_som_65 <- read.csv("CA_som65.txt", sep = "\t")
ca_som_66 <- read.csv("CA_som66.txt", sep = "\t")
ca_som_67 <- read.csv("CA_som67.txt", sep = "\t")
ca_som_68 <- read.csv("CA_som68.txt", sep = "\t")
ca_som_69 <- read.csv("CA_som69.txt", sep = "\t")
ca_som_70 <- read.csv("CA_som70.txt", sep = "\t")
ca_som_71 <- read.csv("CA_som71.txt", sep = "\t")
ca_som_72 <- read.csv("CA_som72.txt", sep = "\t")
ca_som_73 <- read.csv("CA_som73.txt", sep = "\t")
ca_som_74 <- read.csv("CA_som74.txt", sep = "\t")
ca_som_75 <- read.csv("CA_som75.txt", sep = "\t")
ca_som_76 <- read.csv("CA_som76.txt", sep = "\t")
ca_som_77 <- read.csv("CA_som77.txt", sep = "\t")
ca_som_78 <- read.csv("CA_som78.txt", sep = "\t")
ca_som_79 <- read.csv("CA_som79.txt", sep = "\t")
ca_som_80 <- read.csv("CA_som80.txt", sep = "\t")
ca_som_81 <- read.csv("CA_som81.txt", sep = "\t")
ca_som_82 <- read.csv("CA_som82.txt", sep = "\t")
ca_som_83 <- read.csv("CA_som83.txt", sep = "\t")
ca_som_84 <- read.csv("CA_som84.txt", sep = "\t")
ca_som_85 <- read.csv("CA_som85.txt", sep = "\t")
ca_som_86 <- read.csv("CA_som86.txt", sep = "\t")
ca_som_87 <- read.csv("CA_som87.txt", sep = "\t")
ca_som_88 <- read.csv("CA_som88.txt", sep = "\t")
ca_som_89 <- read.csv("CA_som89.txt", sep = "\t")
ca_som_90 <- read.csv("CA_som90.txt", sep = "\t")
ca_som_91 <- read.csv("CA_som91.txt", sep = "\t")
ca_som_92 <- read.csv("CA_som92.txt", sep = "\t")
ca_som_93 <- read.csv("CA_som93.txt", sep = "\t")
ca_som_94 <- read.csv("CA_som94.txt", sep = "\t")
ca_som_95 <- read.csv("CA_som95.txt", sep = "\t")
ca_som_96 <- read.csv("CA_som96.txt", sep = "\t")
ca_som_97 <- read.csv("CA_som97.txt", sep = "\t")
ca_som_98 <- read.csv("CA_som98.txt", sep = "\t")
ca_som_99 <- read.csv("CA_som99.txt", sep = "\t")
ca_som_100 <- read.csv("CA_som100.txt", sep = "\t")

print_cluster_counts(ca_som_1, "1")
print_cluster_counts(ca_som_2, "2")
print_cluster_counts(ca_som_3, "3")
print_cluster_counts(ca_som_4, "4")
print_cluster_counts(ca_som_5, "5")
print_cluster_counts(ca_som_6, "6")
print_cluster_counts(ca_som_7, "7")
print_cluster_counts(ca_som_8, "8")
print_cluster_counts(ca_som_9, "9")
print_cluster_counts(ca_som_10, "10")
print_cluster_counts(ca_som_11, "11")
print_cluster_counts(ca_som_12, "12")
print_cluster_counts(ca_som_13, "13")
print_cluster_counts(ca_som_14, "14")
print_cluster_counts(ca_som_15, "15")
print_cluster_counts(ca_som_16, "16")
print_cluster_counts(ca_som_17, "17")
print_cluster_counts(ca_som_18, "18")
print_cluster_counts(ca_som_19, "19")
print_cluster_counts(ca_som_20, "20")
print_cluster_counts(ca_som_21, "21")
print_cluster_counts(ca_som_22, "22")
print_cluster_counts(ca_som_23, "23")
print_cluster_counts(ca_som_24, "24")
print_cluster_counts(ca_som_25, "25")
print_cluster_counts(ca_som_26, "26")
print_cluster_counts(ca_som_27, "27")
print_cluster_counts(ca_som_28, "28")
print_cluster_counts(ca_som_29, "29")
print_cluster_counts(ca_som_30, "30")
print_cluster_counts(ca_som_31, "31")
print_cluster_counts(ca_som_32, "32")
print_cluster_counts(ca_som_33, "33")
print_cluster_counts(ca_som_34, "34")
print_cluster_counts(ca_som_35, "35")
print_cluster_counts(ca_som_36, "36")
print_cluster_counts(ca_som_37, "37")
print_cluster_counts(ca_som_38, "38")
print_cluster_counts(ca_som_39, "39")
print_cluster_counts(ca_som_40, "40")
print_cluster_counts(ca_som_41, "41")
print_cluster_counts(ca_som_42, "42")
print_cluster_counts(ca_som_43, "43")
print_cluster_counts(ca_som_44, "44")
print_cluster_counts(ca_som_45, "45")
print_cluster_counts(ca_som_46, "46")
print_cluster_counts(ca_som_47, "47")
print_cluster_counts(ca_som_48, "48")
print_cluster_counts(ca_som_49, "49")
print_cluster_counts(ca_som_50, "50")
print_cluster_counts(ca_som_51, "51")
print_cluster_counts(ca_som_52, "52")
print_cluster_counts(ca_som_53, "53")
print_cluster_counts(ca_som_54, "54")
print_cluster_counts(ca_som_55, "55")
print_cluster_counts(ca_som_56, "56")
print_cluster_counts(ca_som_57, "57")
print_cluster_counts(ca_som_58, "58")
print_cluster_counts(ca_som_59, "59")
print_cluster_counts(ca_som_60, "60")
print_cluster_counts(ca_som_61, "61")
print_cluster_counts(ca_som_62, "62")
print_cluster_counts(ca_som_63, "63")
print_cluster_counts(ca_som_64, "64")
print_cluster_counts(ca_som_65, "65")
print_cluster_counts(ca_som_66, "66")
print_cluster_counts(ca_som_67, "67")
print_cluster_counts(ca_som_68, "68")
print_cluster_counts(ca_som_69, "69")
print_cluster_counts(ca_som_70, "70")
print_cluster_counts(ca_som_71, "71")
print_cluster_counts(ca_som_72, "72")
print_cluster_counts(ca_som_73, "73")
print_cluster_counts(ca_som_74, "74")
print_cluster_counts(ca_som_75, "75")
print_cluster_counts(ca_som_76, "76")
print_cluster_counts(ca_som_77, "77")
print_cluster_counts(ca_som_78, "78")
print_cluster_counts(ca_som_79, "79")
print_cluster_counts(ca_som_80, "80")
print_cluster_counts(ca_som_81, "81")
print_cluster_counts(ca_som_82, "82")
print_cluster_counts(ca_som_83, "83")
print_cluster_counts(ca_som_84, "84")
print_cluster_counts(ca_som_85, "85")
print_cluster_counts(ca_som_86, "86")
print_cluster_counts(ca_som_87, "87")
print_cluster_counts(ca_som_88, "88")
print_cluster_counts(ca_som_89, "89")
print_cluster_counts(ca_som_90, "90")
print_cluster_counts(ca_som_91, "91")
print_cluster_counts(ca_som_92, "92")
print_cluster_counts(ca_som_93, "93")
print_cluster_counts(ca_som_94, "94")
print_cluster_counts(ca_som_95, "95")
print_cluster_counts(ca_som_96, "96")
print_cluster_counts(ca_som_97, "97")
print_cluster_counts(ca_som_98, "98")
print_cluster_counts(ca_som_99, "99")
print_cluster_counts(ca_som_100, "100")

c_1 <- ca_som_1$Cluster
c_2 <- ca_som_2$Cluster
c_3 <- ca_som_3$Cluster
c_4 <- ca_som_4$Cluster
c_5 <- ca_som_5$Cluster
c_6 <- ca_som_6$Cluster
c_7 <- ca_som_7$Cluster
c_8 <- ca_som_8$Cluster
c_9 <- ca_som_9$Cluster
c_10 <- ca_som_10$Cluster
c_11 <- ca_som_11$Cluster
c_12 <- ca_som_12$Cluster
c_13 <- ca_som_13$Cluster
c_14 <- ca_som_14$Cluster
c_15 <- ca_som_15$Cluster
c_16 <- ca_som_16$Cluster
c_17 <- ca_som_17$Cluster
c_18 <- ca_som_18$Cluster
c_19 <- ca_som_19$Cluster
c_20 <- ca_som_20$Cluster
c_21 <- ca_som_21$Cluster
c_22 <- ca_som_22$Cluster
c_23 <- ca_som_23$Cluster
c_24 <- ca_som_24$Cluster
c_25 <- ca_som_25$Cluster
c_26 <- ca_som_26$Cluster
c_27 <- ca_som_27$Cluster
c_28 <- ca_som_28$Cluster
c_29 <- ca_som_29$Cluster
c_30 <- ca_som_30$Cluster
c_31 <- ca_som_31$Cluster
c_32 <- ca_som_32$Cluster
c_33 <- ca_som_33$Cluster
c_34 <- ca_som_34$Cluster
c_35 <- ca_som_35$Cluster
c_36 <- ca_som_36$Cluster
c_37 <- ca_som_37$Cluster
c_38 <- ca_som_38$Cluster
c_39 <- ca_som_39$Cluster
c_40 <- ca_som_40$Cluster
c_41 <- ca_som_41$Cluster
c_42 <- ca_som_42$Cluster
c_43 <- ca_som_43$Cluster
c_44 <- ca_som_44$Cluster
c_45 <- ca_som_45$Cluster
c_46 <- ca_som_46$Cluster
c_47 <- ca_som_47$Cluster
c_48 <- ca_som_48$Cluster
c_49 <- ca_som_49$Cluster
c_50 <- ca_som_50$Cluster
c_51 <- ca_som_51$Cluster
c_52 <- ca_som_52$Cluster
c_53 <- ca_som_53$Cluster
c_54 <- ca_som_54$Cluster
c_55 <- ca_som_55$Cluster
c_56 <- ca_som_56$Cluster
c_57 <- ca_som_57$Cluster
c_58 <- ca_som_58$Cluster
c_59 <- ca_som_59$Cluster
c_60 <- ca_som_60$Cluster
c_61 <- ca_som_61$Cluster
c_62 <- ca_som_62$Cluster
c_63 <- ca_som_63$Cluster
c_64 <- ca_som_64$Cluster
c_65 <- ca_som_65$Cluster
c_66 <- ca_som_66$Cluster
c_67 <- ca_som_67$Cluster
c_68 <- ca_som_68$Cluster
c_69 <- ca_som_69$Cluster
c_70 <- ca_som_70$Cluster
c_71 <- ca_som_71$Cluster
c_72 <- ca_som_72$Cluster
c_73 <- ca_som_73$Cluster
c_74 <- ca_som_74$Cluster
c_75 <- ca_som_75$Cluster
c_76 <- ca_som_76$Cluster
c_77 <- ca_som_77$Cluster
c_78 <- ca_som_78$Cluster
c_79 <- ca_som_79$Cluster
c_80 <- ca_som_80$Cluster
c_81 <- ca_som_81$Cluster
c_82 <- ca_som_82$Cluster
c_83 <- ca_som_83$Cluster
c_84 <- ca_som_84$Cluster
c_85 <- ca_som_85$Cluster
c_86 <- ca_som_86$Cluster
c_87 <- ca_som_87$Cluster
c_88 <- ca_som_88$Cluster
c_89 <- ca_som_89$Cluster
c_90 <- ca_som_90$Cluster
c_91 <- ca_som_91$Cluster
c_92 <- ca_som_92$Cluster
c_93 <- ca_som_93$Cluster
c_94 <- ca_som_94$Cluster
c_95 <- ca_som_95$Cluster
c_96 <- ca_som_96$Cluster
c_97 <- ca_som_97$Cluster
c_98 <- ca_som_98$Cluster
c_99 <- ca_som_99$Cluster
c_100 <- ca_som_100$Cluster

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

for (i in 1:length(options)) {
  for (j in 1:length(options)) {
    a <- options[[i]]
    #print(a)
    b <- options[[j]]
    #print(b)
    value <- adjustedRandIndex(a,b)
    #print(paste0("C",i," - C",j,": ",value))
  }
}


write.table(ca_som_1, file = "CA_som.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_2, file = "CA_som2.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_3, file = "CA_som3.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_4, file = "CA_som4.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_5, file = "CA_som5.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_6, file = "CA_som6.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_7, file = "CA_som7.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_8, file = "CA_som8.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_9, file = "CA_som9.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(ca_som_10, file = "CA_som10.txt", sep="\t",quote=F, row.names=FALSE)

