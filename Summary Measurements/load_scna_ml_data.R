rm(list = ls())

library(dplyr)

cna_gain_path="~/data/tm/Generated/PPCG_Gain_mergedsegs.txt"
cna_loh_path="~/data/tm/Generated/PPCG_LOH_mergedsegs.txt"
cna_hd_path="~/data/tm/Generated/PPCG_HD_mergedsegs.txt"

df_files <- read.csv("PPCG_Samples.txt", header=FALSE)

#if there is any overlap between the regions then true

cna_loh <- c(
"LOH.1.77736122.78057291",
"LOH.1.226163330.226181186",
"LOH.1.226206724.229440190",
"LOH.1.229475097.229624424",
"LOH.2.119267760.153588354",
"LOH.3.69378594.75354435",
"LOH.3.75404459.75793226",
"LOH.5.51726903.80627173",
"LOH.5.83613678.120417719",
"LOH.6.65260011.127619070",
"LOH.6.128027552.128062827",
"LOH.6.167188492.168295503",
"LOH.6.168321380.168761235",
"LOH.8.143207.46902612",
"LOH.10.48438147.54116478",
"LOH.10.57983428.58248765",
"LOH.10.72155618.72311454",
"LOH.10.72328063.120603780",
"LOH.10.120661390.123820380",
"LOH.10.129362163.130765792",
"LOH.10.130942659.131830640",
"LOH.11.113582559.116199943",
"LOH.12.202094.31275837",
"LOH.13.23350255.23504568",
"LOH.13.24319296.24384892",
"LOH.13.24407245.24468832",
"LOH.13.30113378.114986633",
"LOH.16.47665148.89998794",
"LOH.17.1880.20873417",
"LOH.17.39942449.45107361",
"LOH.18.132649.78015230",
"LOH.21.39844933.42882432",
"LOH.22.16857983.23442916",
"LOH.22.23829276.33635996",
"LOH.22.42570458.42867332",
"LOH.22.42892333.49978151"
)
cna_gain <- c(
"GAIN.3.10248974.11167269",
"GAIN.3.11179976.23927051",
"GAIN.3.24014059.24078594",
"GAIN.3.25148943.26116588",
"GAIN.3.28721084.29119291",
"GAIN.3.30119957.32226009",
"GAIN.3.32553110.40632166",
"GAIN.3.59111534.60076397",
"GAIN.3.65965346.66116448",
"GAIN.3.66852327.67740402",
"GAIN.3.70061492.70129131",
"GAIN.3.70683487.70705259",
"GAIN.3.71981056.72868666",
"GAIN.3.83895346.84906448",
"GAIN.3.94711260.94828954",
"GAIN.3.94896654.95163558",
"GAIN.3.95427443.197842892",
"GAIN.5.1836908.2156619",
"GAIN.5.8459661.8801834",
"GAIN.5.23681090.24207077",
"GAIN.5.25965614.26078933",
"GAIN.5.26532989.39334260",
"GAIN.7.81237.159122682",
"GAIN.8.165405.4926708",
"GAIN.8.6116657.6273642",
"GAIN.8.6556248.11074036",
"GAIN.8.11131836.20691579",
"GAIN.8.21616063.146300287",
"GAIN.9.88440920.90458137",
"GAIN.9.90623475.91998121",
"GAIN.9.93871851.100211938",
"GAIN.9.100254594.102220012",
"GAIN.9.104627014.105935999",
"GAIN.9.107008238.107494862",
"GAIN.9.107858523.113406445",
"GAIN.9.113928964.114056169",
"GAIN.9.119919154.120426703",
"GAIN.9.121403691.122781254",
"GAIN.9.138151943.138298164",
"GAIN.16.109665.20349054",
"GAIN.21.40296213.42847185"
)
cna_hd <- c(
"HD.8.91936.13324121",
"HD.8.13473456.13653983",
"HD.8.14318618.14602515",
"HD.8.26027842.26319993",
"HD.10.89599354.90296426"
)
cna_loh_header <- append(cna_loh,"Sample_ID",0)
cna_gain_header <- append(cna_gain,"Sample_ID",0) 
cna_hd_header <- append(cna_hd,"Sample_ID",0) 

df_merge_gain <- read.csv(cna_gain_path, sep="\t")
df_merge_loh <- read.csv(cna_loh_path, sep="\t")
df_merge_hd <- read.csv(cna_hd_path, sep="\t")

df_gain <- data.frame(matrix(vector(),0,42,dimnames=list(c(), cna_gain_header)), stringsAsFactors = F)  
df_loh <- data.frame(matrix(vector(),0,37,dimnames=list(c(), cna_loh_header)), stringsAsFactors = F)  
df_hd <- data.frame(matrix(vector(),0,6,dimnames=list(c(), cna_hd_header)), stringsAsFactors = F)

find_cna_presence <- function(sample_id, df_merge, cna_list) {
  values <- c()
  
  df_samples <- df_merge[df_merge$Tumour_Name == sample_id,]

  if (nrow(df_samples) > 0) {
    for (j in 1:length(cna_list)){
      #split cna details
      cna_dets = strsplit(cna_list[j],"\\.")

      cna_chr <- as.numeric(cna_dets[[1]][2])
      cna_start <- as.numeric(cna_dets[[1]][3])
      cna_end <- as.numeric(cna_dets[[1]][4])
      
      #get all rows with that chromosome
      df_chr_samples <- df_samples[df_samples$chr == cna_chr,]
      if (nrow(df_chr_samples) > 0){
        exists = 0

        #loop through these samples until find one that is an example of the cna, else it will add 0
        for (k in 1:nrow(df_chr_samples)) {
          sam_start <- as.numeric(df_chr_samples$startpos)
          sam_end <- as.numeric(df_chr_samples$endpos)
          #To check that there is no overlap they must be EITHER
          #Both values are smaller than both start and end, or bigger than both
          #So NOT no overlap

          #if (!((sam_start < cna_start & sam_end < cna_start & sam_start < cna_end & sam_end < cna_end) | (sam_start > cna_start & sam_end > cna_start & sam_start > cna_end & sam_end > cna_end))){
          #  exists = 1
          #  break
          #}
          
          if (sam_start == cna_start & sam_end == cna_end){
            exists = 1
            break
          }
        }
        values <- append(values, exists)
        
      } else {
        values <- append(values, 0)
      }
    }
  } else {
    values <- replicate(length(cna_list),0)
  }
  values <- append(values, sample_id, 0)
  
  return(values)
}

for (i in 1:nrow(df_files)) {
  
  sample_id <- substr(df_files[i,],0,30)
  
  loh_exists <- find_cna_presence(sample_id, df_merge_loh, cna_loh)
  gain_exists <- find_cna_presence(sample_id, df_merge_gain, cna_gain)
  hd_exists <- find_cna_presence(sample_id, df_merge_hd, cna_hd)
  
  df_loh <- rbind(df_loh, t(loh_exists))
  df_gain <- rbind(df_gain, t(gain_exists))
  df_hd <- rbind(df_hd, t(hd_exists))
}

names(df_gain) <- cna_gain_header
names(df_loh) <- cna_loh_header
names(df_hd) <- cna_hd_header

cna_summary <- merge(df_gain, df_loh, by="Sample_ID")
cna_summary <- merge(cna_summary, df_hd, by="Sample_ID")

cna_summary$Sample_ID <- substr(cna_summary$Sample_ID,0,13)

write.table(cna_summary, file = "cna_summary.txt", sep="\t",quote=F, row.names = FALSE)

