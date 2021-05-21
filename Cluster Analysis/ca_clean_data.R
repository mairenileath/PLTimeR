rm(list = ls())

library(data.table)

df_feat <- read.csv("combined_summary.txt", sep =  "\t")
feature_list <- c( "number_of_snvs",
                   "number_of_indels",
                   "number_ins",
                   "number_del",
                   "number_complex",
                   "number_of_rearrangements",
                   "rearrangements_inversion",
                   "rearrangements_deletion",
                   "rearrangements_tandemDuplication",
                   "rearrangements_translocation", 
                   "ploidy",
                   "pga_clonal",
                   "pga_subclonal",
                   "pga_total",
                   "CT2",
                   "CT_prop",
                   "CT_per_sample",
                   "CT_max_size",
                   "kataegis",
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
                     "LOH.22.42892333.49978151",
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
                     "GAIN.21.40296213.42847185",
                     "HD.8.91936.13324121",
                     "HD.8.13473456.13653983",
                     "HD.8.14318618.14602515",
                     "HD.8.26027842.26319993",
                     "HD.10.89599354.90296426"
)
df_feat$number_of_snvs <- as.numeric(df_feat$number_of_snvs)              
df_feat$number_of_indels <- as.numeric(df_feat$number_of_indels)               
df_feat$number_ins <- as.numeric(df_feat$number_ins)                       
df_feat$number_del <- as.numeric(df_feat$number_del)
df_feat$number_complex <- as.numeric(df_feat$number_complex)
df_feat$number_of_rearrangements <- as.numeric(df_feat$number_of_rearrangements)         
df_feat$rearrangements_inversion <- as.numeric(df_feat$rearrangements_inversion)        
df_feat$rearrangements_deletion <- as.numeric(df_feat$rearrangements_deletion)          
df_feat$rearrangements_tandemDuplication <- as.numeric(df_feat$rearrangements_tandemDuplication)
df_feat$rearrangements_translocation <- as.numeric(df_feat$rearrangements_translocation)     
df_feat$ploidy <- as.numeric(df_feat$ploidy)                          
df_feat$pga_clonal <- as.numeric(df_feat$pga_clonal)                       
df_feat$pga_subclonal <- as.numeric(df_feat$pga_subclonal)                   
df_feat$pga_total <- as.numeric(df_feat$pga_total)                        
df_feat$CT2 <- as.numeric(df_feat$CT2)                             
df_feat$CT_prop <- as.numeric(df_feat$CT_prop)                          
df_feat$CT_per_sample <- as.numeric(df_feat$CT_per_sample)                   
df_feat$CT_max_size <- as.numeric(df_feat$CT_max_size)                      
df_feat$kataegis <- as.numeric(df_feat$kataegis)                        
df_feat$LOH.1.77736122.78057291 <- as.numeric(df_feat$LOH.1.77736122.78057291)    
df_feat$LOH.1.226163330.226181186 <- as.numeric(df_feat$LOH.1.226163330.226181186)
df_feat$LOH.1.226206724.229440190 <- as.numeric(df_feat$LOH.1.226206724.229440190)      
df_feat$LOH.1.229475097.229624424 <- as.numeric(df_feat$LOH.1.229475097.229624424)      
df_feat$LOH.2.119267760.153588354 <- as.numeric(df_feat$LOH.2.119267760.153588354)      
df_feat$LOH.3.69378594.75354435 <- as.numeric(df_feat$LOH.3.69378594.75354435)    
df_feat$LOH.3.75404459.75793226 <- as.numeric(df_feat$LOH.3.75404459.75793226)    
df_feat$LOH.5.51726903.80627173 <- as.numeric(df_feat$LOH.5.51726903.80627173)    
df_feat$LOH.5.83613678.120417719 <- as.numeric(df_feat$LOH.5.83613678.120417719)   
df_feat$LOH.6.65260011.127619070 <- as.numeric(df_feat$LOH.6.65260011.127619070)   
df_feat$LOH.6.128027552.128062827 <- as.numeric(df_feat$LOH.6.128027552.128062827)      
df_feat$LOH.6.167188492.168295503 <- as.numeric(df_feat$LOH.6.167188492.168295503)      
df_feat$LOH.6.168321380.168761235 <- as.numeric(df_feat$LOH.6.168321380.168761235)      
df_feat$LOH.8.143207.46902612 <- as.numeric(df_feat$LOH.8.143207.46902612)      
df_feat$LOH.10.48438147.54116478 <- as.numeric(df_feat$LOH.10.48438147.54116478)   
df_feat$LOH.10.57983428.58248765 <- as.numeric(df_feat$LOH.10.57983428.58248765)   
df_feat$LOH.10.72155618.72311454 <- as.numeric(df_feat$LOH.10.72155618.72311454)   
df_feat$LOH.10.72328063.120603780 <- as.numeric(df_feat$LOH.10.72328063.120603780)      
df_feat$LOH.10.120661390.123820380 <- as.numeric(df_feat$LOH.10.120661390.123820380)     
df_feat$LOH.10.129362163.130765792 <- as.numeric(df_feat$LOH.10.129362163.130765792)     
df_feat$LOH.10.130942659.131830640 <- as.numeric(df_feat$LOH.10.130942659.131830640)     
df_feat$LOH.11.113582559.116199943 <- as.numeric(df_feat$LOH.11.113582559.116199943)     
df_feat$LOH.12.202094.31275837 <- as.numeric(df_feat$LOH.12.202094.31275837)     
df_feat$LOH.13.23350255.23504568 <- as.numeric(df_feat$LOH.13.23350255.23504568)   
df_feat$LOH.13.24319296.24384892 <- as.numeric(df_feat$LOH.13.24319296.24384892)   
df_feat$LOH.13.24407245.24468832 <- as.numeric(df_feat$LOH.13.24407245.24468832)   
df_feat$LOH.13.30113378.114986633 <- as.numeric(df_feat$LOH.13.30113378.114986633)      
df_feat$LOH.16.47665148.89998794 <- as.numeric(df_feat$LOH.16.47665148.89998794)   
df_feat$LOH.17.1880.20873417 <- as.numeric(df_feat$LOH.17.1880.20873417)       
df_feat$LOH.17.39942449.45107361 <- as.numeric(df_feat$LOH.17.39942449.45107361)   
df_feat$LOH.18.132649.78015230 <- as.numeric(df_feat$LOH.18.132649.78015230)     
df_feat$LOH.21.39844933.42882432 <- as.numeric(df_feat$LOH.21.39844933.42882432)   
df_feat$LOH.22.16857983.23442916 <- as.numeric(df_feat$LOH.22.16857983.23442916)   
df_feat$LOH.22.23829276.33635996 <- as.numeric(df_feat$LOH.22.23829276.33635996)   
df_feat$LOH.22.42570458.42867332 <- as.numeric(df_feat$LOH.22.42570458.42867332)   
df_feat$LOH.22.42892333.49978151 <- as.numeric(df_feat$LOH.22.42892333.49978151)   
df_feat$GAIN.3.10248974.11167269 <- as.numeric(df_feat$GAIN.3.10248974.11167269)   
df_feat$GAIN.3.11179976.23927051 <- as.numeric(df_feat$GAIN.3.11179976.23927051)   
df_feat$GAIN.3.24014059.24078594 <- as.numeric(df_feat$GAIN.3.24014059.24078594)   
df_feat$GAIN.3.25148943.26116588 <- as.numeric(df_feat$GAIN.3.25148943.26116588)   
df_feat$GAIN.3.28721084.29119291 <- as.numeric(df_feat$GAIN.3.28721084.29119291)   
df_feat$GAIN.3.30119957.32226009 <- as.numeric(df_feat$GAIN.3.30119957.32226009)   
df_feat$GAIN.3.32553110.40632166 <- as.numeric(df_feat$GAIN.3.32553110.40632166)   
df_feat$GAIN.3.59111534.60076397 <- as.numeric(df_feat$GAIN.3.59111534.60076397)   
df_feat$GAIN.3.65965346.66116448 <- as.numeric(df_feat$GAIN.3.65965346.66116448)   
df_feat$GAIN.3.66852327.67740402 <- as.numeric(df_feat$GAIN.3.66852327.67740402)   
df_feat$GAIN.3.70061492.70129131 <- as.numeric(df_feat$GAIN.3.70061492.70129131)   
df_feat$GAIN.3.70683487.70705259 <- as.numeric(df_feat$GAIN.3.70683487.70705259)   
df_feat$GAIN.3.71981056.72868666 <- as.numeric(df_feat$GAIN.3.71981056.72868666)   
df_feat$GAIN.3.83895346.84906448 <- as.numeric(df_feat$GAIN.3.83895346.84906448)   
df_feat$GAIN.3.94711260.94828954 <- as.numeric(df_feat$GAIN.3.94711260.94828954)   
df_feat$GAIN.3.94896654.95163558 <- as.numeric(df_feat$GAIN.3.94896654.95163558)   
df_feat$GAIN.3.95427443.197842892 <- as.numeric(df_feat$GAIN.3.95427443.197842892)      
df_feat$GAIN.5.1836908.2156619 <- as.numeric(df_feat$GAIN.5.1836908.2156619)     
df_feat$GAIN.5.8459661.8801834 <- as.numeric(df_feat$GAIN.5.8459661.8801834)     
df_feat$GAIN.5.23681090.24207077 <- as.numeric(df_feat$GAIN.5.23681090.24207077)   
df_feat$GAIN.5.25965614.26078933 <- as.numeric(df_feat$GAIN.5.25965614.26078933)   
df_feat$GAIN.5.26532989.39334260 <- as.numeric(df_feat$GAIN.5.26532989.39334260)   
df_feat$GAIN.7.81237.159122682 <- as.numeric(df_feat$GAIN.7.81237.159122682)     
df_feat$GAIN.8.165405.4926708 <- as.numeric(df_feat$GAIN.8.165405.4926708)      
df_feat$GAIN.8.6116657.6273642 <- as.numeric(df_feat$GAIN.8.6116657.6273642)     
df_feat$GAIN.8.6556248.11074036 <- as.numeric(df_feat$GAIN.8.6556248.11074036)    
df_feat$GAIN.8.11131836.20691579 <- as.numeric(df_feat$GAIN.8.11131836.20691579)   
df_feat$GAIN.8.21616063.146300287 <- as.numeric(df_feat$GAIN.8.21616063.146300287)      
df_feat$GAIN.9.88440920.90458137 <- as.numeric(df_feat$GAIN.9.88440920.90458137)   
df_feat$GAIN.9.90623475.91998121 <- as.numeric(df_feat$GAIN.9.90623475.91998121)   
df_feat$GAIN.9.93871851.100211938 <- as.numeric(df_feat$GAIN.9.93871851.100211938)      
df_feat$GAIN.9.100254594.102220012 <- as.numeric(df_feat$GAIN.9.100254594.102220012)     
df_feat$GAIN.9.104627014.105935999 <- as.numeric(df_feat$GAIN.9.104627014.105935999)     
df_feat$GAIN.9.107008238.107494862 <- as.numeric(df_feat$GAIN.9.107008238.107494862)     
df_feat$GAIN.9.107858523.113406445 <- as.numeric(df_feat$GAIN.9.107858523.113406445)     
df_feat$GAIN.9.113928964.114056169 <- as.numeric(df_feat$GAIN.9.113928964.114056169)     
df_feat$GAIN.9.119919154.120426703 <- as.numeric(df_feat$GAIN.9.119919154.120426703)     
df_feat$GAIN.9.121403691.122781254 <- as.numeric(df_feat$GAIN.9.121403691.122781254)     
df_feat$GAIN.9.138151943.138298164 <- as.numeric(df_feat$GAIN.9.138151943.138298164)     
df_feat$GAIN.16.109665.20349054 <- as.numeric(df_feat$GAIN.16.109665.20349054)    
df_feat$GAIN.21.40296213.42847185 <- as.numeric(df_feat$GAIN.21.40296213.42847185)      
df_feat$HD.8.91936.13324121 <- as.numeric(df_feat$HD.8.91936.13324121)        
df_feat$HD.8.13473456.13653983 <- as.numeric(df_feat$HD.8.13473456.13653983)     
df_feat$HD.8.14318618.14602515 <- as.numeric(df_feat$HD.8.14318618.14602515)     
df_feat$HD.8.26027842.26319993 <- as.numeric(df_feat$HD.8.26027842.26319993)     
df_feat$HD.10.89599354.90296426 <- as.numeric(df_feat$HD.10.89599354.90296426)     

df_feat_var <- sapply(df_feat, var)
invalid_feat <- df_feat_var[df_feat_var == 0]

df_feat <- subset(df_feat, select = -c(number_complex,CT2))

df_feat <- as.data.frame(df_feat)
setDT(df_feat, keep.rownames = TRUE)
colnames(df_feat)[1] <- "Sample_ID"
df_feat <- as.data.frame(df_feat)

#write.table(df_feat, file = paste0("PPCG.txt"), sep="\t",quote=F)