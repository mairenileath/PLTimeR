
## Step 2


#setwd("/gpfs2/well/wedge/atesah/Per_chr_BSA/ARBS_analyses/Groups/Input")

args = commandArgs(TRUE)
sample = toString(args[1])

iterations = 1000
max_dist = 7360000
bin_width = 20000
  
  ## Create output_dir
  
  output_dir <- paste0("./", sample, "/Input/")
  
  if(!file.exists(output_dir)){
    dir.create(output_dir)
  }
  
  
  ## Pool obs & sim dist for each chr seprately
  
  for (i in c(1:22, "X", "Y")) {
    
    file1 = paste0("./", sample, "/Per_chr/chr", i, "/Simulated_data_dist_", i, ".csv")
    
    file2 = paste0("./", sample, "/Per_chr/chr", i, "/tmp00.csv")
    
    if(file.exists(file1) & file.exists(file2)) {
      
      #setwd(paste0("../", sample, "/Per_chr/chr", i))
      
      data1 = as.matrix(read.table(paste0("./", sample, "/Per_chr/chr", i, "/tmp00.csv"),header=F,sep=","))
      
      data2 = as.matrix(read.table(paste0("./", sample, "/Per_chr/chr", i, "/Simulated_data_dist_",i,".csv"),header=F,sep=","))
      
      chr = cbind(data1[,3], data2[,c(-1,-2)])
      
      #setwd(output_dir)
      
      write.table(chr, file = paste0(output_dir, "chr_",i,".csv"), sep =",", col.names = FALSE, row.names = FALSE)
      
    }
    
    else {
      print(paste0("No SVs on chr", i))
    }
    
  }
  
  ## Pool obs & sim dist for all chr in a single file
  
  output = (NULL)
  
  for (h in 0:iterations) {
    o = paste0("dist_", h)
    output = c(output, o)
  }
  
  #setwd(paste0("./", sample, "/Input"))
  
  chrla =(NULL)
  
  for (f in c(1:22, "X", "Y")) {
    
    file3 = paste0("./", sample, "/Per_chr/chr", f, "/Simulated_data_dist_", f, ".csv")
    
    if (file.exists(file3)) {
      chrla = c(chrla, f)
    }
    
    else {
      print(paste0("No SVs on chr", f))
    }
    
  }
  
  for (j in chrla) {
    
    data = as.matrix(read.table(paste0("./", sample, "/Input/chr_",j,".csv"),header=T,sep=","))
    
    output = rbind(output, data)
    
  }
  
  write.table(output, file = paste0("./", sample, "/Input/all_chr.csv"), sep =",", col.names = FALSE, row.names = FALSE)
  
  
  ## Pool obs & sim dist for all chr minus one or more chr in a single file
  
  output = (NULL)
  
  for (h in 0:iterations) {
    o = paste0("dist_", h)
    output = c(output, o)
  }
  
  #setwd(paste0("./", sample, "/Input"))
  
  chrlm =(NULL)
  
  for (g in c(1:20, 22, "X", "Y")) {
    
    file4 = paste0("./", sample, "/Per_chr/chr", g, "/Simulated_data_dist_", g, ".csv")
    
    if (file.exists(file4)) {
      chrlm = c(chrlm, g)
    }
    
    else {
      print(paste0("No SVs on chr", g))
    }
    
  }
  
  for (j in chrlm) {
    
    data = as.matrix(read.table(paste0("./", sample, "/Input/chr_",j,".csv"),header=T,sep=","))
    
    output = rbind(output, data)
    
  }
  
  write.table(output, file = paste0("./", sample, "/Input/min_21.csv"), sep =",", col.names = FALSE, row.names = FALSE)
  
  
  ##################################################################################################################
  
  ## Create output_dir
  
  output_dir <- paste0("./", sample, "/Output/")
  
  if(!file.exists(output_dir)){
    dir.create(output_dir)
  }
  
  plots_dir <- paste0("./", sample, "/Output/All_plots/")
  
  if(!file.exists(plots_dir)){
    dir.create(plots_dir)
  }
  
  sets1 = c("all_", "min_", rep("chr_", times= 24))
  
  sets2 = c("chr", 21, c(1:22), "X", "Y")
  
  for (n in 1:length(sets1)) {
    
    #setwd(paste0("./", sample, "/Input"))
    
    fn1 = sets1[n]
    
    fn2 = sets2[n]
    
    file5 = paste0("./", sample, "/Input/", fn1, fn2,".csv")
    
    if(file.exists(file5)) {
      
      data = as.data.frame(read.table(paste0("./", sample, "/Input/", fn1, fn2,".csv"),header=T,sep=","))
      
      #setwd(output_dir)
      
      set_dir <- paste0("./", sample, "/Output/", fn1, fn2, "/")
      
      if(!file.exists(set_dir)){
        dir.create(set_dir)
      }
      
      #setwd(set_dir)
      
      bin = seq(from= 0, to= max_dist, by= bin_width)
      
      output1 = c("distance", bin)
      
      brpb = c("Obs", sum(data[,1] == 0))
      
      for (i in 2:length(bin)) {
        
        brpc= sum(data[,1] > bin[i-1] & data[,1] <= bin[i])
        
        brpb = c(brpb, brpc)
        
      }
      
      output1 = cbind(output1, brpb)
      
      for (j in 2:(iterations+1)) {
        
        brpb = c(paste0("Sim_", j-1), sum(data[,j] == 0))
        
        for (i in 2:length(bin)) {
          
          brpc= sum(data[,j] > bin[i-1] & data[,j] <= bin[i])
          
          brpb = c(brpb, brpc)
          
        }
        
        output1 = cbind(output1, brpb)
        
      }
      
      write.table(output1, file = paste0(set_dir, sample, "_", fn1, fn2, "_Brp_count_per_bin.csv"), sep =",", col.names = FALSE, row.names = FALSE)
      
      sim_m = "Median"
      
      sim_LCI = "LCI"
      
      sim_UCI = "UCI"
      
      for (k in 2:length(output1[,1])) {
        
        m = median(na.omit(as.numeric(output1[k, 3:(iterations+2)])))
        
        l = quantile(na.omit(as.numeric(output1[k, 3:(iterations+2)])), 0.025)
        
        u = quantile(na.omit(as.numeric(output1[k, 3:(iterations+2)])), 0.975)
        
        sim_m = c(sim_m, m)
        
        sim_LCI = c(sim_LCI, l)
        
        sim_UCI = c(sim_UCI, u)
        
      }
      
      fold= "Obs-Sim"
      
      color= "Color"
      
      obs= output1[,2]
      
      for (distf in 2:length(output1[,1])) {
        
        if (as.numeric(obs[distf]) > as.numeric(sim_UCI[distf])) {
          
          f = (as.numeric(obs[distf])) - (as.numeric(sim_m[distf]))
          
          c = "blue"
          
        }
        
        else if ((as.numeric(obs[distf]) <= as.numeric(sim_UCI[distf])) & (as.numeric(obs[distf]) > as.numeric(sim_m[distf]))) {
          
          f = (as.numeric(obs[distf])) - (as.numeric(sim_m[distf]))
          
          c = "grey"
          
        }
        
        else if (as.numeric(obs[distf]) < as.numeric(sim_LCI[distf])) {
          
          f = (as.numeric(obs[distf])) - (as.numeric(sim_m[distf]))
          
          c = "red"
          
        }
        
        else if ((as.numeric(obs[distf]) >= as.numeric(sim_LCI[distf])) & (as.numeric(sim_m[distf]) > as.numeric(obs[distf]))) {
          
          f = (as.numeric(obs[distf])) - (as.numeric(sim_m[distf]))
          
          c = "grey"
          
        }
        
        else {
          
          f = 0
          
          c = "grey"
          
        }
        
        fold = c(fold, f)
        
        color = c(color, c)
        
      }
      
      output2 = cbind(output1[,1:2], sim_m, sim_LCI, sim_UCI, fold, color)
      
      write.table(output2, file = paste0(set_dir, sample, "_", fn1, fn2, "_Per_bin_plot_data.csv"), sep =",", col.names = FALSE, row.names = FALSE)
      
      png(paste0(set_dir, sample, "_", fn1, fn2,"_500kb_plot_1.png"),
          width = 600,
          height = 300
      )
      
      if (fn1 == "all_") {
        sub = "the whole genome"
      }
      
      if (fn1 == "chr_") {
        sub = paste0("chromosome ", fn2)
      }
      
      if (fn1 == "min_") {
        sub = paste0("the whole genome except chromosome ", fn2)
      }
      
      barp = barplot(as.numeric(output2[3:27,6]),
              col= output2[3:27,7],
              names.arg = seq(from = 20, to = 500, by = 20),
              cex.names = 0.8,
              main = paste0(sample, " SVs on ", sub),
              xlab = "Distance to the nearest ARBS (kbp)",
              ylab = "Observed - simulated breakpoint count",
              ylim = c(min(c((as.numeric(output2[3:27,4]))-(as.numeric(output2[3:27,3])), as.numeric(output2[3:27,6]))),
                       max(c((as.numeric(output2[3:27,5]))-(as.numeric(output2[3:27,3]))), as.numeric(output2[3:27,6]))),
              las=2
      )
      
      barp
      
      lines(x= barp, y= (as.numeric(output2[3:27,4]))-(as.numeric(output2[3:27,3])), col= "grey", lwd= 2)
      
      points(x= barp, y= (as.numeric(output2[3:27,4]))-(as.numeric(output2[3:27,3])), col= "black", lwd= 2)
      
      lines(x= barp, y= (as.numeric(output2[3:27,5]))-(as.numeric(output2[3:27,3])), col= "grey", lwd= 2)
      
      points(x= barp, y= (as.numeric(output2[3:27,5]))-(as.numeric(output2[3:27,3])), col= "black", lwd= 2)
      
      dev.off()
      
      plots_dir <- paste0("./", sample, "/Output/All_plots/500kb/")
      
      if(!file.exists(plots_dir)){
        dir.create(plots_dir)
      }
      
      #setwd(plots_dir)
      
      png(paste0(plots_dir, sample, "_", fn1, fn2,"_500kb_plot_2.png"),
          width = 600,
          height = 300
      )
      
      if (fn1 == "all_") {
        sub = "the whole genome"
      }
      
      if (fn1 == "chr_") {
        sub = paste0("chromosome ", fn2)
      }
      
      if (fn1 == "min_") {
        sub = paste0("the whole genome except chromosome ", fn2)
      }
      
      barplot(as.numeric(output2[3:27,6]),
              col= output2[3:27,7],
              names.arg = seq(from = 20, to = 500, by = 20),
              cex.names = 0.8,
              main = paste0(sample, " SVs on ", sub),
              xlab = "Distance to the nearest ARBS (kbp)",
              ylab = "Observed - simulated breakpoint count",
              las=2
      )
      
      dev.off()

    }
    
    else {
      print(paste0("No SVs on ", fn1, fn2))
    }
    
  }

