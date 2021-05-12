#https://www.analyticsvidhya.com/blog/2017/01/t-sne-implementation-r-python/

library(Rtsne)
library(mclust)
source("functions_subtyping.R")

run_tsne <- function(data, prop_perp) {
  prop_dims <- 2
  prop_theta <- 0.5 #higher for less accuracy (speed/accuracy trade-off)
  prop_pca <- TRUE
  prop_max_iter <- 5000
  prop_normalize <- TRUE
  
  tsne_model <- Rtsne(data, dims=prop_dims, perplexity=prop_perp, verbose=TRUE, theta=prop_theta, pca = prop_pca, max_iter=prop_max_iter, normalize = prop_normalize)
  d_tsne = as.data.frame(tsne_model$Y)
  return(d_tsne)
}

produce_tsne_plot <- function(data, title, x=NULL, y=NULL, sl=NULL, inter=NULL) {
  g <- ggplot(data, aes(x=V1, y=V2)) + 
    geom_point(size=0.25) +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")
    )
    
  if (!is.null(x)) {
    g <- g + geom_vline(xintercept = x, colour="red")
  } else if (!is.null(y)) {
    g <- g + geom_hline(yintercept = y, colour="red")
  } else if ((!is.null(sl))|(!is.null(inter))) {
    g <- g + geom_abline(slope = sl, intercept = inter, colour="red")
  }
  return(g)
}

get_samples_from_tsne <- function(data, flip_cluster_labels, x=NULL, y=NULL, sl=NULL, inter=NULL) {
  
  data <- cbind(df_samples,data)
  data[,4] <- NA
  colnames(data) <- c("Sample_ID", "X", "Y", "Cluster")
  
  if (flip_cluster_labels) {
    cluster_label_a <- 2
    cluster_label_b <- 1
  } else {
    cluster_label_a <- 1
    cluster_label_b <- 2
  }
  
  if (!is.null(x)) {
    data[data$X > x, "Cluster"] <- cluster_label_a
    data[data$X < x, "Cluster"] <- cluster_label_b
  } else if (!is.null(y)) {
    data[data$Y > y, "Cluster"] <- cluster_label_a
    data[data$Y < y, "Cluster"] <- cluster_label_b
  } else if ((!is.null(sl))|(!is.null(inter))) {
    data[data$Y > ((sl*data$X) + inter), "Cluster"] <- cluster_label_a
    data[data$Y < ((sl*data$X) + inter), "Cluster"] <- cluster_label_b
  }
  
  samples_clustered <- subset(data, select = c("Sample_ID", "Cluster"))
  
  return(samples_clustered)
}

find_perplexity_tsne <- function(data) {
  
  d_tsne_p2 = run_tsne(data,2)
  d_tsne_p5 = run_tsne(data,5)
  d_tsne_p30 = run_tsne(data,30)
  d_tsne_p50 = run_tsne(data,50)
  d_tsne_p100 = run_tsne(data,100)
  
  p_p2 <- produce_tsne_plot(d_tsne_p2, "Perplexity = 2")
  p_p5 <- produce_tsne_plot(d_tsne_p5, "Perplexity = 5")
  p_p30 <- produce_tsne_plot(d_tsne_p30, "Perplexity = 30")
  p_p50 <- produce_tsne_plot(d_tsne_p50, "Perplexity = 50")
  p_p100 <- produce_tsne_plot(d_tsne_p100, "Perplexity = 100")
  
  perplexity_options <- grid.arrange(p_p2, p_p5, p_p30, p_p50, p_p100, ncol=5)
  
  #ggsave(paste0("tsne_perplexity_plots.png"), plot=perplexity_options)
}

print_cluster_counts <- function(data, title) {
  c1 <- nrow(data[data$Cluster == 1,])
  c2 <- nrow(data[data$Cluster == 2,])
  print(paste(title, "C1", c1))
  print(paste(title, "C2", c2))
}

#data load and preparation
df_feat <- read.csv("PPCG.txt", sep = "\t")

#Remove sample ID column and set the row names to those values.
df_samples <- df_feat[,1]
rownames(df_feat) <- df_feat[,1]
df_feat <- df_feat[,-1]

find_perplexity_tsne(df_feat)
prop_perplexity <- 100

d_tsne_1 = run_tsne(df_feat,prop_perplexity)
d_tsne_2 = run_tsne(df_feat,prop_perplexity)
d_tsne_3 = run_tsne(df_feat,prop_perplexity)
d_tsne_4 = run_tsne(df_feat,prop_perplexity)
d_tsne_5 = run_tsne(df_feat,prop_perplexity)
d_tsne_6 = run_tsne(df_feat,prop_perplexity)
d_tsne_7 = run_tsne(df_feat,prop_perplexity)
d_tsne_8 = run_tsne(df_feat,prop_perplexity)
d_tsne_9 = run_tsne(df_feat,prop_perplexity)
d_tsne_10 = run_tsne(df_feat,prop_perplexity)

# write.table(d_tsne_1, file = "tsne_coord1.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_2, file = "tsne_coord2.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_3, file = "tsne_coord3.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_4, file = "tsne_coord4.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_5, file = "tsne_coord5.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_6, file = "tsne_coord6.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_7, file = "tsne_coord7.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_8, file = "tsne_coord8.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_9, file = "tsne_coord9.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_tsne_10, file = "tsne_coord10.txt", sep="\t",quote=F, row.names=FALSE)
# 
# d_tsne_1 <- read.csv("tsne_coord1.txt", sep = "\t")
# d_tsne_2 <- read.csv("tsne_coord2.txt", sep = "\t")
# d_tsne_3 <- read.csv("tsne_coord3.txt", sep = "\t")
# d_tsne_4 <- read.csv("tsne_coord4.txt", sep = "\t")
# d_tsne_5 <- read.csv("tsne_coord5.txt", sep = "\t")
# d_tsne_6 <- read.csv("tsne_coord6.txt", sep = "\t")
# d_tsne_7 <- read.csv("tsne_coord7.txt", sep = "\t")
# d_tsne_8 <- read.csv("tsne_coord8.txt", sep = "\t")
# d_tsne_9 <- read.csv("tsne_coord9.txt", sep = "\t")
# d_tsne_10 <- read.csv("tsne_coord10.txt", sep = "\t")

p_1 <- produce_tsne_plot(d_tsne_1, "1")
p_2 <- produce_tsne_plot(d_tsne_2, "2")
p_3 <- produce_tsne_plot(d_tsne_3, "3")
p_4 <- produce_tsne_plot(d_tsne_4, "4")
p_5 <- produce_tsne_plot(d_tsne_5, "5")
p_6 <- produce_tsne_plot(d_tsne_6, "6")
p_7 <- produce_tsne_plot(d_tsne_7, "7")
p_8 <- produce_tsne_plot(d_tsne_8, "8")
p_9 <- produce_tsne_plot(d_tsne_9, "9")
p_10 <- produce_tsne_plot(d_tsne_10, "10")
com_plot <- grid.arrange(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, ncol=5, nrow=2)
com_plot

x1 <- NULL
y1 <- NULL
s1 <- 1
i1 <- 0

x2 <- NULL
y2 <- NULL
s2 <- 1
i2 <- 0

x3 <- 2
y3 <- NULL
s3 <- NULL
i3 <- NULL

x4 <- NULL
y4 <- NULL
s4 <- 1
i4 <- 0

x5 <- NULL
y5 <- NULL
s5 <- 1
i5 <- 0

x6 <- NULL
y6 <- NULL
s6 <- 0.3
i6 <- 2

x7 <- NULL
y7 <- NULL
s7 <- 1
i7 <- 0

x8 <- NULL
y8 <- NULL
s8 <- 0.3
i8 <- 2

x9 <- NULL
y9 <- NULL
s9 <- 1
i9 <- 0

x10 <- NULL
y10 <- NULL
s10 <- 1
i10 <- 0

#x, y, slope, intercept
p_1 <- produce_tsne_plot(d_tsne_1, "1", x1, y1, s1, i1)
p_2 <- produce_tsne_plot(d_tsne_2, "2", x2, y2, s2, i2)
p_3 <- produce_tsne_plot(d_tsne_3, "3", x3, y3, s3, i3)
p_4 <- produce_tsne_plot(d_tsne_4, "4", x4, y4, s4, i4)
p_5 <- produce_tsne_plot(d_tsne_5, "5", x5, y5, s5, i5)
p_6 <- produce_tsne_plot(d_tsne_6, "6", x6, y6, s6, i6)
p_7 <- produce_tsne_plot(d_tsne_7, "7", x7, y7, s7, i7)
p_8 <- produce_tsne_plot(d_tsne_8, "8", x8, y8, s8, i8)
p_9 <- produce_tsne_plot(d_tsne_9, "9", x9, y9, s9, i9)
p_10 <- produce_tsne_plot(d_tsne_10, "10", x10, y10, s10, i10)
com_plot <- grid.arrange(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, ncol=5, nrow=2)
com_plot
#ggsave("t-sne_map.png",plot=com_plot)

d_1 <- get_samples_from_tsne(d_tsne_1, TRUE, x1, y1, s1, i1)
d_2 <- get_samples_from_tsne(d_tsne_2, TRUE, x2, y2, s2, i2)
d_3 <- get_samples_from_tsne(d_tsne_3, TRUE, x3, y3, s3, i3)
d_4 <- get_samples_from_tsne(d_tsne_4, TRUE, x4, y4, s4, i4)
d_5 <- get_samples_from_tsne(d_tsne_5, TRUE, x5, y5, s5, i5)
d_6 <- get_samples_from_tsne(d_tsne_6, TRUE, x6, y6, s6, i6)
d_7 <- get_samples_from_tsne(d_tsne_7, TRUE, x7, y7, s7, i7)
d_8 <- get_samples_from_tsne(d_tsne_8, TRUE, x8, y8, s8, i8)
d_9 <- get_samples_from_tsne(d_tsne_9, TRUE, x9, y9, s9, i9)
d_10 <- get_samples_from_tsne(d_tsne_10, TRUE, x10, y10, s10, i10)

print_cluster_counts(d_1, "1")
print_cluster_counts(d_2, "2")
print_cluster_counts(d_3, "3")
print_cluster_counts(d_4, "4")
print_cluster_counts(d_5, "5")
print_cluster_counts(d_6, "6")
print_cluster_counts(d_7, "7")
print_cluster_counts(d_8, "8")
print_cluster_counts(d_9, "9")
print_cluster_counts(d_10, "10")

c_1 <- d_1$Cluster
c_2 <- d_2$Cluster
c_3 <- d_3$Cluster
c_4 <- d_4$Cluster
c_5 <- d_5$Cluster
c_6 <- d_6$Cluster
c_7 <- d_7$Cluster
c_8 <- d_8$Cluster
c_9 <- d_9$Cluster
c_10 <- d_10$Cluster

options <- list(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10)

for (i in 1:length(options)) {
  for (j in 1:length(options)) {
    a <- options[[i]]
    b <- options[[j]]
    value <- adjustedRandIndex(a,b)
    print(paste0("C",i," - C",j,": ",value))
  }
}

#Output t-SNE cluster file.
# write.table(d_1, file = "CA_tsne1.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_2, file = "CA_tsne2.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_3, file = "CA_tsne3.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_4, file = "CA_tsne4.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_5, file = "CA_tsne5.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_6, file = "CA_tsne6.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_7, file = "CA_tsne7.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_8, file = "CA_tsne8.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_9, file = "CA_tsne9.txt", sep="\t",quote=F, row.names=FALSE)
# write.table(d_10, file = "CA_tsne10.txt", sep="\t",quote=F, row.names=FALSE)
# 
# 
# write.table(d_1, file = "CA_tsne.txt", sep="\t",quote=F, row.names=FALSE)

