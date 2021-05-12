#https://statslab.eighty20.co.za/posts/autoencoders_keras_r/
#https://github.com/Hua0113/DAE_km/blob/master/autoen_km.py
#https://www.datacamp.com/community/tutorials/keras-r-deep-learning
#https://cran.r-project.org/web/packages/keras/vignettes/guide_keras.html
#https://www.sciencedirect.com/science/article/pii/S0950705119300140
#https://www.datatechnotes.com/2020/02/how-to-build-simple-autoencoder-with-keras-in-r.html
#https://blogs.rstudio.com/ai/posts/2018-01-24-keras-fraud-autoencoder/
#https://www.datacamp.com/community/tutorials/autoencoder-keras-tutorial
#https://blogs.rstudio.com/ai/posts/2018-01-24-keras-fraud-autoencoder/

library(keras)
library(factoextra)
source("functions_subtyping.R")

for (i in 1:100) {
  df_ppcg <- read.csv("PPCG.txt", sep = "\t")
  
  df <- df_ppcg[sample(nrow(df_ppcg)),]
  
  rownames(df) <- df[,1]
  ppcg_samples <- df[,1]
  df <- df[,-1]
  df <- scale(df)
  
  train <- df[1:650,]
  test <- df[651:838,]
  
  train_X <- train[,1:99] %>% as.matrix()
  test_X <- test[,1:99] %>% as.matrix()
  full_X <- df[,1:99] %>% as.matrix()
  
  #Initialise a sequential model
  autoencoder_model <- keras_model_sequential()
  
  autoencoder_model %>%
    layer_dense(units = ncol(train_X), activation = "relu", input_shape = ncol(train_X)) %>%
    layer_dense(units = 30, activation = "relu", name = "bottleneck") %>%
    layer_dense(units = ncol(train_X), activation = "relu")
  
  autoencoder_model %>% compile(
    loss='mean_squared_error',
    optimizer='adam'
  )
  
  summary(autoencoder_model)
  
  #early_stopping <- callback_early_stopping(patience = 5)
  
  history <-
    autoencoder_model %>%
    keras::fit(train_X, 
               train_X, 
               epochs = 200, 
               batch_size = 256,
               shuffle = TRUE, 
               validation_data = list(test_X, test_X)#, 
               #callback = list(early_stopping)
    )
  
  #Visualise training
  #Plots loss plot for training and validation
  plot(history)
  
  #Visualize the embedding
  reconstructed_points <-
    autoencoder_model %>%
    keras::predict_on_batch(x = train_X)
  
  Viz_data <-
    dplyr::bind_rows(
      reconstructed_points %>%
        tibble::as_tibble() %>%
        setNames(names(train_X %>% tibble::as_tibble())) %>%
        dplyr::mutate(data_origin = "reconstructed"),
      train_X %>%
        tibble::as_tibble() %>%
        dplyr::mutate(data_origin = "original")
    )
  
  #Viz_data %>%
  #  ggplot(aes(CT2,kataegis, color = data_origin))+geom_point()
  
  #Evaluate performance
  mse.ae2 <- evaluate(autoencoder_model, train_X, train_X)
  mse.ae2
  
  #Extract bottleneck layer
  intermediate_layer_model <- keras_model(inputs = autoencoder_model$input, outputs = get_layer(autoencoder_model, "bottleneck")$output)
  intermediate_output <- predict(intermediate_layer_model, full_X)
  
  #Cluster
  intermediate_output <- cbind(ppcg_samples, intermediate_output)
  
  #Determine how many cluster based on within sum of squares
  vis_plot <- fviz_nbclust(intermediate_output[,-1], kmeans, method="silhouette") + labs(title="Silhouette - kmeans")
  
  ggsave(paste0("ae_optimal_",i,".png"),plot=vis_plot)
  
  cluster_count = 2
  clust <- kmeans(intermediate_output[,-1], centers = cluster_count, nstart = 10, iter.max = 100)
  
  samples_clustered <- as.data.frame(cbind(ppcg_samples, clust$cluster))
  colnames(samples_clustered) <- c("Sample_ID","Cluster")
  ca_data_a_count <- nrow(samples_clustered[samples_clustered$Cluster == 1,])
  ca_data_b_count <- nrow(samples_clustered[samples_clustered$Cluster == 2,])
  
  print(paste0("A: ",ca_data_a_count))
  print(paste0("B: ",ca_data_b_count))
  
  samples_clustered <- samples_clustered[order(samples_clustered$Sample_ID),]
  
  #Output clusters as files
  write.table(samples_clustered, file = paste0("CA_ae",i,".txt"), sep="\t",quote=F, row.names = FALSE)
}




# 
# 
# run_autoencoder <- function() {
#   df_ppcg <- read.csv("PPCG.txt", sep = "\t")
#   
#   #Remove sample ID column and set the row names to those values.
#   rownames(df_ppcg) <- df_ppcg[,1]
#   ppcg_samples <- df_ppcg[,1]
#   df_ppcg <- df_ppcg[,-1]
#   df_ppcg <- scale(df_ppcg)
#   
#   #Model properties
#   #epochs <- 100
#   #batch_size <- 256
#   #learning_rate <- 0.001
#   
#   #Split test/train data
#   #split_ind <- df_ppcg %>% caret::createDataPartition(p = 0.8, list = FALSE)
#   train <- df_ppcg[1:650,]
#   test <- df_ppcg[651:838,]
#   
#   #Pre-process
#   train_X <- train[,1:99] %>% as.matrix()
#   test_X <- test[,1:99] %>% as.matrix()
#   full_X <- df_ppcg[,1:99] %>% as.matrix()
#   
#   add_noise <- function(val) {
#     print(val)
#     noise_factor <- 0.5
#     val <- val + noise_factor * rnorm(1,mean=0.5,sd=0.5)
#     return(val)
#   }
#   
#   #Implementing DAE
#   #Set values smaller than 0 to 0
#   #Set values greater than 1 to 1
#   x_train_noisy <- train_X
#   x_test_noisy <- test_X
#   
#   # for (i in 1:nrow(x_train_noisy)) {
#   #   for (j in 1:ncol(x_train_noisy)) {
#   #     x_train_noisy[i,j] <- add_noise(x_train_noisy[i,j])
#   #     #x_train_noisy[i,j] <- pmax(0, pmin(add_noise(x_train_noisy[i,j]), 1))
#   #   }
#   # }
#   # for (i in 1:nrow(x_test_noisy)) {
#   #   for (j in 1:ncol(x_test_noisy)) {
#   #     x_test_noisy[i,j] <- add_noise(x_test_noisy[i,j])
#   #     #x_test_noisy[i,j] <- pmax(0, pmin(add_noise(x_test_noisy[i,j]), 1))
#   #   }
#   # }
# 
#   #tanh is the non-linear activation function
#   
#   #Initialise a sequential model
#   autoencoder_model <- keras_model_sequential()
#   
#   autoencoder_model %>%
#     layer_dense(units = ncol(train_X), activation = "tanh", input_shape = ncol(train_X)) %>%
#     layer_dense(units = 35, activation = "tanh", name = "bottleneck") %>%
#     layer_dense(units = ncol(train_X), activation = "tanh")
#   #%>%#layer_dense(units = ncol(train_X))
#   
#   #Compile and train the autoencoder
#   autoencoder_model %>% compile(
#     loss='mean_squared_error',
#     optimizer='adam'
#     #,metrics=c('accuracy')
#   )
#   #accuracy not useful metric for regression problem.
#   
#   summary(autoencoder_model)
#   
#   early_stopping <- callback_early_stopping(patience = 5)
#   
#   history <-
#     autoencoder_model %>%
#     keras::fit(x_train_noisy, 
#                train_X, 
#                epochs = 100, 
#                shuffle = TRUE, 
#                validation_data = list(x_test_noisy, test_X), 
#                callback = list(early_stopping)
#     )
#   
#   #Visualise training
#   #Plots loss plot for training and validation
#   plot(history)
#   
#   #Visualize the embedding
#   reconstructed_points <-
#     autoencoder_model %>%
#     keras::predict_on_batch(x = train_X)
#   
#   Viz_data <-
#     dplyr::bind_rows(
#       reconstructed_points %>%
#         tibble::as_tibble() %>%
#         setNames(names(train_X %>% tibble::as_tibble())) %>%
#         dplyr::mutate(data_origin = "reconstructed"),
#       train_X %>%
#         tibble::as_tibble() %>%
#         dplyr::mutate(data_origin = "original")
#     )
#   
#   Viz_data %>%
#     ggplot(aes(CT2,kataegis, color = data_origin))+geom_point()
#   
#   #Evaluate performance
#   mse.ae2 <- evaluate(autoencoder_model, train_X, train_X)
#   mse.ae2
#   
#   #Extract bottleneck layer
#   intermediate_layer_model <- keras_model(inputs = autoencoder_model$input, outputs = get_layer(autoencoder_model, "bottleneck")$output)
#   intermediate_output <- predict(intermediate_layer_model, full_X)
#   
#   #Cluster
#   #kmeans_output <- kmeans(intermediate_output,2)
#   intermediate_output <- cbind(ppcg_samples, intermediate_output)
#   
#   #Determine how many cluster based on within sum of squares
#   fviz_nbclust(intermediate_output[,-1], kmeans, method="silhouette") + labs(title="Silhouette - kmeans")
#   
#   cluster_count = 2
#   clust <- kmeans(intermediate_output[,-1], centers = cluster_count, nstart = 10, iter.max = 100)
#   
#   samples_clustered <- as.data.frame(cbind(ppcg_samples, clust$cluster))
#   colnames(samples_clustered) <- c("Sample_ID","Cluster")
#   ca_data_a_count <- nrow(samples_clustered[samples_clustered$Cluster == 1,])
#   ca_data_b_count <- nrow(samples_clustered[samples_clustered$Cluster == 2,])
#   
#   print(paste0("A: ",ca_data_a_count))
#   print(paste0("B: ",ca_data_b_count))
#   
#   return(samples_clustered)
# }
# 
# results <- run_autoencoder()
# 
# #Output clusters as files
# write.table(results, file = "CA_ae.txt", sep="\t",quote=F, row.names = FALSE)
# 
# 



#Converts to matrix
#df_mat <- acast(df_distances, A~B, value.var="Distance", mean)
#df_dist <- as.dist(df_mat)
#clusterboot_results <- clusterboot(df_dist, clustermethod=disthclustCBI, method="average", k=3)
#Hierarchical clustering
#hclust_result <- hclust(intermediate_output, method = 'average')
#hier_clust_plot <- ggdendrogram(hclust_result, rotate = FALSE, size = 2)
#hier_clust_plot
#save_plot(paste0("PPCG_Hierarchical_Clustering_Plot.pdf"), plot = hier_clust_plot)
#ggplot(data.frame(PC1 = intermediate_output[,1], PC2 = intermediate_output[,2]), aes(x = PC1, y = PC2)) + geom_point()