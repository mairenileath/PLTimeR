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
  
  history <-
    autoencoder_model %>%
    keras::fit(train_X, 
               train_X, 
               epochs = 200, 
               batch_size = 256,
               shuffle = TRUE, 
               validation_data = list(test_X, test_X)
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
