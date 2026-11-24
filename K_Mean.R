# Select the continuous features identified during EDA
cluster_vars <- c("Annual_Income_USD", "Age", "Daily_Commute_km", "Environmental_Concern_Level")

#standardize using training set means and standard deviations (to avoid leakage from val data)
#k-means requires euclidean distance metric, so terms must be identical in axes
#income ranges between $20k : $200k, whereas ECL {1,2,3,4,5}
train_means <- sapply(train_data[cluster_vars], mean)
train_sds   <- sapply(train_data[cluster_vars], sd)

#converts train_scaled and val_scaled into N x 4 matrices, where N = # of rows
#4 columns - 1 for each of the cluster_vars converted to z-scores
train_scaled <- scale(train_data[cluster_vars], center = train_means, scale = train_sds)
val_scaled   <- scale(val_data[cluster_vars],   center = train_means, scale = train_sds)

k_values <- 5
auc_by_k <- sapply(k_values, function(k) {
  set.seed(42)
  fit   <- kmeans(train_scaled, centers = k, nstart = 10, iter.max = 25)
  rates <- tapply(train_data$Will_Buy_EV == "1", fit$cluster, mean)
  
  clusters <- apply(val_scaled, 1, function(individual) {
    distances <- colSums((t(fit$centers) - individual)^2)
    which.min(distances)
  })
  
  p <- rates[as.character(clusters)]
  roc_auc_vec(val_data$Will_Buy_EV, as.numeric(p), event_level = "second")
})
