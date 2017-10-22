#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

in_sample_fit <- function(x, X, fit_index, prior_scale = NULL, node, prob){
  X <- na.omit(scale(X))
  bayes_r2 <- list()
  bayes_MSE <- list()
  bayes_RMSE <- list()
  if(length(x) > 1 && is.null(prior_scale)){
  stop("detected > 1 model. must specify prior scale for model to compute posterior predictive")
}
if(sum(node <= 0) > 0){
  stop("node must be positive")
}
if(length(node) > x$stan_dat$K){
  stop("Specified more nodes than in the model")
}
mat_pred <- matrix(nrow = (x$iter * x$chains) / 2, ncol = length(X[,1]))
if(fit_index == "bayes_r2"){
# list to store node_wise y_rep
# rename node argument
node_loop <- node
# matrix to store y_rep
for(i in 1:length(node_loop)){
  node_temp <- node_loop[i]
  X_pred <- data.frame(1, scale(X[,-node_temp]))
  temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                              node = node_temp, prob = 0.50)
  beta <- cbind(temp$posterior_samples_not_BETA[,1],
                temp$posterior_sample_BETA)

for (j in 1:nrow(beta)){
b_temp <- beta[j,]
mat_pred[j, 1:length(X_pred[,node_temp])] <- as.matrix(X_pred) %*% as.numeric(b_temp)
}
e <- 1 * sweep(mat_pred, 2, X[,node_temp])
var_yhat <- apply(mat_pred,1, var)
var_e <- apply(e, 1, var)
r_2 <- var_yhat / (var_yhat + var_e)
bayes_r2[[i]] <- r_2
}
dat_r2 <- data.frame(bayes_r2)
names(bayes_r2) <- paste("node", node, sep = "_")
mlt_bayes_r2 <- reshape2::melt(bayes_r2)
results_r2 <- mlt_bayes_r2 %>%
  group_by(L1) %>%
  summarise(mean = mean(value),
            median = median(value),
            mode = mode(value),
            lb_hdi = hdi(value, prob)[1],
            ub_hdi = hdi(value, prob)[2])
list(bayes_r2 = results_r2, posterior_sample = dat_r2)
}

else if(fit_index == "bayes_MSE"){
  node_loop <- node
  for(i in 1:length(node_loop)){

    node_temp <- node_loop[i]
    X_pred <- data.frame(1, scale(X[,-node_temp]))
    temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                                node = node_temp, prob = 0.50)
    beta <- cbind(temp$posterior_samples_not_BETA[,1],
                  temp$posterior_sample_BETA)

    for (j in 1:nrow(beta)){
      b_temp <- beta[j,]
      mat_pred[j, 1:length(X_pred[,node_temp])] <- as.matrix(X_pred) %*% as.numeric(b_temp)
    }
    temp_mse <- NULL
    for(k in 1:nrow(mat_pred)){
      mse_temp <- mat_pred[k,]
      mse <- mean((mse_temp - scale(X[,node_temp]))^2)
      temp_mse[k] <- mse
      }
    bayes_MSE[[i]] <- temp_mse
  }
    names(bayes_MSE) <- paste("node", node, sep = "_")
    dat_MSE <- data.frame(bayes_MSE)
    mlt_bayes_MSE <- reshape2::melt(bayes_MSE)
    results_MSE <- mlt_bayes_MSE %>%
    group_by(L1) %>%
    summarise(mean = mean(value),
              median = median(value),
              mode = mode(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])
list(bayes_MSE = results_MSE, posterior_samples = dat_MSE)
}else if(fit_index == "bayes_RMSE"){
  node_loop <- node
  for(i in 1:length(node_loop)){

    node_temp <- node_loop[i]
    X_pred <- data.frame(1, scale(X[,-node_temp]))
    temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                                node = node_temp, prob = 0.50)
    beta <- cbind(temp$posterior_samples_not_BETA[,1],
                  temp$posterior_sample_BETA)
    for (j in 1:nrow(beta)){
      b_temp <- beta[j,]
      mat_pred[j, 1:length(X_pred[,node_temp])] <- as.matrix(X_pred) %*% as.numeric(b_temp)
    }
    temp_rmse <- NULL
    for(k in 1:nrow(mat_pred)){
      rmse_temp <- mat_pred[k,]
      rmse <- sqrt(mean((rmse_temp - scale(X[,node_temp]))^2))
      temp_rmse[k] <- rmse
  }
    bayes_RMSE[[i]] <- temp_rmse
  }
  names(bayes_RMSE) <- paste("node", node, sep = "_")
  dat_RMSE <- data.frame(bayes_RMSE)
  mlt_bayes_RMSE <- reshape2::melt(bayes_RMSE)
  results_RMSE <- mlt_bayes_RMSE %>%
    group_by(L1) %>%
    summarise(mean = mean(value),
              median = median(value),
              mode = mode(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])
  list(bayes_RMSE = results_RMSE, posterior_samples = dat_RMSE)
} else if(fit_index == "all"){
  node_loop <- node
  for(i in 1:length(node_loop)){
    node_temp <- node_loop[i]
    X_pred <- data.frame(1, scale(X[,-node_temp]))
    temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                                node = node_temp, prob = 0.50)
    beta <- cbind(temp$posterior_samples_not_BETA[,1],
                  temp$posterior_sample_BETA)
    for (j in 1:nrow(beta)){
      b_temp <- beta[j,]
      mat_pred[j, 1:length(X_pred[,node_temp])] <- as.matrix(X_pred) %*% as.numeric(b_temp)
    }
    temp_mse <- NULL
    temp_rmse <- NULL
    for(k in 1:nrow(mat_pred)){
      mse_temp  <- mat_pred[k,]
      rmse_temp <- mat_pred[k,]
      mse <- mean((mse_temp - scale(X[,node_temp]))^2)
      rmse <- sqrt(mean((rmse_temp - scale(X[,node_temp]))^2))
      temp_mse[k] <- mse
      temp_rmse[k] <- rmse
}
    e <- 1 * sweep(mat_pred, 2, X[,node_temp])
    var_yhat <- apply(mat_pred,1, var)
    var_e <- apply(e, 1, var)
    r_2 <- var_yhat / (var_yhat + var_e)
    bayes_r2[[i]] <- r_2
    bayes_MSE[[i]] <- temp_mse
    bayes_RMSE[[i]] <- temp_rmse
}
  names(bayes_r2) <- paste("node", node, sep = "_")
  names(bayes_MSE) <- paste("node", node, sep = "_")
  names(bayes_RMSE) <- paste("node", node, sep = "_")
  }

  c_all <- rbind.data.frame(bayes_r2, bayes_MSE, bayes_RMSE)
  c_all$fit_index <- rep(c("bayes_r2", "bayes_MSE", "bayes_RMSE"), each = 4000)
  mlt_call <- suppressMessages(reshape2::melt(c_all))

  all_results <- mlt_call %>% group_by(variable, fit_index) %>%
    summarise(mean = mean(value),
              median = median(value),
              mode = mode(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])
  list(summary_all = all_results, bayes_r2 = bayes_r2,
       bayes_MSE = bayes_MSE, bayes_RMSE = bayes_RMSE)
}
