#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @useDynLib bnets, .registration = TRUE

in_sample_fit <- function(x, X, fit_index, prior_scale = NULL, node){
  X <- na.omit(scale(X))
  bayey_r2 <- list()
  bayes_MSE <- list()
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


temp <- bnets::extract_BETA(x, prior_scale = .1,
                            node = 1, prob = 0.50)
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
bayey_r2[[i]] <- r_2

}
names(bayey_r2) <- paste("node", node, sep = "_")
list(bayey_r2 = bayey_r2)
} else if(fit_index == "bayes_MSE"){
  node_loop <- node
  for(i in 1:length(node_loop)){

    node_temp <- node_loop[i]
    X_pred <- data.frame(1, scale(X[,-node_temp]))
    temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                                node = node_temp, prob = 0.50)
    beta <- cbind(temp$posterior_samples_not_BETA[,1],
                  temp$posterior_sample_BETA)


    #temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
     #                           node = node_temp, prob = 0.50)

    #beta <- cbind(temp$posterior_samples_not_BETA[,1],
     #             temp$posterior_sample_BETA)
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
  list(bayes_MSE = bayes_MSE)
}}



