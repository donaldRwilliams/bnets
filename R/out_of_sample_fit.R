#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

out_of_sample_fit <- function(x, fit_index){

models <- length(x)
prior_scale <- as.numeric(gsub("[A-z]",replacement = "", x =  names(x$mod_fit)))
X <- x$stan_dat$X
n_samples <- (x$iter * x$chains) / 2
store_ll <- array(NA , c(n_samples, nrow(X), length(prior_scale), length(node)))
for(i in 1:length(prior_scale)){
  prior_temp <- prior_scale[i]
  for(k in 1:length(node)){

      node_temp <- node[k]
      temp <- bnets::extract_BETA(x, prior_scale = prior_temp,
                                  node = node_temp, prob = 0.50)
      beta <- cbind(temp$posterior_samples_not_BETA[,1],
                      temp$posterior_sample_BETA)
      sig <- temp$posterior_samples_not_BETA[,2]
      beta <- as.matrix(beta)

      X_mat <- data.frame(1, X[, -node_temp])

    store_ll[,,i, k] = t(sapply(1:n_samples,
                           function(s) dnorm(X[,node_temp],mean =
                               as.matrix(X_mat) %*% beta[s,],
                               sd = sig[s], log = TRUE)))
  }
}
if (fit_index == "loo"){
loo_list <- list()
for(i in 1:length(prior_scale)){
  temp <- as.matrix(as.data.frame(store_ll[,,i,]))
  loo_list[[i]] <- loo::loo(temp)
}

#grid_comp <-  t(combn(4, 2))
l <- list()
for(i in 1:4){
l[[i]] <-cbind(loo_list[[i]])[1:6]
}
mlt <- reshape2::melt(l) %>% group_by(L2, L1)
t <- reshape2::dcast(mlt, L1 ~ L2)[,-1]
#t <- t[,-1]
#temp <- data.frame(prior_scale, t)
colnames(t) <- row.names(cbind(loo_list[[2]]))[1:6]

temp <- data.frame(prior_scale, t)
#temp
done_deal <- temp %>% arrange(desc(elpd_loo))
list(done_deal = done_deal, log_lik = store_ll)

}else if(fit_index == "waic"){
  waic_list <- list()
  for(i in 1:length(prior_scale)){
    temp <- as.matrix(as.data.frame(store_ll[,,i,]))
    waic_list[[i]] <- loo::waic(temp)
  }

  #grid_comp <-  t(combn(4, 2))
  l <- list()
  for(i in 1:4){
    l[[i]] <-cbind(waic_list[[i]])[1:6]
  }
  mlt <- reshape2::melt(l) %>% group_by(L2, L1)
  t <- reshape2::dcast(mlt, L1 ~ L2)[,-1]
  #t <- t[,-1]
  #temp <- data.frame(prior_scale, t)
  colnames(t) <- row.names(cbind(waic_list[[2]]))[1:6]

  temp <- data.frame(prior_scale, t)
  #temp
  done_deal <- temp %>% arrange(desc(elpd_waic))
  list(done_deal = done_deal, log_lik = store_ll)
}}



