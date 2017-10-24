#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

log_likelihood <- function(x){
  # number of fitted models
  models <- length(x)
  # data used to fit models
  X <- x$stan_dat$X
  # number of nodes
  nodes <- 1:ncol(X)
  # posterior samples
  n_samples <- (x$iter * x$chains) / 2
  # prior scale (only numeric; remove letters)
  prior_scale <- as.numeric(gsub("[A-z]",
                 replacement = "", x =  names(x$mod_fit)))

  prior_scale <- as.numeric(na.omit(prior_scale))
   # multidimensional array for log_likelihood (model specific)
  store_log_lik <- array(NA , c(n_samples, nrow(X),
                                length(prior_scale), ncol(X)))
  # double for loop
  for(i in 1:length(prior_scale)){
    prior_temp <- prior_scale[i]
  for(k in 1:length(nodes)){
    node_temp <- nodes[k]
    # temporary beta extraction
    temp <- extract_BETA(x, prior_scale = prior_temp,
                                node = node_temp, prob = 0.50)
    # betas (beta[,1] as intercept)
    beta <- cbind(temp$posterior_samples_not_BETA[,1],
                  temp$posterior_sample_BETA)
    # residual sigma
    sig <- temp$posterior_samples_not_BETA[,2]
    # for matrix operations
    beta <- as.matrix(beta)
    # model matrix (-node_temp)
    X_mat <- data.frame(1, X[, -node_temp])

    # extract log_likelihood
    store_log_lik[,,i, k] = t(sapply(1:n_samples,
                            function(s) dnorm(X[,node_temp],
                            mean = as.matrix(X_mat) %*% beta[s,],
                            sd = sig[s], log = TRUE)))
    }
  }
list(log_lik = store_log_lik, prior_scale = prior_scale, node = nodes)
}

