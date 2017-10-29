#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @useDynLib bnets, .registration = TRUE

#X <- bfi[1:100, 1:10]

posterior_predict <- function(x, X, prior_scale = NULL, nsims){
      K <- x$stan_dat$K
      X <- na.omit(X)
      if(length(x) > 1 && is.null(prior_scale)){
             stop("detected > 1 model. must specify prior scale for model to compute posterior predictive")
          }
            if(sum(nodes <= 0) > 0){
              stop("node must be positive")
            }
            if(length(nodes) > x$stan_dat$K){
              stop("Specified more nodes than in the model")
            }
      # list to store node_wise y_rep
      y_rep <- list()
      # rename node argument
      node_loop <- nodes
      # matrix to store y_rep
      mat <- matrix(nrow = (x$iter * x$chains) / 2,
                    ncol = x$stan_dat$K)

      pb <- txtProgressBar(min = 0, max = max(node_loop), style = 3)

      for(i in 1:length(node_loop)){
              setTxtProgressBar(pb, i)
              node_temp <- node_loop[i]
              temp <- bnets::extract_BETA(x, prior_scale = prior_scale,
                                          node = node_temp, prob = 0.50)
              beta <- cbind(temp$posterior_samples_not_BETA[,1],
                            temp$posterior_sample_BETA)

              # sigma on raw scale
              sig <- temp$posterior_samples_not_BETA[,2] * sd(X[,1])

              # backtransform to raw scale
      for(j in 1:nrow(beta)){
              t <- beta[j,]
              mat[j, 1:K] <- scaleBack.lm(X[,-node_temp], X[,node_temp], as.numeric(t))
              }
          # model matrix
          X_yrp <- data.frame(1, X[,-node_temp])
          y_rep[[i]] = t(sapply(1:nsims, function(s) rnorm(length(X_yrp[,1]),
                                                     as.matrix(X_yrp) %*% mat[s,], sig[s])))
          }
#names(y_rep) <- paste("node", nodes, sep = "_")
names(y_rep) <- colnames(X)
list(y_rep = y_rep, nsims = nsims, data = X)
}

#length(temp$posterior_samples_not_BETA$y_1_sigma)







