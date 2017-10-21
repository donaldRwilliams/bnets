#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @useDynLib bnets, .registration = TRUE


posterior_predict_net <- function(x, X, prior_scale = NULL, node, nsims){
      X <- na.omit(X)
        if(length(x) > 1 && is.null(prior_scale)){
             stop("detected > 1 model. must specify prior scale for model to compute posterior predictive")
          }
            if(sum(node <= 0) > 0){
              stop("node must be positive")
            }
            if(length(node) > x$stan_dat$K){
              stop("Specified more nodes than in the model")
            }
      # list to store node_wise y_rep
      y_rep <- list()
      # rename node argument
      node_loop <- node
      # matrix to store y_rep
      mat <- matrix(nrow = (x$iter * x$chains) / 2,
                    ncol = x$stan_dat$K)

      for(i in 1:length(node_loop)){
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
              mat[j, 1: K] <- scaleBack.lm(X[-node_temp], X[,node_temp], as.numeric(t))
              }
          # model matrix
          X_yrp <- data.frame(1, X[,-node_temp])
          y_rep[[i]] = t(sapply(1:nsims, function(s) rnorm(length(X_yrp[,1]),
                                                     as.matrix(X_yrp) %*% mat[s,], sig[s])))
          }
names(y_rep) <- paste("node", node, sep = "_")
list(y_rep = y_rep)
}
