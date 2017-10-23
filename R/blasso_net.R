#' @export
#' @import rstan
#' @import parallel
#' @useDynLib bnets, .registration = TRUE
blasso_net <- function(X, lasso_df = 1, prior_scale = 1, chains = 4, models = 1,
                       iter = 2000, adapt_delta = 0.80, max_treedepth = 10){
  #-arguements:
  #   1) X = matrix or data.frame of variables
  #   2) lasso_df = degrees of freedom for chi-squared dist.
  #   3) lasso_scale = scale of lasso prior
  #      note: smaller produces more shrinkage
  #   4) chains = number of MCMC chains
  #   5) iter = number of samples
  #       note: 2000 is recommended, as higher values increase
  #             fitting time are not necessary for convergence
  #   6) adapt_delta = increase > 0.80 if divergent transitions
  #      after warmup (i.e., burn-in period)
  #   7) max_treedepth = indicates that sampling is terminating
  #      prematurely to avoid excessively long execution time.

  # see here for more about warnings:
  # http://mc-stan.org/misc/warnings.html

  # Stan data
  X <- as.matrix(na.omit(scale(X)))
  N <- length(X[,1])
  K <- ncol(X)
  mod_fit <- list()
  if(lasso_df <= 0){
    stop("The degrees of freedom parameter must be positive. The default is set to 1")
  }
  for(i in 1:length(prior_scale)){
    temp <- prior_scale[i]
  if(temp <= 0){
    stop("The scale parameter must be positive. The default is set to 1")
  }}
  if(iter > 2000){
    warning("Increasing iterations will make model fitting slower and may not be necessary for model convergence!")
  }
  if(models == 1){
  # create Stan data
  stan_dat <- list(N = N, K = K, X = X, lasso_df = lasso_df,
                   lasso_scale = prior_scale)

  # fit model
  mod_fit <- sampling(stanmodels$lasso, data = stan_dat,
                  chains = chains, iter = iter,
                  control = list(adapt_delta = adapt_delta,
                                 max_treedepth = max_treedepth))
}
else if(models > 1){
  if((as.numeric(models) ==  length(prior_scale)) == 0){
    stop("Models must be same length as prior scale")
  }
  total <- length(prior_scale)
  # create progress bar
  pb <- txtProgressBar(min = 1, max = total, style = 3)

  for(i in 1:length(prior_scale)){
    setTxtProgressBar(pb, i)

    temp <- prior_scale[i]

    stan_dat <- list(N = N, K = K, X = X, lasso_df = lasso_df,
                     lasso_scale = temp)

    mod_fit[[i]] <- sampling(stanmodels$lasso, data = stan_dat,
                             chains = chains, iter = iter,
                             control = list(adapt_delta = adapt_delta,
                                            max_treedepth = max_treedepth))
  }
}
temp_names <- rep("prior_scale", length(prior_scale))

mod_names <- paste(temp_names, prior_scale, sep = " ")

if(length(mod_fit) == 1){
  mod_fit <-  list(mod_fit, "")
  names(mod_fit) <- c(mod_names, "")
}

names(mod_fit) <- mod_names
list(mod_fit = mod_fit, stan_dat = stan_dat, iter = iter, chains = chains)
}




