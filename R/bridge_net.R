#' @export
#' @useDynLib bnets, .registration = TRUE
bridge_net <- function(X, prior_scale = 1, chains = 4, models = 1,
                       scale_seq = scale_seq, iter = 2000,
                       adapt_delta = 0.80, max_treedepth = 10){

  cores <- parallel::detectCores() - 2

  if(sum(colSums(is.na(X))) > 0){
    warning("NA values detected and removed", call. = FALSE)
  }

  X <- as.matrix(na.omit(scale(X)))
  N <- length(X[,1])
  K <- ncol(X)
  mod_fit <- list()

  for(i in 1:length(prior_scale)){
    temp <- prior_scale[i]
    if(temp <= 0){
      stop("The scale must be positive. The default is set to 1", call. = FALSE)
    }}
  if(iter > 2000){
    warning("Increasing iterations will make model fitting slower and may not be necessary for convergence!",
            call. = FALSE)
  }
  if(models == 1){
    stan_dat <- list(N = N, K = K, X = X, prior_scale = prior_scale)
    # fit model
    mod_fit   <- rstan::sampling(stanmodels$ridge, data = stan_dat,
                          chains = chains, iter = iter, cores = cores,
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

      stan_dat <- list(N = N, K = K, X = X, prior_scale = temp)

      mod_fit[[i]] <- rstan::sampling(stanmodels$ridge, data = stan_dat, cores = cores,
                               chains = chains, iter = iter, refresh = 0,
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
  list(mod_fit = c(mod_fit), stan_dat = c(stan_dat))
}
