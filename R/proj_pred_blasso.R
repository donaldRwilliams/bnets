#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

proj_pred_blasso <- function(x, prior_scale){

  nodes <- 1:x$stan_dat$K
  beta_list <- list()
  search_path <- list()
  list_res <- list()
  d <- max(nodes) - 1

  for(i in 1:length(nodes)){
    b_temp <- extract_BETA(x, nodes = nodes[i],
                           prior_scale = prior_scale, prob = .5)$posterior_sample_BETA

    int_temp <- extract_BETA(x, nodes = nodes[i],
                             prior_scale = prior_scale, prob = .5)$posterior_samples_not_BETA[,1]

    sigma_temp <- extract_BETA(x, nodes = nodes[i],
                               prior_scale = prior_scale, prob = .5)$posterior_samples_not_BETA[,2]

    beta_list[[i]] <- cbind(int_temp, b_temp, sigma_temp)
  }

  names(beta_list) <- colnames(x$stan_dat$X)

  for(i in 1:length(beta_list)){
    temp_dat <- rbind(beta_list[[i]]$int_temp, t(beta_list[[i]] %>%
                                                   select(contains("x"))))

    search_path[[i]] <- lm_fprojsel(temp_dat, beta_list[[i]]$sigma_temp^2,
                                    cbind(rep(1, x$stan_dat$N) , x$stan_dat$X[,-i]))

  }

  mse_mat <- matrix(NA, max(nodes), max(nodes))
  mlpd_mat <- matrix(NA, max(nodes), max(nodes))

  for(i in 1:length(search_path)){
    search_temp <- search_path[[i]]
    temp_dat <- rbind(beta_list[[i]]$int_temp, t(beta_list[[i]] %>% select(contains("x"))))

    for (k in 1:(d+1)) {
      # projected parameters
      submodel <- lm_proj(temp_dat, beta_list[[i]]$sigma_temp^2,  cbind(rep(1, x$stan_dat$N),
                                                                        x$stan_dat$X[,-i]),  search_temp$chosen[1:k])
      wp <- submodel$w
      sigma2p <- submodel$sigma2

      # mean squared error
      ypred <- rowMeans(cbind(rep(1, x$stan_dat$N),
                              x$stan_dat$X[,-i]) %*% wp)

      mse_mat[k,i] <- mean((x$stan_dat$X[,i]-ypred)^2)
      # mean log predictive density

      pd <- dnorm(x$stan_dat$X[,i], cbind(rep(1, x$stan_dat$N),
                                          x$stan_dat$X[,-i]) %*% wp, sqrt(sigma2p))
      mlpd_mat[k,i] <- mean(log(rowMeans(pd)))


    }
  }

    for(k in 1:ncol(mse_mat)) {
      search_temp <- search_path[[k]]
      c_name_temp <- colnames(x$stan_dat$X[,search_temp$chosen])
      c_name <- c("int", colnames(x$stan_dat$X[,-k]))

      list_res[[k]] <- list(spath = search_temp$chosen,
                            col_names = c_name[search_temp$chosen],
                            kl_dist = search_temp$kl, mse = mse_mat[,k], mlpd= mlpd_mat[,k])
    }

  names(list_res) <- colnames(x$stan_dat$X)
  list(search_results = list_res, beta_list = beta_list)
}


lm_fprojsel <- function(w, sigma2, x) {

  # forward variable selection using the projection

  d = dim(x)[2]
  chosen <- 1 # chosen variables, start from the model with the intercept only
  notchosen <- setdiff(1:d, chosen)

  # start from the model having only the intercept term
  kl <- rep(0,d)
  kl[1] <- lm_proj(w,sigma2,x,1)$kl

  # start adding variables one at a time
  for (k in 2:d) {

    nleft <- length(notchosen)
    val <- rep(0, nleft)

    for (i in 1:nleft) {
      ind <- sort( c(chosen, notchosen[i]) )
      proj <- lm_proj(w,sigma2,x,ind)
      val[i] <- proj$kl
    }

    # find the variable that minimizes the kl
    imin <- which.min(val)
    chosen <- c(chosen, notchosen[imin])
    notchosen <- setdiff(1:d, chosen)

    kl[k] <- val[imin]
  }
  return(list(chosen=chosen, kl=kl))
}

lm_proj <- function(w,sigma2,x,indproj) {

  # assume the intercept term is stacked into w, and x contains
  # a corresponding vector of ones. returns the projected samples
  # and estimated kl-divergence.

  # pick the columns of x that form the projection subspace
  n <- dim(x)[1]
  xp <- x[,indproj]

  # solve the projection equations
  fit <- x %*% w # fit of the full model
  wp <- solve(t(xp) %*% xp, t(xp) %*% fit)
  sigma2p <- sigma2 + colMeans((fit - xp %*% wp)^2)

  # this is the estimated kl-divergence between the full and projected model
  kl <- mean(0.5*log(sigma2p/sigma2))
  # reshape wp so that it has same dimensionality as x, and zeros for
  # those variables that are not included in the projected model
  d <- dim(w)[1]
  S <- dim(w)[2]
  wptemp <- matrix(0, d, S)
  wptemp[indproj,] <- wp
  wp <- wptemp

  return(list(w=wp, sigma2=sigma2p, kl=kl))
}
