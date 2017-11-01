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
  list_results <- list()
  mlpd <- list()
  mse <- list()
  d <- max(nodes) - 1

  for(i in 1:length(nodes)){
    b_temp <- extract_BETA(mod_lasso, nodes = nodes[i],
                           prior_scale = prior_scale, prob = .5)$posterior_sample_BETA

    int_temp <- extract_BETA(mod_lasso, nodes = nodes[i],
                             prior_scale = prior_scale, prob = .5)$posterior_samples_not_BETA[,1]

    sigma_temp <- extract_BETA(mod_lasso, nodes = nodes[i],
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
  mse_sd  <- matrix(NA, max(nodes), max(nodes))
  mlpd_mat <- matrix(NA, max(nodes), max(nodes))
  store_log_lik <- array(NA , c(n_samples, nrow(x$stan_dat$X),
                                length(search_path), max(nodes)))

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
                            kl_dist = search_temp$kl,  mse = mse_mat[,k], mlpd= mlpd_mat[,k])
    }

  names(list_res) <- colnames(x$stan_dat$X)
  list(search_results = list_res, beta_list = beta_list)
}

