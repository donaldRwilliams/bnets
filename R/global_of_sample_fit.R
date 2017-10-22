#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

global_out_of_sample <- function(x, fit_index){
  prior_scale = x$prior_scale
  if (fit_index == "loo"){
    loo_list <- list()
    for(i in 1:length(prior_scale)){
      temp <- as.matrix(as.data.frame(x$log_lik[,,i,]))
      loo_list[[i]] <- loo::loo(temp)
}
names(loo_list) <- x$prior_scale
loo_temp <- list()
  for(i in 1:length(loo_list)){
    loo_temp[[i]] <-cbind(loo_list[[i]])[1:6]
    }
  mlt <- reshape2::melt(loo_temp) %>% group_by(L2, L1)
  df_temp <- reshape2::dcast(mlt, L1 ~ L2)[,-1]
  colnames(df_temp) <- row.names(cbind(loo_list[[2]]))[1:6]
  temp1 <- data.frame(prior_scale, df_temp)
  temp2 <- temp1 %>%
    arrange(desc(elpd_loo))

results <- data.frame(prior_scale = temp2$prior_scale,
                      elpd = temp2$elpd_loo,
                      elpd_se = temp2$se_elpd_loo,
                      looic = temp2$looic,
                      looic_se = temp2$se_looic,
                      p_loo = temp2$p_loo,
                      p_loo_se = temp2$se_p_loo)
list(results = results, loo_list = loo_list)
}
else if(fit_index == "waic"){
  waic_list <- list()
  for(i in 1:length(prior_scale)){
    temp <- as.matrix(as.data.frame(x$log_lik[,,i,]))
    waic_list[[i]] <- loo::waic(temp)
  }
names(waic_list) <- prior_scale
  waic_temp <- list()
  for(i in 1:length(waic_list)){
    waic_temp[[i]] <-cbind(waic_list[[i]])[1:6]
  }
  mlt <- reshape2::melt(waic_temp) %>% group_by(L2, L1)
  df_temp <- reshape2::dcast(mlt, L1 ~ L2)[,-1]
  colnames(df_temp) <- row.names(cbind(waic_list[[2]]))[1:6]
  temp1 <- data.frame(prior_scale, df_temp)
  temp2 <- temp1 %>%
    arrange(desc(elpd_waic))

  results <- data.frame(prior_scale = temp2$prior_scale,
                        elpd_waic = temp2$elpd_waic,
                        elpd_se = temp2$se_elpd_waic,
                        waic = temp2$waic,
                        waic_se = temp2$se_waic,
                        p_waic = temp2$p_waic,
                        p_waic_se = temp2$se_p_waic)

  list(results = results, waic_list = waic_list)
}}
