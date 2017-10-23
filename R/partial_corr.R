#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

partial_corr <- function(x, prior_scale, prob){
  nodes  <- 1:x$stan_dat$K
  temp <- (x$iter * x$chains) / 2
  n_samples <- 1:temp
  list_beta <- list()
for(i in 1:length(nodes)){
  temp_node <- nodes[i]
  list_beta[[i]]  <- extract_BETA(x, prior_scale = prior_scale,
                                  nodes = temp_node, prob = 0.50)$posterior_sample_BETA
  }

  names(list_beta) <- paste("y", nodes, sep = "_")
  mat <- matrix(0, max(nodes), max(nodes))
  par_cor <- list()
  mat_check <- list()

for(j in 1:length(n_samples)){
  temp_sample <- n_samples[j]
  for(i in 1:length(list_beta)){
    not_i <-  subset(nodes, nodes !=i)
    mat[i, not_i] <- as.numeric(list_beta[[i]][temp_sample,])
    mat_check[[j]] <- mat
    par_cor[[j]]  <- parcor::Beta2parcor(mat)
    }
  }

t <- reshape2::melt(par_cor)

temp_results <- t %>%
  group_by(Var1, Var2) %>%
  summarise(mean = mean(value),
  median = median(value),
  mode = mode(value),
  lb_hdi = hdi(value, prob)[1],
  ub_hdi = hdi(value, prob)[2],
  lb_eq  = quantile(value, (1- prob)/ 2),
  ub_eq  = quantile(value, 1 - ((1- prob)/ 2)))

summary_results <- temp_results[!duplicated(apply(temp_results,1,function(x)
                               paste(sort(x),collapse=''))),]



mean_par <- matrix(temp_results$mean,  max(nodes))
median_par <- matrix(temp_results$median,  max(nodes))
mode_par <- matrix(temp_results$mode,  max(nodes))
summary_results  <- subset(summary_results,  Var1 != Var2)

list(summary = data.frame(summary_results), matrices = list(mean_par = mean_par,
    median_par = median_par, mode_par = mode_par), par_mat = par_cor)

}
