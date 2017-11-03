#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @import data.table
#' @useDynLib bnets, .registration = TRUE

x <- mod_1
prior_scale <-
partial_corr <- function(x, prior_scale, prob){
  nodes  <- 1:x$stan_dat$K

  temp <- (x$iter * x$chains) / 2
  n_samples <- 1:temp
  list_beta <- list()

  pb <- txtProgressBar(min = 0, max = max(nodes), style = 3)

for(i in 1:length(nodes)){
  setTxtProgressBar(pb, i)
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

#t$Var2 <- rep(colnames(x$stan_dat$X), each = max(nodes))
#t1  <- t %>%
 # filter(value != 0)

if(!is.null(colnames(x$stan_dat$X))){
  t1 <- t %>% mutate(Var1 = rep(colnames(x$stan_dat$X), length(Var1) / max(nodes)))
  t1$Var2 <- rep(colnames(x$stan_dat$X), each = max(nodes))

} else {
  t1$Var1 = rep(1:length(nodes), length(t$value) / max(nodes))
  t1$Var2 = rep(1:length(nodes), each = max(nodes))
}

t1  <- t1 %>%
  filter(value != 0)

temp_results <- t1 %>%
  group_by(Var1, Var2) %>%
  summarise(mean = mean(value),
  median = median(value),
  mode = mode(value),
  post_sd = sd(value),
  psuedo_z = mean(value) / sd(value),
  ev.ratio = sum(value > 0) / sum(value < 0),
  lb_hdi = hdi(value, prob)[1],
  ub_hdi = hdi(value, prob)[2],
  lb_eq  = quantile(value, (1- prob)/ 2),
  ub_eq  = quantile(value, 1 - ((1- prob)/ 2)))

summary_results <- temp_results[!duplicated(apply(temp_results,1,function(x)
                               paste(sort(x),collapse=''))),]

t2 <- t %>%
   filter(value != 0)
mat_results <- t2 %>%
  group_by(Var1, Var2) %>%
  summarise(mean = mean(value),
            median = median(value),
            mode = mode(value))

mean_par <- matrix(mat_results$mean,  max(nodes))
median_par <- matrix(mat_results$median,  max(nodes))
mode_par <- matrix(mat_results$mode,  max(nodes))
colnames(mean_par) <- colnames(x$stan_dat$X)
colnames(median_par) <- colnames(x$stan_dat$X)
colnames(mode_par) <- colnames(x$stan_dat$X)

summary_results  <- subset(summary_results,  Var1 != Var2)

the_list <- list(summary = data.frame(summary_results), matrices = list(mean_par = mean_par,
     median_par = median_par, mode_par = mode_par), par_mat = par_cor, df_post = t,
     stan_dat = x$stan_dat$X)

class(the_list) <- append(class(the_list),"partial_corr")
return(the_list)
}
