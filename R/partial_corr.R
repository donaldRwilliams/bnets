#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

partial_corr <- function(x, prior_scale, prob){

 #prior_scale = .01
# x <- mods
  nodes  <- 1:x$stan_dat$K
 temp <- (x$iter * x$chains) / 2
 n_samples <- 1:temp
 list_beta <- list()
  for(i in 1:length(nodes)){
  temp_node <- nodes[i]
  list_beta[[i]]  <-extract_BETA(x, prior_scale = prior_scale, nodes = temp_node, prob = 0.50)$posterior_sample_BETA
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
     # a <-   as.numeric(temp_list[i,])
    #not_i <-  subset(nodes, nodes !=i)
   #<- a
  }
  }

t <- reshape2::melt(par_cor)

test_par_Cor <- t %>%
  group_by(Var1, Var2) %>%
  summarise(mean = mean(value),
  median = median(value),
  mode = mode(value),
  lb_hdi = hdi(value, prob)[1],
  ub_hdi = hdi(value, prob)[2])


mean_par <- matrix(test_par_Cor$mean,  max(nodes))
median_par <- matrix(test_par_Cor$median,  max(nodes))
mode_par <- matrix(test_par_Cor$mode,  max(nodes))
test_par_Cor <- subset(test_par_Cor,  Var1 != Var2)
list(summary = data.frame(test_par_Cor), matrices = list(mean_par = mean_par,
    median_par = median_par, mode_par = mode_par), par_mat = par_cor)

}
