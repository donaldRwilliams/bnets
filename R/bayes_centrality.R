#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE


bayes_centrality <- function(x, rule, scale, point_est_cent,
                             point_est_edge, prob_cent, prob_edge, cutoff){
  if(!is(x, "partial_corr")){
    stop("x is not of class partial_corr")
  }
temp <-  edge_decision_rule(x, rule,  point_est = point_est_edge, prob = prob_edge, cut_off)
m_temp <- as.numeric(temp > 0)
m_temp2 <- matrix(m_temp, nrow(temp), nrow(temp))

p_ <- x$par_mat
cent_list <- list()
pb <- txtProgressBar(min = 0, max = length(p_), style = 3)

if(scale == "raw" ){
for(i in 1:length(p_)){
  setTxtProgressBar(pb, i)
 cent_list[[i]] <-  centralityTable(p_[[i]], standardized = FALSE)[,3:5]
  }
}else if(scale == "std"){
  for(i in 1:length(p_)){
    setTxtProgressBar(pb, i)
    cent_list[[i]] <-  centralityTable(p_[[i]], standardized = TRUE)[,3:5]
  }
}
t_mlt <- suppressMessages(reshape2::melt(between_list))
t_mlt$node <- colnames(x$stan_dat)

if(point_est_cent == "mean"){

t_mlt <- t_mlt %>%
  group_by(node, measure) %>%
  summarise(mean_cent = mean(value),
  low_hdi = hdi(value, prob_cent)[1],
  up_hdi = hdi(value, prob_cent)[2])

}else if(point_est_cent == "mode"){
  t_mlt <- t_mlt %>%
    group_by(node, measure) %>%
    summarise(mode_cent = mean(value),
              low_hdi = hdi(value, prob_cent)[1],
              up_hdi = hdi(value, prob_cent)[2])

}else if(point_est_cent == "median"){
  t_mlt <- t_mlt %>%
    group_by(node, measure) %>%
    summarise(mode_cent = mean(value),
              low_hdi = hdi(value, prob_cent)[1],
              up_hdi = hdi(value, prob_cent)[2])
}
list(centrality_indices = t_mlt)
}
