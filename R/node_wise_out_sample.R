#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

node_wise_out_of_sample <- function(x, prior_scale, nodes){

  log_lik <- x$log_lik
  models <- length(x$prior_scale)
  mod_scales <- x$prior_scale
if(models < length(prior_scale)){
    stop("More prior scales than models")
}
  for(i in 1:length(prior_scale)){
  temp <- prior_scale[i]
  if(temp <= 0){
    stop("The scale must be positive. The default is set to 1", call. = FALSE)
  }}
  if(sum(is.na(match(prior_scale, mod_scales))) > 0){
    stop("User defined prior scales do not match priors used in models")

    }

  if(sum(!is.na(match(prior_scale, mod_scales))) < 2){
    stop("Must specificy atleast two prior scales")
  }

  for(i in 1:length(nodes)){
    temp <- nodes[i]
    if(temp > max(x$node)){
      stop("User defined node exceeds nodes used in model", call. = FALSE)
    }}
     assess_values <- t(combn(prior_scale , 2))
     assess_indicator <- t(combn(1:length(prior_scale), 2))
     temp <- cbind(assess_values, assess_indicator)

     # nodes to be evaluated
     nodes_temp <- rep(nodes, each = length(assess_values[,1]))


     for_mapply  <- data.frame(temp, nodes_temp)

     fun_mapply <- function(prior_scale_1, prior_scale_2, node){
       loo::compare(loo::loo(log_lik[,,prior_scale_1,node]),
                    loo::loo(log_lik[,,prior_scale_2, node]))
     }
     m <- t(mapply(fun_mapply, for_mapply[,3], for_mapply[,4], for_mapply[,5]))

    res <- data.frame(prior_scale_1 = for_mapply[,1],
                prior_scales_2 = for_mapply[,2],
                node = for_mapply[,5],
                elpd_loo_diff = m[,1], elpd_se = m[,2],
                looic_diff = -2 * m[,1], looic_se = 2 * m[,2])
  list(results = res)
}
