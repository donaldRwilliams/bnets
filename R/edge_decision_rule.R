#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE


edge_decision_rule <- function(x, type, point_est = NULL,  prob = NULL, cutoff = NULL, direction = NULL){
  if(!is(x, "partial_corr")){
    stop("x is not of class partial_corr")
  }
  d_temp <- x$df_post
  nodes <-  length(unique(x$df_post$Var1))
if(type == "psuedo_z"){
    if(is.null(point_est)){
      stop("point estimate (mean, median, or mode) must be specified for psuedo_z")
    } else if (point_est == "mean"){
      mean_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(psuedo_z = mean(value) / sd(value))
      mean_mat <- matrix(mean_est$psuedo_z, nodes, nodes)
      diag(mean_mat) <- 1
      if(is.null(cutoff)){
        return(mean_mat)
      } else {
       mean_cut <- ifelse(abs(mean_est$psuedo_z) > cutoff, mean_est$psuedo_z, 0)
       mean_mat <- matrix(mean_cut, nodes, nodes)
       diag(mean_mat) <- 1
       return(mean_mat)
      } } else if(point_est == "mode"){
        mode_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(psuedo_z = mode(value) / sd(value))
        mode_mat <- matrix(mode_est$psuedo_z, nodes, nodes)
        diag(mode_mat) <- 1
        if(is.null(cutoff)){
          return(mode_mat)
        } else {
          mode_cut <- ifelse(abs(mode_est$psuedo_z) > cutoff, mode_est$psuedo_z, 0)
          mode_mat <- matrix(mode_cut, nodes, nodes)
          diag(mode_mat) <- 1
          return(mode_mat)
          }}
  else if(point_est == "median"){
            median_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(psuedo_z = median(value) / sd(value))
            median_mat <- matrix(median_est$psuedo_z, nodes, nodes)
            diag(median_mat) <- 1
            if(is.null(cutoff)){
              return(median_mat)
            } else {
              median_cut <- ifelse(abs(median_est$psuedo_z) > cutoff, median_est$psuedo_z, 0)
              median_mat <- matrix(median_cut, nodes, nodes)
              diag(median_mat) <- 1
              return(median_mat)
              }
}} else if (type == "interval") {
  if(is.null(prob)){
    stop("Must specify HDI probability")
  } else{
    hdi_est <- par_mod_1$df_post  %>% group_by(Var1, Var2) %>% summarise(m_value = mean(value), low = hdi(value, prob)[1], up = hdi(value, prob)[2])
    hdi_cut <- ifelse(hdi_est$low < 0 & hdi_est$up < 0, 1, ifelse(hdi_est$low > 0 & hdi_est$up > 0, 1, 0))
    hdi_cut_mat <- matrix(hdi_cut, nodes, nodes)
    hdi_mat <- matrix(hdi_est$m_value, nodes, nodes)
    sig_mat <- hdi_mat * hdi_cut
    return(sig_mat)
  }
} else if (type == "par_cor"){
  if(is.null(cutoff) | is.null(point_est)){
    stop("Must specify cutoff value or point_est")
  } else if(point_est == "mean"){
  parcor_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(mean_est = mean(value))
  parcor_cut <- ifelse(abs(parcor_est$mean_est) > cutoff,  parcor_est$mean_est, 0)
  parcor_mat <- matrix(parcor_cut, nodes, nodes)
  diag(parcor_mat) <- 1
  return(parcor_mat)
  } else if(point_est == "mode"){
    parcor_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(mode_est = mode(value))
    parcor_cut <- ifelse(abs(parcor_est$mode_est) > cutoff,  parcor_est$mode_est, 0)
    parcor_mat <- matrix(parcor_cut, nodes, nodes)
    diag(parcor_mat) <- 1
    return(parcor_mat)
  } else if(point_est == "median"){
    parcor_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(median_est = median(value))
    parcor_cut <- ifelse(abs(parcor_est$median_est) > cutoff,  parcor_est$median_est, 0)
    parcor_mat <- matrix(parcor_cut, nodes, nodes)
    diag(parcor_mat) <- 1
    return(parcor_mat)
  }}else if(type == "post_prob"){
    if(is.null(direction)){
      stop("Must specify direction")
    }else if(direction == "greater"){
      post_prob_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(pp = sum(value > 0) / length(value))
      post_prob_mat <- matrix(post_prob_est$pp, nodes, nodes)
      diag(post_prob_mat) <- 1
      if(is.null(cutoff)){
        return(post_prob_mat)
      } else{
        if(cutoff > 1){
          stop("Cutoff must be between 0 and 1")
        }
        pp_cut <- ifelse(post_prob_est$pp  > cutoff, post_prob_est$pp, 0)
        pp_mat <- matrix(pp_cut, nodes, nodes)
        return(pp_mat)
      }
}else if(direction == "lesser"){
    post_prob_est <- d_temp %>% group_by(Var1, Var2) %>% summarise(pp = sum(value < 0) / length(value))
    post_prob_mat <- matrix(post_prob_est$pp, nodes, nodes)
    diag(post_prob_mat) <- 1
    if(is.null(cutoff)){
      return(post_prob_mat)
    } else{
      if(cutoff > 1){
        stop("Cutoff must be between 0 and 1")
      }
      pp_cut <- ifelse(post_prob_est$pp  > cutoff, post_prob_est$pp, 0)
      pp_mat <- matrix(pp_cut, nodes, nodes)
      return(pp_mat)
    }
}}
}


