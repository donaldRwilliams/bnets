#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE


edge_decision_rule <- function(x, rule, point_est = NULL,  cut_off = NULL, prob = NULL){

  d_temp <- x$df_post
  d_temp <- d_temp %>% filter(value != 0)
  nodes <-  length(unique(x$df_post$Var1))
if(rule == "par_cor" && is.numeric(prob)){
  warning("HDI specificed for par_cor and is thus ignored")
}
if(rule == "post_prob" && is.numeric(cut_off)){
  warning("Cut_off specified for post_prob and is thus ignored")
}
if(rule == "interval" && is.numeric(cut_off)){
    warning("Cut_off specified for interval and is thus ignored")
  }

if (rule == "interval") {
  if(is.null(prob) | is.null(point_est)){
    stop("Must specify HDI probability or point_est")
  } else if (point_est == "mean") {
    hdi_est <- d_temp %>%
               group_by(Var1, Var2) %>%
               summarise(m_value = mean(value),
                         low = hdi(value, prob)[1],
                         up = hdi(value, prob)[2])
    hdi_cut <- ifelse(hdi_est$low < 0 & hdi_est$up < 0, 1,
                      ifelse(hdi_est$low > 0 & hdi_est$up > 0, 1, 0))
    hdi_cut_mat <- matrix(hdi_cut, nodes, nodes)
    hdi_mat <- matrix(hdi_est$m_value, nodes, nodes)
    sig_mat <- hdi_mat * hdi_cut
    colnames(sig_mat) <- colnames(x$stan_dat)
    return(sig_mat)
} else if (point_est == "mode") {
    hdi_est <- d_temp %>%
      group_by(Var1, Var2) %>%
      summarise(m_value = mode(value),
                low = hdi(value, prob)[1],
                up = hdi(value, prob)[2])
    hdi_cut <- ifelse(hdi_est$low < 0 & hdi_est$up < 0, 1,
                      ifelse(hdi_est$low > 0 & hdi_est$up > 0, 1, 0))
    hdi_cut_mat <- matrix(hdi_cut, nodes, nodes)
    hdi_mat <- matrix(hdi_est$m_value, nodes, nodes)
    sig_mat <- hdi_mat * hdi_cut
    colnames(sig_mat) <- colnames(x$stan_dat)
    return(sig_mat)
} else if (point_est == "median") {
    hdi_est <- d_temp %>% filter(value != 0) %>%
      group_by(Var1, Var2) %>%
      summarise(m_value = median(value),
                low = hdi(value, prob)[1],
                up = hdi(value, prob)[2])
    hdi_cut <- ifelse(hdi_est$low < 0 & hdi_est$up < 0, 1,
                      ifelse(hdi_est$low > 0 & hdi_est$up > 0, 1, 0))
    hdi_cut_mat <- matrix(hdi_cut, nodes, nodes)
    hdi_mat <- matrix(hdi_est$m_value, nodes, nodes)
    sig_mat <- hdi_mat * hdi_cut
    colnames(sig_mat) <- colnames(x$stan_dat)
    return(sig_mat)
}} else if (rule == "post_prob"){
  if(is.null(point_est)){
    stop("Must specify  point_est")
  } else if(point_est == "mean"){
  post_prob_est <- d_temp %>%
                filter(value != 0) %>%
                group_by(Var1, Var2) %>%
                summarise(m_value = mean(value),
                prob_greater = sum(value > 0) / length(value),
                prob_less = sum(value < 0) / length(value))

  post_prob_cut <- ifelse(post_prob_est$prob_greater > prob,  1,
                       ifelse(post_prob_est$prob_less > prob, 1, 0))
  post_prob_cut_mat <- matrix(post_prob_cut, nodes, nodes)
  post_prob_mat <- matrix(post_prob_est$m_value, nodes, nodes)
  sig_mat <- post_prob_mat  * post_prob_cut_mat
  colnames(sig_mat) <- colnames(x$stan_dat)
  return(sig_mat)
} else if(point_est == "mode"){
    post_prob_est <- d_temp %>%
      filter(value != 0) %>%
      group_by(Var1, Var2) %>%
      summarise(m_value = mode(value),
                prob_greater = sum(value > 0) / length(value),
                prob_less = sum(value < 0) / length(value))

    post_prob_cut <- ifelse(post_prob_est$prob_greater > prob,  1,
                         ifelse(post_prob_est$prob_less > prob, 1, 0))
    post_prob_cut_mat <- matrix(post_prob_cut, nodes, nodes)
    post_prob_mat <- matrix(post_prob_est$m_value, nodes, nodes)
    sig_mat <- post_prob_mat  * post_prob_cut_mat
    colnames(sig_mat) <- colnames(x$stan_dat)
    return(sig_mat)
} else if(point_est == "median"){
    post_prob_est <- d_temp %>%
      filter(value != 0) %>%
      group_by(Var1, Var2) %>%
      summarise(m_value = median(value),
                prob_greater = sum(value > 0) / length(value),
                prob_less = sum(value < 0) / length(value))

    post_prob_cut <- ifelse(post_prob_est$prob_greater > prob,  1,
                         ifelse(post_prob_est$prob_less > prob, 1, 0))
    post_prob_cut_mat <- matrix(post_prob_cut, nodes, nodes)
    post_prob_mat <- matrix(post_prob_est$m_value, nodes, nodes)
    sig_mat <- post_prob_mat  * post_prob_cut_mat
    colnames(sig_mat) <- colnames(x$stan_dat)
  return(sig_mat)
}}else if (rule == "par_cor"){
  if(is.null(cut_off)){
    stop("Must specify cut off")
}else if(point_est == "mean"){
    parcor_est <- d_temp %>%
      filter(value != 0) %>%
      group_by(Var1, Var2) %>%
      summarise(m_value = mean(value))
    parcor_cut <- ifelse(abs(parcor_est$m_value) > cut_off,  1, 0)
    parcor_cut_mat <- matrix(parcor_cut, nodes, nodes)
    parcor_mat <- matrix(parcor_est$m_value, nodes, nodes)
    sig_mat <- parcor_mat  * parcor_cut_mat
    colnames(sig_mat) <- colnames(x$stan_dat)
    return(sig_mat)
} else if(point_est == "mode"){
      parcor_est <- d_temp %>%
        filter(value != 0) %>%
        group_by(Var1, Var2) %>%
        summarise(m_value = mode(value))
      parcor_cut <- ifelse(abs(parcor_est$m_value) > cut_off,  1, 0)
      parcor_cut_mat <- matrix(parcor_cut, nodes, nodes)
      parcor_mat <- matrix(parcor_est$m_value, nodes, nodes)
      sig_mat <- parcor_mat  * parcor_cut_mat
      colnames(sig_mat) <- colnames(x$stan_dat)
      return(sig_mat)
} else if(point_est == "median"){
        parcor_est <- d_temp %>%
          filter(value != 0) %>%
          group_by(Var1, Var2) %>%
          summarise(m_value = mode(value))
        parcor_cut <- ifelse(abs(parcor_est$m_value) > cut_off,  1, 0)
        parcor_cut_mat <- matrix(parcor_cut, nodes, nodes)
        parcor_mat <- matrix(parcor_est$m_value, nodes, nodes)
        sig_mat <- parcor_mat  * parcor_cut_mat
        colnames(sig_mat) <- colnames(x$stan_dat)
        return(sig_mat)}
  }
}



