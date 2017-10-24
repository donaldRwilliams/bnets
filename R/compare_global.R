#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @import loo
#' @useDynLib bnets, .registration = TRUE

compare_global <- function(x){

  c_names <- names(loo$loo_list)
  assess_values <- t(combn(c_names , 2))
  fun_mapply <- function(prior_scale_1, prior_scale_2){
      loo::compare(loo$loo_list[[prior_scale_1]],
                   loo$loo_list[[prior_scale_2]])
    }
    m <- t(mapply(fun_mapply,  assess_values[,2], assess_values[,1]))
    dat <- suppressWarnings(data.frame(assess_values, round(m, 3)))
    looic <- dat$elpd_diff * -2
    se <- dat$se * 2
    results <- data.frame(dat, looic, se)
    colnames(results) <- c("prior_1", "prior_2", "elpd_diff",
                           "elpd_se", "looic_diff", "looic_se")
    list(results = results)
}

