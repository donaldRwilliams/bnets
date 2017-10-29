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

  # get list names that correspond to priors
  list_names <- names(x$loo_list)

  # combn to evaluate pairwise loo
  assess_values <- t(combn(list_names , 2))

  # function for mapply
  fun_mapply <- function(prior_scale_1, prior_scale_2){
      loo::compare(x$loo_list[[prior_scale_1]],
                   x$loo_list[[prior_scale_2]])
    }

  # temporary results
  temp_results1 <- t(mapply(fun_mapply,
                           assess_values[,2], assess_values[,1]))

  temp_results2 <- suppressWarnings(data.frame(assess_values, round(temp_results1, 3)))

    looic <- temp_results2$elpd_diff * -2

    se <- temp_results2$se * 2

    results <- data.frame(temp_results2, looic, se)

    colnames(results) <- c("prior_1", "prior_2", "elpd_diff",
                           "elpd_se", "looic_diff", "looic_se")
    list(results = results)
}

