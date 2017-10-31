#' @export
#' @import dplyr
#' @import HDInterval
#' @import R1magic
extract_BETA <- function(x, prior_scale, nodes = NULL, prob){

  # number of nodes
  K <- 1:x$stan_dat$K
  # extract names of fitted models: corresponds to priors
  names_fit <-  names(x$mod_fit)
  # take apart names to get prior scale of fitted models
  prior_fit <- as.numeric(unlist(lapply(names_fit, function(x) substr(x, 13,20))))
  prior_fit <- as.numeric(na.omit(prior_fit))

  # stop if prior_scale does not match prior of fitted model
  if(sum(as.numeric(prior_scale == prior_fit)) < 1){
    stop("prior_scale does not match scale used in models")
  }

  # temporary data.frame: select prior based on position
  temp_data <- data.frame(prior_fit, prior_position = 1:length(prior_fit))

  # model to be extracted from bridge_net
  temp_extract <- subset(temp_data, prior_fit == prior_scale)[,2]

  temp <- data.frame(rstan::extract(x$mod_fit[[temp_extract]]))

  if(is.numeric(x$stan_dat$lasso_scale) | is.numeric(x$ridge)){
  # select all BETA estiamtes
  BETA_est <- temp %>%
    dplyr::select(starts_with("b."))

  # select intercepts and sigma
  not_BETA <- temp %>%
    dplyr::select(-starts_with("b.")) %>%
    dplyr::select(contains("b_int."), contains("sigma."),
           contains("lp__"))

  # unique variables, and get model sepecific BETAs
  # temporary variable in sequence
  x_vars <- length(unique(K))
  temp_1 <- paste("b", ".", 1:max(K), ".", sep = "")
  temp_dat <- lapply(temp_1[1:max(K)], function(x)   dplyr::select(BETA_est, contains(x)))
  names(temp_dat) <-  paste(rep("y", max(K)), 1:length(K), sep = "_")

  # list to store (p - y)
  l <- list()
  for(i in 1:length(K)){
    l[[i]] <- K[-i]
  }

  pre <- paste("x", unlist(l), sep = c(""))
  new_names <- paste(rep(substr(names(temp_dat),1,10),
                         each = max(K) - 1), pre, sep = "~")

  # returned data.frame
  dat <- data.frame(temp_dat)
  colnames(dat) <- new_names

  # name for not_BETA
  not_BETA_int <- paste("y", 1:x_vars, "intercept", sep = "_") # intercept
  not_BETA_2 <- paste("y", 1:x_vars, "sigma", sep = "_") # sigma
  not_BETA_lp <- "lp__" # l_p


  colnames(not_BETA) <- c(not_BETA_int, not_BETA_2, not_BETA_lp)

  melt_dat <-suppressMessages(reshape2::melt(dat))

  if(is.null(nodes)){
  results <-  melt_dat %>%
    group_by(variable) %>%
    summarise(mean = mean(value),
              median = median(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])

  list(summary = results, posterior_sample_BETA = dat,
        posterior_samples_not_BETA = not_BETA)

  }else if (!is.null(nodes)){
    node_select <- c(nodes)
    node_select_BETA <- paste("y_", node_select, "~", sep = "")
    node_select_not_BETA <- paste("y_", node_select, "_", sep = "")
    node_BETA <- lapply(node_select_BETA[1:length(node_select)], function(x) dplyr::select(dat, matches(x)))
    node_not_BETA <- lapply(node_select_not_BETA[1:length(node_select)], function(x) dplyr::select(not_BETA, contains(x)))


    node_BETA_df <- data.frame(node_BETA)
    node_not_BETA_df <- data.frame(node_not_BETA)

    temp_names <- colnames(node_BETA_df)
    temp_names2 <- colnames(node_not_BETA_df)

    colnames(node_BETA_df) <- gsub("[.]{1}", "~", temp_names)

    melt_dat <-  suppressMessages(reshape2::melt(node_BETA_df))

    results <-  melt_dat %>%
      group_by(variable) %>%
      summarise(mean = mean(value),
                median = median(value),
                lb_hdi = hdi(value, prob)[1],
                ub_hdi = hdi(value, prob)[2])

  list(summary = results, posterior_sample_BETA = node_BETA_df,
         posterior_samples_not_BETA = node_not_BETA_df)}
# following is for horseshoe estimates
} else if(is.numeric(x$stan_dat$scale_global)){
  BETA_est <- temp %>%
    dplyr::select(starts_with("beta1."))

  # select intercepts and sigma
  not_BETA <- temp %>%
    dplyr::select(-starts_with("beta1.")) %>%
    dplyr::select(contains("b_int."), contains("sigma."),
                  contains("lp__"))

  x_vars <- length(unique(K))

  temp_1 <- paste("beta1", ".", 1:max(K), ".", sep = "")
  #temp_2 <- paste0(temp_1, ".")

  temp_dat <- lapply(temp_1[1:max(K)], function(x)   dplyr::select(BETA_est, contains(x)))

  names(temp_dat) <-  paste(rep("y", max(K)), 1:length(K), sep = "_")

  # list to store (p - y)
  l <- list()

  for(i in 1:length(K)){
    l[[i]] <- K[-i]
  }

  pre <- paste("x", unlist(l), sep = c(""))
  new_names <- paste(rep(substr(names(temp_dat),1,10),
                         each = max(K) - 1), pre, sep = "~")

  # returned data.frame
  dat <- data.frame(temp_dat)
  colnames(dat) <- new_names

  # name for not_BETA
  not_BETA_int <- paste("y", 1:x_vars, "intercept", sep = "_") # intercept
  not_BETA_2 <- paste("y", 1:x_vars, "sigma", sep = "_") # sigma
  not_BETA_lp <- "lp__" # l_p


  colnames(not_BETA) <- c(not_BETA_int, not_BETA_2, not_BETA_lp)

  melt_dat <-suppressMessages(reshape2::melt(dat))

  if(is.null(nodes)){
    results <-  melt_dat %>%
      group_by(variable) %>%
      summarise(mean = mean(value),
                median = median(value),
                lb_hdi = hdi(value, prob)[1],
                ub_hdi = hdi(value, prob)[2])

    list(summary = results, posterior_sample_BETA = dat,
         posterior_samples_not_BETA = not_BETA)

  }else if (!is.null(nodes)){
    node_select <- c(nodes)
    node_select_BETA <- paste("y_", node_select, "~", sep = "")
    node_select_not_BETA <- paste("y_", node_select, "_", sep = "")
    node_BETA <- lapply(node_select_BETA[1:length(node_select)], function(x) dplyr::select(dat, matches(x)))
    node_not_BETA <- lapply(node_select_not_BETA[1:length(node_select)], function(x) dplyr::select(not_BETA, contains(x)))


    node_BETA_df <- data.frame(node_BETA)
    node_not_BETA_df <- data.frame(node_not_BETA)

    temp_names <- colnames(node_BETA_df)
    temp_names2 <- colnames(node_not_BETA_df)

    colnames(node_BETA_df) <- gsub("[.]{1}", "~", temp_names)

    melt_dat <-  suppressMessages(reshape2::melt(node_BETA_df))

    results <-  melt_dat %>%
      group_by(variable) %>%
      summarise(mean = mean(value),
                median = median(value),
                lb_hdi = hdi(value, prob)[1],
                ub_hdi = hdi(value, prob)[2])

    list(summary = results, posterior_sample_BETA = node_BETA_df,
         posterior_samples_not_BETA = node_not_BETA_df)
 }
}}
