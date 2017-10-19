#' @export
#' @import dplyr
#' @import HDInterval

extract_BETA <- function(x, prior_scale, prob){

  #a <- x$mod_fit

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

  # select all BETA estiamtes
  BETA_est <- temp %>%
    dplyr::select(starts_with("b."))

  # select intercepts and sigma
  not_BETA <- temp %>%
    dplyr::select(-starts_with("b.")) %>%
    select(contains("b_int."), contains("sigma."),
           contains("lp__"))

  # column names (only numbers) of BETA estiamtes
  #temp_name <- colnames(BETA_est)
  BETA_numbs <- as.numeric(gsub("*\\D", "", colnames(BETA_est)))

  # store unique variables
  x_vars <- NULL

  for(i in 1:length(BETA_numbs)){
    temp <- BETA_numbs[i]

    if(temp < 100){
      x_vars[i] <- substr(temp, 1, nchar(temp) - 1)
    }else if(t_test >= 100){
      x_vars[i] <- substr(temp, 1, nchar(temp) - 2)
    }
  }

  # unique variables, and get model sepecific BETAs
  # temporary variable in sequence
  x_vars <- length(unique(x_vars))
  temp_1 <- paste("b", ".", 1:x_vars, sep = "")
  temp_2 <- paste0(temp_1, ".")
  temp_dat <- lapply(temp_2[1:x_vars], function(x) select(BETA_est, contains(x)))
  names(temp_dat) <-  paste(rep("y", x_vars), 1:x_vars, sep = "_")
  temp_4 <- as.numeric(substr(names(temp_dat), 3, 10) )

  # list to store (p - y)
  l <- list()

  temp_5 <- seq(1:x_vars)

  for(i in 1:length(temp_5)){
    l[[i]] <- temp_5[-i]
  }

  pre <- paste("x", unlist(l), sep = c(""))
  new_names <- paste(rep(substr(names(temp_dat),1,10),
                         each = x_vars - 1), pre, sep = "~")

  # returned data.frame
  dat <- data.frame(temp_dat)
  colnames(dat) <- new_names

  # name for not_BETA
  not_BETA_int <- paste("y", 1:x_vars, "intercept", sep = "_") # intercept
  not_BETA_2 <- paste("y", 1:x_vars, "sigma", sep = "_") # sigma
  not_BETA_lp <- "lp__" # l_p


  colnames(not_BETA) <- c(not_BETA_int, not_BETA_2, not_BETA_lp)

  melt_dat <-suppressMessages(reshape2::melt(dat))

  results <-  melt_dat %>%
    group_by(variable) %>%
    summarise(mean = mean(value),
              median = median(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])

  list(summary = results, posterior_sample_BETA = dat,
       posterior_samples_not_BETA = not_coefs)
}

