#' @export
#' @import dplyr
#' @import HDInterval

extract_BETA <- function(x, prior_scale, prob){

  a <- x$mod_fit
  n <-  names(a)
  p_scl <- as.numeric(unlist(lapply(n, function(x) substr(x, 13,20))))
  p_scl <- as.numeric(na.omit(p_scl))

  if(sum(as.numeric(prior_scale == p_scl)) < 1){
    stop("prior_scale does not match scale used in models")
  }
  d <- data.frame(p_scl, 1:length(p_scl))
  list_el <- subset(d, p_scl == prior_scale)[,2]
  temp <- data.frame(rstan::extract(a[[list_el]]))
  coefs <- temp %>% dplyr::select(starts_with("b."))
  not_coefs <- temp %>% dplyr::select(-starts_with("b."))
  c_name <- colnames(coefs)
  test <- as.numeric(gsub("*\\D", "", c_name))

  x_vars <- NULL
  for(i in 1:length(test)){
    t_test <- test[i]
    if(t_test < 100){
      x_vars[i] <- substr(t_test, 1, nchar(t_test)-1)
    }
    else if(t_test >= 100){
      x_vars[i] <- substr(t_test, 1, nchar(t_test)-2)
    }
  }
  x_vars <- length(unique(x_vars))
  temp <- paste("b", ".", 1:x_vars, sep = "")
  temp <- paste0(temp, ".")
  temp_l <- lapply(temp[1:x_vars], function(x)  select(coefs, contains(x)))

  names(temp_l) <-  paste(rep("y", x_vars), 1:x_vars, sep = "_")

  t <- as.numeric(substr(names(temp_l), 3, 10) )
  l <- list()
  temp2 <- seq(1:x_vars)

  for(i in 1:length(t)){
    temp <- t[i]
    l[[i]] <- temp2[-temp]
  }

  pre <- paste("x", unlist(l), sep = c(""))
  new_names <- paste(rep(substr(names(temp_l),1,10),
                         each = x_vars - 1), pre, sep = "~")

  dat <- data.frame(temp_l)
  colnames(dat) <- new_names

  not_coef_1 <- paste("y", 1:x_vars, "intercept", sep = "_")
  not_coef_2 <- paste("y", 1:x_vars, "sigma", sep = "_")
  not_coef_3 <- "lp__"
  colnames(not_coefs) <- c(not_coef_1, not_coef_2, not_coef_3)

  b <-suppressMessages(reshape2::melt(dat))

  results <-  b %>% group_by(variable) %>%
    summarise(mean = mean(value),
              median = median(value),
              lb_hdi = hdi(value, prob)[1],
              ub_hdi = hdi(value, prob)[2])

  list(summary = results, posterior_sample_BETA = dat,
       posterior_samples_not_BETA = not_coefs)
}

