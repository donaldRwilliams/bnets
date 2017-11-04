#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @import rstanarm
#' @useDynLib bnets, .registration = TRUE

project_predict <- function(X, prior){
  X <- scale(X)
  l <- fit <- beta_l <- fit_cv <- list_pctch <-
    selected <- par_cor <- m <- par_cor <- list()
  for(i in 1:ncol(X)){
    y <- X[,i]
    X_pred <- X[,-i]
    l[[i]] <- suppressWarnings(stan_glm(y ~ X_pred, family=gaussian(), prior=prior,
                    seed=1, adapt_delta=0.50, chains=1, iter=800))
}
for(i in 1:ncol(X)){
fit[[i]] <-  varsel(l[[i]], method='forward')
fit_cv[[i]] <- cv_varsel(l[[i]], method='forward', cv_method='LOO', )
}
for(i in 1:ncol(X)){
 s <- fit_cv[[i]]$varsel$ssize
 list_pctch[[i]] <- fit_cv[[i]]$varsel$pctch[,1:(1 + s)]
}
for(i in 1:ncol(X)){
 s2 <- colnames(list_pctch[[i]])[-1]
  selected[[i]] <- gsub(pattern =  "X_pred", replacement = "", x = s2)
}

for(i in 1:ncol(X)){
 b1 <- t(project(fit[[i]], nv = length(selected[[i]]))$beta)
 colnames(b1) <- selected[[i]]
 beta_l[[i]] <- b1
}

mat_matrix <- data.frame(matrix(0, ncol(X), ncol(X)))
colnames(mat_matrix) <- colnames(X)
ns <- 400
for(j in 1:ns){
for(i in 1:ncol(X)){
mat_matrix[i, names(beta_l[[i]][i,])] <- beta_l[[i]][j,]
m[[j]] <- mat_matrix
par_cor[[j]] <- parcor::Beta2parcor(mat_matrix)
}
}
t <- reshape2::melt(par_cor)
t$L2 <- 1:ncol(X)
mat_results <- t %>%
  group_by(variable, L2) %>%
  summarise(mean = mean(value),
            median = median(value),
            mode = mode(value))
mean_par <- matrix(mat_results$mean,  nrow =  ncol(X))
mean_par
median_par <- matrix(mat_results$median,  ncol(X))
#mode_par <- matrix(mat_results$mode,  10)
colnames(mean_par) <- colnames(X)
colnames(median_par) <- colnames(X)
list(par_mats =  list(mean_par, median_par), fit = fit)
}

