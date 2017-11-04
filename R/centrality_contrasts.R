#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE


centrality_contrasts <- function(x, contrast_list, prob){
  df_cont <-   as_tibble(x[[2]])
  l <- contrast_list
  c_list <- list()
  for(i in 1:length(l)){
  one <- df_cont  %>% filter( node==l[[i]][2], measure == l[[i]][1])
  two <- df_cont  %>% filter( node == l[[i]][3], measure == l[[i]][1])

  dat_2 <- data.frame(one, two)
  c_list[[i]] <- dat_2 %>%
                 summarise(difference = mean(value - value.1),
                           post_sd = sd(value - value.1),
                           low_hdi = hdi(value - value.1, prob)[1],
                           up_hdi = hdi(value - value.1, prob)[2])
}
t <- (t(unlist(l)))
t2 <-  data.frame(t(matrix(t, 3)))
c <- suppressMessages(reshape2::melt(c_list))
c_temp <-  suppressMessages(reshape2::dcast(c, L1 ~ variable))
res <- suppressMessages(data.frame(t2, c_temp))
res <- res[,-4]
#colnames(c_temp)[1] <- "contrast"
#c_temp$contrast <- t2
colnames(res)[1] <- "Measure"
colnames(res)[2] <- "node_1"
colnames(res)[3] <- "node_2"
list(contrast_results = res)
}
