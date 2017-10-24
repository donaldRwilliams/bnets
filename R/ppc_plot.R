#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @useDynLib bnets, .registration = TRUE

ppc_plot <- function(x, nodes, alpha, color_y_rep, color_y){

  if(sum(!is.na(match(x$nodes, nodes))) < 1){
    stop("At least one node does not match between ppc_plot and posterior_predict", call. = FALSE)
  }

  if(length(nodes) > length(x$nodes)){
    stop("Specified more nodes than in posterior_predictive", call. = FALSE)
  }

  temp_name <- paste("node", nodes, sep = "_")
  temp_df <- reshape2::melt(x$y_rep)

  plot_df <- temp_df %>% filter(L1 %in% temp_name)

  dat_y <-  suppressMessages(reshape2::melt(x$data[,nodes]))
  #n_row_X  <- nrow(d)
  names_d  <-  rep(paste("node", nodes, sep = "_"), each = length(x$data[,1]))
  names_plt <-  rep(paste("node", nodes, sep = " " ), each = 1)
  dat_y$L1 <- names_d
 # plotting options from bayesplot
 trim = FALSE
 size = 0.25

# plot
ggplot(plot_df, aes_(x = ~value)) +
        stat_density(aes_(group = ~Var2,
        color = "yrep"), geom = "line",  position = "identity",
        size = size, color = color_y_rep, alpha = alpha, trim = FALSE) +
   facet_wrap(~ L1, labeller=labeller(L1 = labels)) +
   stat_density(inherit.aes = FALSE, data = dat_y, aes(x= value),
                geom = "line", color = color_y, position = "identity",
                lineend = "round", size = 1, trim = FALSE) +
   facet_wrap(~ L1) +
   theme_bw() +
   xlab("")
}


#ppc_plot(y_rep, nodes = 1:10, alpha = .5, color_y_rep = "lightblue", color_y =  "black")


