#' @export
#' @import rstan
#' @import parallel
#' @import R1magic
#' @import HDInterval
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @useDynLib bnets, .registration = TRUE

centrality_plot <- function(x, shape, size_1, size_2, width){
  df_plt <- x[[1]]
  df_plt %>% ggplot( aes(y = cent, x = node)) +
    geom_errorbar(aes(ymin = low_hdi, ymax = up_hdi), width = width) +
    geom_point(shape = shape, size = size_1) +
    geom_point(shape = shape, size = size_2, color = "white") +
    facet_grid(~ measure) +
    coord_flip() +
    theme_bw() +
    ylab("") +
    xlab("")+
    theme(panel.grid.minor = element_blank())
  }

