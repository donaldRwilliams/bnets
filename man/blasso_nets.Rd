\title{Bayesian LASSO regression}
\name{blasso_net}
\alias{blasso}
\docType{package}
\description{
  This function allows for fitting Bayesian node-wise LASSO regressions. The model estimates are then used to obtain regularized partial correlations.}
\usage{
  blasso_net(X, lasso_df,  prior_scale = 1, models)
}
\arguments{
  \item{X} {Matrix of data used to fit models.}
  \item{models} {The number of models to be estimated. Must match length of prior_scale.}
  \item{prior_scale} {The scale parameter of the double exponential distribution on the coefficients. Smaller values will produce more shrinkage towards zero.}
  \item{lasso_df} {Chi-squared distribution degrees of freedom for the inverse of the lambda parameter}
}
\examples{
  library(psych)
  library(qgraph)
  X <- bfi[1:100, 1:10]

  # fit models
  mod_blasso <- blasso_net(X, models = 5, prior_scale = seq(.1, .5,  by = .1))

  # compare with leave-one-out cross-validation
  # 1) extract log-likelihood
  ll_blasso <- log_likelihood(mod_blasso)

  # 2) check LOO values and weights
  loo_blasso <- global_out_of_sample(ll_blasso, fit_index = "loo")
  loo_blasso$results

  # 3) direcly compare LOO
  compare_global(loo_blasso)

  # 4) compute partial correlation matrix
  par_corr_blasso <- partial_corr(mod_blasso,  prior_scale = 0.1, prob  = 0.90)

  # can choose mean, median, or mode of partial posterior
  # (use qgraph for plotting)
  qgraph(par_corr_blasso$matrices$median_par)
}

\references{

  N. Kraemer, J. Schaefer, A.-L. Boulesteix (2009) "Regularized Estimation of Large-Scale Gene
Regulatory Networks with Gaussian Graphical Models", BMC Bioinformatics, 10:384
  \url{http://www.biomedcentral.com/1471-2105/10/384/}

  Park, T., Casella, G. (2008). The Bayesian Lasso.
  Journal of the American Statistical Association, 103(482), June 2008, pp. 681-686
  \url{http://www.stat.ufl.edu/~casella/Papers/Lasso.pdf}

}
