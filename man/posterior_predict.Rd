\title{posterior predictive checks}
\name{posterior_predict}
\alias{posterior_predict}
\docType{package}
\description{
This function allows for node-wise posterior predictive checks, which can be used to assess how well the model fits the observed data.}
\usage{
posterior_predict(x, X, prior_scale = NULL, nodes, nsims)
}
\arguments{
\item{x} {A fitted model, or models, with either blasso_net, bridge_net, or bhs_net.}
\item{X} {Matrix of data used to fit models.}
\item{prior_scale} {prior_scale of fitted model object.}
\item{nodes} {The specific nodes to generate posterior predictive.}
\item{nsims} {The number of generated data sets.}
}
\examples{
library(psych)
X <- bfi[1:100, 1:5]

# fit model
mod_ridge <- bridge_net(X, models = 1, chains = 2, prior_scale = .2)

# posterior predictive
ppc_ridge <- posterior_predict(mod_ridge, X, prior_scale = .2,
             odes = 5, nsims = 50)

# plot density overlay
ppc_plot(ppc_ridge, nodes = 5, color_y_rep = "lightblue",
         color_y = "black", alpha = 0.8)
}
