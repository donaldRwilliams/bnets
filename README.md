# bnets: Bayesian network models via Stan
```diff
- CAUTION: 
- This package can be used, but it has not (yet) been thoroughly tested for 
- accuracy or compatability between functions. Experienced users can 
- certainly look at the code (still under development) and compare estimates 
- to other network packages. All feedback is welcome and greatly appreciated!
```


bnets allow for fitting regularized partial correlation networks. Regularization is acheived with Bayesian penalized regression techniques:

1. LASSO
2. Ridge regression
3. Horsehoe Estimators

### Installation
#### Development Version

```{r}
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("donaldRwilliams/bnets", args = "--preclean")
```
### Example

```{r}
library(bnets)
library(psych)

X <- bfi[1:100, 1:10]

# fit blasso (Bayesian LASSO) regression with three prior scales
mod_lasso <- blasso_net(X, lasso_df = 3, models = 3, prior_scale = c(0.01, 0.1, 0.5))

# compute log likelihood
ll_lasso <- log_likelihood(mod_lasso)

# compare models with LOO
loo <- global_out_of_sample(ll_lasso, fit_index = "loo")
loo$results

  prior_scale      elpd  elpd_se    looic looic_se    p_loo p_loo_se
1        0.01 -1286.438 22.66390 2572.877 45.32780 72.09085 4.133830
2        0.10 -1286.635 23.17878 2573.269 46.35756 82.75012 4.592892
3        0.50 -1288.412 23.27100 2576.824 46.54200 85.21935 4.739270

}
