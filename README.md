# bnets: Bayesian network models via Stan
```diff
- #### this text is highlighted in red
```

bnets allow for fitting regularized partial correlation networks. Regularization is acheived with Bayesian penalized regression techniques:

1. LASSO
2. Ridge regression
3. Horeshoe Estimators

### Installation
#### Development Version

```{r}
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("donaldRwilliams/bnets", args = "--preclean")
```
