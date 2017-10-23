# bnets: Bayesian network models via Stan

bnets allow for fitting regularized partial correlation networks. Regularization is acheived with Bayesian penalized regression techniques:

1. LASSO
2. Ridge regression
3. Horeshoe Estimators

### Installation
#### Development Version
<span style="color:red">some **This is Red Bold.** text</span>
```{r}
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("donaldRwilliams/bnets", args = "--preclean")
```
