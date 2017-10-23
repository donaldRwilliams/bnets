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
