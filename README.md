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
library(qgraph)
```
#### Big five inventory data:
```
X <- bfi[1:100, 1:10]
```
#### fit blasso (Bayesian LASSO) regression with three prior scales"
```{r}
mod_lasso <- blasso_net(X, lasso_df = 3, models = 3, prior_scale = c(0.01, 0.1, 0.5))
```
#### compute log likelihood:
```{r}
ll_lasso <- log_likelihood(mod_lasso)
```
##### compare models with LOO
```{r}
loo <- global_out_of_sample(ll_lasso, fit_index = "loo")
```
```{r}
#### select models based on lowest LOOIC value
```{r}
loo$results
  prior_scale      elpd  elpd_se    looic looic_se    p_loo p_loo_se
1        0.01 -1286.438 22.66390 2572.877 45.32780 72.09085 4.133830
2        0.10 -1286.635 23.17878 2573.269 46.35756 82.75012 4.592892
3        0.50 -1288.412 23.27100 2576.824 46.54200 85.21935 4.739270
```
#### compute partial correlation matrix:
```{r}
par_corr_lasso <- partial_corr(mod_lasso, prior_scale = 0.01, prob = 0.90)
```
```
#### see results. Unlike classical methods that lack standard errors, Bayesain
#### methods provide intervals for the partial correlations"
```{r}
par_corr_lasso$summary[1:10,]
   Var1 Var2          mean      median          mode       lb_hdi     ub_hdi        lb_eq      ub_eq
1     1    2 -0.0876583794 -0.08570979 -2.033863e-03 -0.178225669 0.00000000 -0.199803674 0.00000000
2     1    3 -0.0090959536  0.00000000  2.543054e-04 -0.071883489 0.03953284 -0.071527051 0.04018490
3     1    4  0.0541886080  0.04519627  9.370606e-04  0.000000000 0.14595439  0.000000000 0.16017500
4     1    5 -0.0726108048 -0.06873063 -1.793063e-03 -0.156611130 0.00000000 -0.171409451 0.00000000
5     1    6  0.0005768901  0.00000000 -2.666742e-04 -0.054006828 0.05503979 -0.054015200 0.05502283
6     1    7 -0.0133036786  0.00000000  6.192118e-05 -0.076045259 0.03413132 -0.079492079 0.03169498
7     1    8  0.0050352558  0.00000000  7.230252e-05 -0.045944091 0.06237797 -0.045896976 0.06286394
8     1    9  0.0208020712  0.00000000  2.418707e-04 -0.026126197 0.09492174 -0.024757211 0.09661200
9     1   10  0.0323693183  0.01781611  3.977780e-04 -0.002002034 0.11743047 -0.006678188 0.11335299
10    2    3  0.1029003692  0.10023844  9.684066e-02  0.000000000 0.19354966  0.000000000 0.22063858
```
##### to visualize, we can choose the partial correlation mean, median, or mode.
###### 1) mode
```{r}
qgraph(par_corr_lasso$matrices$mode_par)
```
![Optional Text](https://github.com/donaldRwilliams/images_bnets/blob/master/mode.PNG)

###### 2) median
}
