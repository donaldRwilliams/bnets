# bnets: Bayesian network models via Stan
```diff
- CAUTION: 
- This package can be used, but it has not (yet) been thoroughly tested for 
- accuracy or function combatability. Experienced users can 
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
library(bayesplot)
```
### Big five inventory data:
```
X <- bfi[1:100, 1:10]
```
### Fit blasso (Bayesian LASSO) regression with three prior scales:
```{r}
mod_lasso <- blasso_net(X, lasso_df = 3, models = 3, prior_scale = c(0.01, 0.1, 0.5))
```
### Compute log likelihood:
```{r}
ll_lasso <- log_likelihood(mod_lasso)
```
### Compare models with LOO:
```{r}
loo <- global_out_of_sample(ll_lasso, fit_index = "loo")
```
```{r}
```
### Select models based on lowest LOOIC value:
```{r}
loo$results
  prior_scale      elpd  elpd_se    looic looic_se    p_loo p_loo_se
1        0.01 -1286.438 22.66390 2572.877 45.32780 72.09085 4.133830
2        0.10 -1286.635 23.17878 2573.269 46.35756 82.75012 4.592892
3        0.50 -1288.412 23.27100 2576.824 46.54200 85.21935 4.739270
```
### Compute partial correlation matrix:
```{r}
par_corr_lasso <- partial_corr(mod_lasso, prior_scale = 0.01, prob = 0.90)
```
### Unlike classical methods that lack standard errors, Bayesain
### methods provide intervals for the partial correlations:
```
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
### We can choose the partial correlation mean, median, or mode.
##### 1) Mode:
```{r}
qgraph(par_corr_lasso$matrices$mode_par)
```
![Optional Text](https://github.com/donaldRwilliams/images_bnets/blob/master/mode.PNG)
##### 2) Mean:
```{r}
qgraph(par_corr_lasso$matrices$mean_par)
```
![Optional Text](https://github.com/donaldRwilliams/images_bnets/blob/master/mean.PNG)

### Posterior predictive checks:
#### A key aspect of Bayesian modeling is model checking. The idea is that our fitted models should generated data that looks like the observed data. The light blue lines are model implied replications, and the dark lines are the observed outcome.
```{r}
# posterior predictive
y_rep <- posterior_predict_net(mod_lasso, X, prior_scale = 0.01, nsims = 50, node = 1:10)
# plot
ppc_plot(X, y_rep)
```
![Optional Text](https://github.com/donaldRwilliams/images_bnets/blob/master/y_rep.PNG)

### In sample fit:
#### (Bayesian methods provide probability intervals)
```{r}
fit_blasso <- in_sample_fit(mod_lasso, X, fit_index = "all", prior_scale = 0.01, node = 1:5, prob = 0.90)
fit_blasso$summary_all

   variable  fit_index       mean     median       mode     lb_hdi    ub_hdi
1    node_1  bayes_MSE 0.91640632 0.91246702 0.90513414 0.85871756 0.9772988
2    node_2  bayes_MSE 0.72276757 0.71743614 0.70738883 0.66850231 0.7768297
3    node_3  bayes_MSE 0.64338362 0.63840872 0.63284963 0.59698476 0.6870526
4    node_4  bayes_MSE 0.92429050 0.91987506 0.90475765 0.87073946 0.9825584
5    node_5  bayes_MSE 0.60848490 0.60384825 0.59095778 0.55785515 0.6542702
6    node_1   bayes_r2 0.08284408 0.07806330 0.07202419 0.02147104 0.1501508
7    node_2   bayes_r2 0.23128155 0.23180588 0.23414447 0.12048099 0.3352722
8    node_3   bayes_r2 0.31061342 0.31298747 0.32109351 0.19646140 0.4218335
9    node_4   bayes_r2 0.07727938 0.07205418 0.05754770 0.01362520 0.1356741
10   node_5   bayes_r2 0.33764533 0.34142361 0.37217387 0.21418973 0.4481516
11   node_1 bayes_RMSE 0.95709255 0.95523139 0.95192781 0.92667014 0.9885842
12   node_2 bayes_RMSE 0.84991131 0.84701602 0.84125406 0.81761990 0.8813794
13   node_3 bayes_RMSE 0.80189666 0.79900483 0.79560002 0.77264789 0.8288864
14   node_4 bayes_RMSE 0.96122463 0.95910117 0.95118650 0.93313421 0.9912409
15   node_5 bayes_RMSE 0.77978819 0.77707673 0.76990082 0.74689702 0.8088697
```
