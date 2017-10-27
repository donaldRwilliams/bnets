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
### Model assessement via approximate leave-one-out cross-validation (LOOIC = LOO information criterion):
LOO weights are similar to AIC weights, and are often interpreted as the probability (conditioned on the models under consideration) of providing the best out-of-sample predictions. In this case, there is no clear model to select. Model averaging will be implemented soon.
```{r}
  prior_scale      elpd  elpd_se    looic looic_se    p_loo p_loo_se    loo_wt
1        0.01 -1286.564 22.64423 2573.128 45.28846 71.86439 4.135112 0.5482525
2        0.50 -1287.371 23.22311 2574.742 46.44621 84.29675 4.691217 0.2446736
3        0.10 -1287.538 23.17722 2575.076 46.35443 83.35517 4.624088 0.2070739
```
### Compare models via LOOIC differences that include a measure of uncertainty (standard error):
```{r}
compare_global(loo)
$results
  prior_1 prior_2 elpd_diff elpd_se looic_diff looic_se
1    0.01     0.1     0.974   2.201     -1.948    4.402
2    0.01     0.5     0.807   2.434     -1.614    4.868
3     0.1     0.5    -0.167   0.369      0.334    0.738
```

### Compute partial correlation matrix:
```{r}
par_corr_lasso <- partial_corr(mod_lasso, prior_scale = 0.01, prob = 0.90)
```
### Bayesian methods provide intervals for the partial correlations:
(Unlike classical methods that typically lack standard errors)
```
par_corr_lasso$summary[1:9,]
 Var1 Var2         mean       median        mode       lb_hdi       ub_hdi        lb_eq       ub_eq
1    1    2 -0.102719232 -0.097949139 -0.07105492 -0.193443307 -0.013588354 -0.203356665 -0.02029464
2    1    3 -0.015905113 -0.017542355 -0.01955822 -0.079800675  0.055765415 -0.086484687  0.05053733
3    1    4  0.074325782  0.066977974  0.05338389  0.005508365  0.156390627  0.009906923  0.16407786
4    1    5 -0.089859385 -0.084369070 -0.07073199 -0.169190213 -0.009913098 -0.179951824 -0.01699435
5    1    6  0.001303045  0.004648111  0.01895016 -0.068691991  0.074414624 -0.071362604  0.07314436
6    1    7 -0.025680071 -0.026214983 -0.02510438 -0.095282204  0.046677117 -0.098854827  0.04528436
7    1    8  0.009267619  0.012144221  0.02312044 -0.063009495  0.078254497 -0.061535691  0.08108611
8    1    9  0.035172190  0.032385065  0.02528911 -0.034745254  0.105885054 -0.033635502  0.10770205
9    1   10  0.050438370  0.046774957  0.03706542 -0.023173909  0.121173176 -0.020082586  0.12619366
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
A key aspect of Bayesian modeling is model checking. The idea is that our fitted models should generated data that looks like the observed data. The light blue lines are model implied data sets, and the dark lines are the observed outcomes. Importantly, while the big 5 inventory is often used to demonstrate network models, these posterior predictive checks suggest that assuming normality is not adequatlely describing the data (e.g., the data has a lower bound of 1, yet the fitted model is predicting many values less than 1) and that the model should be revised. 
```{r}
# posterior predictive
y_rep <- posterior_predict_net(mod_lasso, X, prior_scale = 0.01, nsims = 50, node = 1:10)
# plot
ppc_plot(X, y_rep)
```
![Optional Text](https://github.com/donaldRwilliams/images_bnets/blob/master/y_rep.PNG)

### In sample fit:
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
