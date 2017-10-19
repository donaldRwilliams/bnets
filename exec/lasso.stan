data{
  int<lower = 0> N;
  int<lower = 0> K;
  matrix[N, K] X;
  // LASSO_prior
  real<lower=0> lasso_df; // degrees of freedom: chi-square
  real<lower=0> lasso_scale; // scale: double exponential
}
transformed data{
  int index[K, K-1];
  for(k in 1:K){int displace = 0;
  for(j in 1:(K-1)){
    if(j == k){displace = 1;}
    index[k,j] = j + displace;}}
}
parameters {
  vector[K-1] b[K];
  vector[K] b_int;
  vector<lower = 0> [K] sigma;
  real<lower=0> lasso_inv_lambda; // LASSO parameter
}
model{
  for(k in 1:K){
 // Likelihood
    X[,k] ~ normal(b_int[k] + X[,index[k]]*b[k], sigma[k]);
//priors
    sigma[k] ~ cauchy(0, 2);
    b_int[k] ~ cauchy(0, 2);
//regularizing prior
    b[k] ~ double_exponential(0, lasso_scale * lasso_inv_lambda);
    lasso_inv_lambda ~ chi_square(lasso_df);
  }
}
