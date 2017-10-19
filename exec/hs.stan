data {
  int < lower =0> N;
  int < lower =0> K;
  matrix [N, K] X;
  // horseshoe_prior
  real < lower =0> scale_global ;
  real < lower =1> nu_global ;
  real < lower =1> nu_local ;
  real < lower =0> slab_scale ;
  real < lower =0> slab_df ;
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
  vector[K]   b_int;
  vector<lower = 0> [K] sigma;
  real < lower =0> tau;
  vector < lower =0 >[K] lambda;
  real < lower =0> caux ;
}
transformed parameters {
  vector < lower =0 >[K] lambda_tilde;
  vector[K-1] beta1 [K];
  real < lower =0> c;
  c = slab_scale * sqrt ( caux );
  lambda_tilde = sqrt ( c^2 * square ( lambda ) ./ (c^2 + tau ^2* square ( lambda )) );
    for(k in 1:K){
    beta1[k] = b[k] * lambda_tilde[k] * tau;
  }
}
model {
  for(k in 1:K){
    // Likelihood
    X[,k] ~ normal(b_int[k] + X[,index[k]]*beta1[k], sigma[k]);
    //priors
    sigma[k] ~ cauchy(0, 2);
    b_int[k] ~ cauchy(0, 2);
    b[k] ~ normal (0, 1);
    //regularizing prior
    lambda ~ student_t (nu_local , 0, 1);
    tau ~ student_t ( nu_global , 0, scale_global * sigma );
    caux ~ inv_gamma (0.5* slab_df , 0.5* slab_df );
  }
}
