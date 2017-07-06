// This code is adapted from code stolen from https://bitbucket.org/flaxter/random-fourier-features-in-stan/

data {
  int<lower=1> N;
  int<lower=1> K;
  vector[N] x;
  int y[N];
}

transformed data {
  real scale;
  vector[K] omega;
  
  for(k in 1:K)
    omega[k] = normal_rng(0.0, 1.0);
  
  scale = sqrt(2.0 / N);
}

parameters {
  vector[K] beta1;
  vector[K] beta2;
  real<lower=0> bw;
}

transformed parameters {
  vector[N] fhat;
  
  {
    matrix[N, K] cosfeatures;
    matrix[N, K] sinfeatures;
    matrix[N, K] features;
    
    features = x * omega' * bw;
    for(i in 1:N)
      for(j in 1:K) {
        cosfeatures[i, j] = cos(features[i, j]);
        sinfeatures[i, j] = sin(features[i, j]);
      }
    cosfeatures = cosfeatures * scale;
    sinfeatures = sinfeatures * scale;
  
    fhat = cosfeatures * beta1 + sinfeatures * beta2;
  }
}

model {
  bw ~ normal(0.0, 1.0);
  beta1 ~ normal(0.0, 1.0);
  beta2 ~ normal(0.0, 1.0);

  y ~ bernoulli_logit(fhat);
}

generated quantities {
  vector[N] f = inv_logit(fhat);
}
