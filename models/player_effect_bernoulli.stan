data {
  int<lower = 1> N;
  int<lower = 1> M;
  matrix[N, M] X;
  int y[N];
}

parameters {
  real alpha;
  real<lower = 0.0> tau;
  vector[M] beta;
  //vector<lower = 0.0>[M] lambda;
}

model {
  tau ~ cauchy(0.0, 0.1);
  //lambda ~ cauchy(0.0, 1.0);
  beta ~ cauchy(0.0, tau);
  //beta ~ normal(0.0, tau);
  alpha ~ normal(0.0, 0.5);
  
  y ~ bernoulli_logit(X * beta + alpha);
}

generated quantities {
  vector[N] p = inv_logit(X * beta + alpha);
}