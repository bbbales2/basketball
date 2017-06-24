data {
  int N;
  int x[N]; // group labels
  int y[N];
}

transformed data {
  int T = max(x);
}

parameters {
  vector[T] q;
}

model {
  q ~ normal(0, 1);
  
  for(n in 1:N)
    y[n] ~ bernoulli_logit(q[x[n]]);
}

generated quantities {
  vector[T] f = inv_logit(q);
}
