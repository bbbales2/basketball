data {
  int N; // Number of games
  int M; // Number of teams
  vector[N] diff;
  int<lower = 1, upper = M> hteam[N]; // Home team
  int<lower = 1, upper = M> ateam[N]; // Away team
}

parameters {
  real<lower = 0.0> sigma;
  real<lower = 0.0> tau;
  vector<multiplier=tau>[M] skills;
}

model {
  tau ~ normal(0, 100);
  skills ~ normal(0, tau);
  diff ~ normal(skills[hteam] - skills[ateam], sigma);
}
