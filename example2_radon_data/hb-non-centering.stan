data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  vector[N] x;
  int county[N];
}
parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  vector[J] a_offset;
  vector[J] b_offset;
  real mu_a;
  real mu_b;
}
transformed parameters {
  vector[J] a;
  vector[J] b;
  a = mu_a + a_offset*sigma_a;
  b = mu_b + b_offset*sigma_b;
}
model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);
  sigma ~ cauchy(0, 5);
  sigma_a ~ cauchy(0, 5);
  sigma_b ~ cauchy(0, 5);
  a_offset ~ normal(0, 1);
  b_offset ~ normal(0, 1);
  y ~ normal(a[county] + b[county].*x, sigma);
}
