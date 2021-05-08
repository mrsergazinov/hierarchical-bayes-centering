data {
  int<lower=0> N1;
  int<lower=0> N2;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  int group[N2];
  vector[N2] Y;
}
parameters {
  vector[N1] X_offset;
  real theta;
}
transformed parameters {
  vector[N1] X;
  X = theta + X_offset;
}
model {
  X_offset ~ normal(0, sigma_x);
  Y ~ normal(X[group], sigma_y);
}
