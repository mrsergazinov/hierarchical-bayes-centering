# load libraries
library(rstan)
library(bayesplot)
library(tidyverse)
library(ggplot2)
library(HLMdiag)

# load and preprocess the data: create list with variables needed for the model
data(radon)
data = list(J = radon$county %>% n_distinct(),
            N = nrow(radon),
            y = radon$log.radon,
            x = radon$basement,
            county = radon$county)


# fit the centered model
fit1 <- stan(
  file = "hb-centering.stan",  # Stan program
  data = data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1,             # no progress shown
  control = list(adapt_delta = 0.99)
)

np <- nuts_params(fit1)
plt1 <- mcmc_pairs(
    posterior,
    np = np,
    pars = c("sigma_a", "sigma_b", "mu_a", "mu_b"),
    diag_fun = c("dens"),
    off_diag_fun = c("hex"))
plt2 <- mcmc_rhat(rhat(fit1, pars = c("sigma_b", "mu_b", "mu_a", "sigma_a"))) + yaxis_text(hjust = 1)
plt3 <- mcmc_neff(neff_ratio(fit1, pars = c("sigma_b", "mu_b", "mu_a", "sigma_a"))) + yaxis_text(hjust = 1)
plt4 <- mcmc_acf(fit1, pars = c("sigma_b", "mu_b", "mu_a", "sigma_a"), lags = 10)
