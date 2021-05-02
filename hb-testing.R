# load libraries
library(rstan)
library(bayesplot)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(HLMdiag)

# define function for comapring centered and non-centered parametrizations
compare_cp_ncp <- function(cp_plot, ncp_plot, ncol = 2, ...) {
  bayesplot_grid(
    cp_plot, ncp_plot,
    grid_args = list(ncol = ncol),
    subtitles = c("Centered parameterization",
                  "Non-centered parameterization"),
    ...
  )
}

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
  refresh = 1,             # progress shown
)
# plot the centered model
np <- nuts_params(fit1)
plt1 <- mcmc_pairs(
    fit1,
    np = np,
    pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"),
    diag_fun = c("dens"),
    off_diag_fun = c("hex"))
plt2 <- mcmc_scatter(
  fit1,
  pars = c("mu_b", "sigma_b"),
  np = np,
  size = 1)
plt3 <- mcmc_rhat(rhat(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))) + yaxis_text(hjust = 1)
plt4 <- mcmc_neff(neff_ratio(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))) + yaxis_text(hjust = 1)
plt5 <- mcmc_acf(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))

# fit the centered model
fit2 <- stan(
  file = "hb-non-centering.stan",  # Stan program
  data = data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1,             # progress shown
)
# plot the centered model
np2 <- nuts_params(fit2)
plt6 <- mcmc_pairs(
  fit2,
  np = np2,
  pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"),
  diag_fun = c("dens"),
  off_diag_fun = c("hex"))
plt7 <- mcmc_scatter(
  fit2,
  pars = c("mu_b", "sigma_b"),
  np = np2,
  size = 1)
plt8 <- mcmc_rhat(rhat(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))) + yaxis_text(hjust = 1)
plt9 <- mcmc_neff(neff_ratio(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))) + yaxis_text(hjust = 1)
plt10 <- mcmc_acf(fit1, pars = c("mu_a", "mu_b", "sigma_a", "sigma_b"))
# compare centered and non-centered models densities for sigma_bs
dt_cp = as.data.frame(fit1)
dt_ncp = as.data.frame(fit2)
dt_den_sigmab = data.frame(value = c(dt_cp$sigma_b, dt_ncp$sigma_b),
                           variable = rep(c("sigma_b_cp", "sigma_b_ncp"), each = length(dt_cp$sigma_b)))
ggplot(dt_den_sigmab, aes(x = value, fill = variable)) + geom_density(alpha = 0.2)
