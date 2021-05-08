# load libraries
library(rstan)
library(bayesplot)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(HLMdiag)

# generate data: returns a list of hierachically defined data
gen_data <- function(N1, N2, theta, sigma_x, sigma_y, seed) {
  set.seed(seed)
  X = rnorm(n = N1, mean = theta, sd = sigma_x)
  Y = rnorm(n = N2, mean = X, sd = sigma_y)
  group = rep(1:N1, N2 / N1)
  data = list(N1 = N1,
              N2 = N2,
              sigma_x = sigma_x,
              sigma_y = sigma_y,
              group = group,
              Y = Y)
  return(data)
}

# fit mcmc centered/non-centered models on the data -> return fitted models
mcmc_fit <- function(data) {
  fit_c <- stan(
    file = "example1_repeated_measures/hb-centering.stan",  # Stan program
    data = data,    # named list of data
    chains = 4,             # number of Markov chains
    warmup = 200,          # number of warmup iterations per chain
    iter = 500,            # total number of iterations per chain
    cores = 4,              # number of cores (could use one per chain)
    refresh = 0            # progress shown
  )
  fit_nc <- stan(
    file = "example1_repeated_measures/hb-non-centering.stan",  # Stan program
    data = data,    # named list of data
    chains = 4,             # number of Markov chains
    warmup = 200,          # number of warmup iterations per chain
    iter = 500,            # total number of iterations per chain
    cores = 4,              # number of cores (could use one per chain)
    refresh = 0          # progress shown
  )
  return(list(c = fit_c, nc = fit_nc))
}

# set parameters for data generation
N1 = 5; N2 = c(10, 50, 100, 250, 500, 750, 1000)
sigma_y = 10; sigma_x = 100 # set variances for Y and X
theta = 1
# define vectors for saving output
rhat_centered = vector(length = length(N2)); rhat_noncentered = vector(length = length(N2))
neff_centered = vector(length = length(N2)); neff_noncentered = vector(length = length(N2))
# fit models
for (i in 1:length(N2)) {
  data = gen_data(N1 = N1, N2 = N2[i], theta = theta, sigma_x = sigma_x, sigma_y = sigma_y, seed = 1113)
  mcmc = mcmc_fit(data)
  rhat_centered[i] = max(rhat(mcmc$c)); rhat_noncentered[i] = max(rhat(mcmc$nc))
  neff_centered[i] = min(neff_ratio(mcmc$c)); neff_noncentered[i] = min(neff_ratio(mcmc$nc))
}
# prepare data for plotting
dt_rhat = data.frame(samples = N2, rhat_centered = rhat_centered,
                rhat_noncentered = rhat_noncentered) %>% melt(id.var = "samples")
dt_neff = data.frame(samples = N2, neff_centered = neff_centered,
                     neff_noncentered = neff_noncentered) %>% melt(id.var = "samples")
#plot data
ggplot(dt_rhat, aes(x = samples, y = value, col = variable)) + geom_line() + geom_point() +
  theme_bw()
ggplot(dt_neff, aes(x = samples, y = value, col = variable)) + geom_line() + geom_point() +
  theme_bw()








