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

# fit the models
fit_c <- stan(
  file = "example2_radon_data/hb-centering.stan",  # Stan program
  data = data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1             # progress shown
)
fit_nc <- stan(
  file = "example2_radon_data/hb-non-centering.stan",  # Stan program
  data = data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  refresh = 1             # progress shown
)

# extract NUTS info
np_c <- nuts_params(fit_c)
np_nc <- nuts_params(fit_nc)
# plot scatter plots of b[75] and sigma_b
plt_c_scatter <- mcmc_scatter(
  fit_c,
  pars = c("b[75]", "sigma_b"),
  np = np_c,
  size = 1)
plt_nc_scatter <- mcmc_scatter(
  fit_nc,
  pars = c("b[75]", "sigma_b"),
  np = np_nc,
  size = 1)
# compare centered and non-centered models densities for sigma_bs
dt_cp = as.data.frame(fit_c)
dt_ncp = as.data.frame(fit_nc)
dt_den_sigmab = data.frame(value = c(dt_cp$sigma_b, dt_ncp$sigma_b),
                           variable = rep(c("sigma_b_cp", "sigma_b_ncp"), each = length(dt_cp$sigma_b)))
ggplot(dt_den_sigmab, aes(x = value, fill = variable)) + geom_density(alpha = 0.2) +theme_bw()
