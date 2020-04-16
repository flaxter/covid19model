library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data = readRDS('data/UK-Italy.rds')
m = stan_model('stan-models/base.stan')

fit = sampling(m,data=stan_data,iter=800,warmup=400,chains=4)

