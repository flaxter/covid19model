library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data = readRDS('data/UK-Italy.rds')
m = stan_model('stan-models/base.stan')

fit = sampling(m,data=stan_data,iter=800,warmup=400,chains=1)

library(rstan)
library(abind)
source("cmdstanr-light.R")

##stan_data  <-  readRDS('data/UK-Italy.rds')

stan_data <- "data/base.data.R"

data  <- read_rdump("data/base.data.R")

data$M
data$N2
data$cases

## set the number of threads to use per chain
Sys.setenv(STAN_NUM_THREADS=3)
Sys.getenv("STAN_NUM_THREADS")

stan_model  <- "stan-models/base.stan"
stan_model_parallel  <- "stan-models/base_rs.stan"
stan_model_parallel_2  <- "stan-models/base_rs2.stan"
stan_model_parallel_3  <- "stan-models/base_rs3.stan"

iter <- 25

options(cmdstan_home="~/work/cmdstan-2.23-candidate")

elapsed  <- system.time(
    cfit <- cmdstan(stan_model,
                    data = stan_data,
                    seed = 123142,
                    chains = 1,
                    cores = 1,
                    num_warmup=iter,
                    num_samples=iter,
                    save_warmup=1
                    ))

elapsed

print(cfit, pars="lp__")

#   user  system elapsed
# 14.738   0.056  14.866

Sys.getenv("STAN_NUM_THREADS")

elapsed_rs  <- system.time(
    cfit_rs <- cmdstan(stan_model_parallel,
                       data = stan_data,
                       seed = 123142,
                       chains = 1,
                       cores = 1,
                       num_warmup=iter,
                       num_samples=iter,
                       save_warmup=1
                       ))

elapsed_rs

print(cfit_rs, pars="lp__")


elapsed_rs2  <- system.time(
    cfit_rs2 <- cmdstan(stan_model_parallel_2,
                       data = stan_data,
                       seed = 123142,
                       chains = 1,
                       cores = 1,
                       num_warmup=iter,
                       num_samples=iter,
                       save_warmup=1
                       ))


elapsed
elapsed_rs2

print(cfit_rs2, pars="lp__")

## lp__ -1607.06    1.48 4.58 -1615.04 -1611.58 -1606.61 -1604.65 -1599.95    10
##

## 132s on 1 core (old model)
## 113 on 1 core (new model)
## 87s on 2 cores
## 66s on 3 cores
## 64s on 4 cores


print(cfit, pars="lp__")
print(cfit_rs2, pars="lp__")

Sys.setenv(STAN_NUM_THREADS=4)

elapsed_rs3  <- system.time(
    cfit_rs3 <- cmdstan(stan_model_parallel_3,
                       data = stan_data,
                       seed = 123142,
                       chains = 1,
                       cores = 1,
                       num_warmup=iter,
                       num_samples=iter,
                       save_warmup=1
                       ))

## 25s

## 21s on 1 core (2nd gen model)
## 12s on 2 cores
## 10s on 3 cores
## 9s on 4 cores

elapsed_rs3
print(cfit_rs3, pars="lp__")
