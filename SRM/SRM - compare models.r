##
## SRMs - compare models
##
library(rstan)
library(loo)

load("models/SRM - intercept-only.rdata")

ll.null = extract_log_lik(m.null)
