##
## Household-level SRMs
##
library(tidyverse)
library(rethinking)
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

allo = read.csv("../householdlevel.csv")

# hist(allo$sum)

##
## sort out dyad, ego and alter indices
##
# ego/alter indices
allo$i_ID = coerce_index(allo$giverhouse)
allo$j_ID = coerce_index(allo$childhouse)

# dyad indices
allo = allo %>% mutate(ij_ID = ifelse(i_ID < j_ID, paste0(i_ID, j_ID), paste0(j_ID, i_ID)))  # create dyad IDs
allo$ij_dyad_ID = coerce_index(allo$ij_ID)    # convert dyad IDs to numbers

# prepping individual dyads
ij_dyad_record = sort(unique(allo$ij_dyad_ID))
ij_dyads = data.frame(ij_dyad_record)

ij_dyads$id1 <- allo$i_ID[match(ij_dyads$ij_dyad_record, allo$ij_dyad_ID)]
ij_dyads$id2 <- allo$j_ID[match(ij_dyads$ij_dyad_record, allo$ij_dyad_ID)]

allo$id1 <- ij_dyads$id1[match(allo$ij_dyad_ID, ij_dyads$ij_dyad_record)]
allo$id2 <- ij_dyads$id2[match(allo$ij_dyad_ID, ij_dyads$ij_dyad_record)]
allo$i_in_dyad <- as.numeric(allo$i_ID != allo$id1) + 1

##
## set up data for Stan
##
N         = nrow(allo)
N_i__j_ID = max(allo$i_ID)
N_dyads   = max(allo$ij_dyad_ID)

d = list(
  N = N,
  N_i__j_ID = N_i__j_ID,
  N_dyads = N_dyads,
  
  # response variable
  y = allo$sum,
  
  # dyad, ego, alter IDs
  ij_dyad_ID = allo$ij_dyad_ID,
  i_in_dyad = allo$i_in_dyad,
  i_ID = allo$i_ID,
  j_ID = allo$j_ID,
  
  # covariates
  childdepend = allo$childdepend,
  giverdepend = allo$giverdepend,
  childcarer = allo$childcarer,
  givercarer = allo$givercarer,
  location = allo$location,
  need = allo$need,
  cost = allo$cost
)

##
## intercept-only model
##
m.null = stan(file = "SRM household - intercept-only.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.null = precis(m.null, depth = 2, prob = .96)

save(m.null, precis_m.null, file = "models/SRM household - intercept-only.rdata")
saveRDS(precis_m.null, file = "models/precis household - intercept.only.rds")

##
## depend
##
m.depend = stan(file = "SRM household - depend.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.depend = precis(m.depend, depth = 2, prob = .96)

save(m.depend, precis_m.depend, file = "models/SRM household - depend.rdata")
saveRDS(precis_m.depend, file = "models/precis household - depend.rds")

##
## carer
##
m.carer = stan(file = "SRM household - carer.stan", 
                data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
                init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.carer = precis(m.carer, depth = 2, prob = .96)

save(m.carer, precis_m.carer, file = "models/SRM household - carer.rdata")
saveRDS(precis_m.carer, file = "models/precis household - carer.rds")

##
## need model 
##
m.need = stan(file = "SRM household - need.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.need = precis(m.need, depth = 2, prob = .96)

save(m.need, precis_m.need, file = "models/SRM household - need.rdata")
saveRDS(precis_m.need, file = "models/precis household - need.rds")

##
## cost model 
##
m.cost = stan(file = "SRM household - cost.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.cost = precis(m.cost, depth = 2, prob = .96)

save(m.cost, precis_m.cost, file = "models/SRM household - cost.rdata")
saveRDS(precis_m.cost, file = "models/precis household - cost.rds")

##
## full model 
##
m.full = stan(file = "SRM household - full.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.full = precis(m.full, depth = 2, prob = .96)

save(m.full, precis_m.full, file = "models/SRM household - full.rdata")
saveRDS(precis_m.full, file = "models/precis household - full.rds")
