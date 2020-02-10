##
## Fit Social Relations Models
## (after Koster and Leckie 2014)
##
library(tidyverse)
library(rethinking)
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

allo = read.csv("../alloready.csv")
# str(allo)

##
## sort out dyad, ego and alter indices
##
# ego/alter indices
allo$i_ID = coerce_index(allo$ref)
allo$j_ID = coerce_index(allo$childref)

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
  y = allo$freq,
  
  # dyad, ego, alter IDs
  ij_dyad_ID = allo$ij_dyad_ID,
  i_in_dyad = allo$i_in_dyad,
  i_ID = allo$i_ID,
  j_ID = allo$j_ID,
  
  # covariates
  childage = allo$childage,
  childsex = allo$childsex,
  r = allo$r,
  cost2 = allo$cost2,
  need = allo$need, 
  samecluster = allo$samecluster, 
  learn = allo$learn, 
  signal = allo$signal
)

##
## intercept-only model
##
m.null = stan(file = "SRM - intercept-only.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.null = precis(m.null, depth = 2, prob = .96)

save(m.null, precis_m.null, file = "models/SRM - intercept-only.rdata")

# precis_m.null

##
## relatedness model 
## freq ~ childage  + childsex  + r  + (1 | ref) + (1 | childref)
##
m.r = stan(file = "SRM - relatedness.stan", 
           data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
           init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.r = precis(m.r, depth = 2, prob = .96)

save(m.r, precis_m.r, file = "models/SRM - relatedness.rdata")

##
## cost model 
## freq ~ childage  + childsex  + cost2  + (1 | ref) + (1 | childref)
##
m.cost = stan(file = "SRM - cost.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.cost = precis(m.cost, depth = 2, prob = .96)

save(m.cost, precis_m.cost, file = "models/SRM - cost.rdata")

##
## need model 
## freq ~ childage  + childsex  + need + (1 | ref) + (1 | childref)
##
m.need = stan(file = "SRM - need.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.need = precis(m.need, depth = 2, prob = .96)

save(m.need, precis_m.need, file = "models/SRM - need.rdata")

##
## proximity/assoication 
## freq ~ childage  + childsex  + samecluster + (1 | ref) + (1 | childref)
##
m.proximity = stan(file = "SRM - proximity.stan", 
                   data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
                   init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.proximity = precis(m.proximity, depth = 2, prob = .96)

save(m.proximity, precis_m.proximity, file = "models/SRM - proximity.rdata")


##
## learning to mother model 
## freq ~ childage  + childsex + learn + (1 | ref) + (1 | childref)
##
m.learning = stan(file = "SRM - learning.stan", 
                  data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
                  init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.learning = precis(m.learning, depth = 2, prob = .96)

save(m.learning, precis_m.learning, file = "models/SRM - learning.rdata")

##
## costly signalling
## freq ~ childage  + childsex  + signal + (1 | ref) + (1 | childref)
##
m.signal = stan(file = "SRM - signal.stan", 
                data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
                init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.signal = precis(m.signal, depth = 2, prob = .96)

save(m.signal, precis_m.signal, file = "models/SRM - signal.rdata")

##
## full model 
## freq ~ childage  + childsex  + r  + cost2 + need + samecluster + 
##                 learn + signal + (1 | ref) + (1 | childref)
##
m.full = stan(file = "SRM - full.stan", 
              data = d, chains = 4, cores = 1, warmup = 1000, iter = 2000,
              init_r = 0.1, control = list(adapt_delta = 0.9, max_treedepth = 15))

precis_m.full = precis(m.full, depth = 2, prob = .96)

save(m.full, precis_m.full, file = "models/SRM - full.rdata")
