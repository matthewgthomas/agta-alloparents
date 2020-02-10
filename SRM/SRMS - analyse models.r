##
## Analysing SRMs
##
library(rstan)
library(rethinking)
library(loo)
library(tidyverse)
library(memisc)
library(xlsx)

source("getSummary.stan.r")

##
## load models
##
# load all models in one go (bad idea - uses lots of memory)
# lapply(list.files("models", pattern = "*.rdata", full.names = T), load, .GlobalEnv)

# load models one by one (because each model takes up a fair chunk of memory)
mods = list.files("models", pattern = "*.rdata", full.names = F)

for (m in mods) {
  load(file.path("models", m))
  rm(list = ls()[grep("^m\\..*", ls())])
}

rm(m, mods)

save.image("models/coefficients.rdata")

# lazily load models so we can extract just the `precis` objects for making the coefficients table
# mods = list.files("models", pattern = "*.rdata", full.names = F)
# 
# # convert .RData -> .rdb/.rdx
# # inspired by: https://stackoverflow.com/a/8703024
# load_model = function(m) {
#   e = local({load(file.path("models", m)); environment()})
#   tools:::makeLazyLoadDB(e, m)
#   lazyLoad(m)
# }
# 
# for (m in mods) load_model(m)

##
## check the models are ok
##
# plot(m.null)
# traceplot(m.null)

##
## make a table of all coefficients
##
srm.summary = mtable("Model 1" = precis_m.full,
                     summary.stats=F, getSummary = getSummary.precis,
                     coef.style="ci.se.horizontal", digits=2)



getSummary.precis(precis_m.full)


precis_list = ls()[grep("^precis_m.*", ls())]

wb = createWorkbook()

#sheet_coef = createSheet(wb, "Coefs")
#start_col = 1

for (p in precis_list) {
  tmp_sheet = createSheet(wb, p)
  tmp_smry = getSummary.precis(get(p))
  addDataFrame(tmp_smry, sheet=tmp_sheet, row.names = F)
  #addDataFrame(tmp_smry, sheet=tmp_sheet, startColumn=start_col, row.names=F)
  #start_col = start_col + ncol(tmp_smry)
}

saveWorkbook(wb, "SRM summaries.xlsx")

# or do this so that each model gets