##
## Analysing SRMs at household level
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
precis_m.null = readRDS("models/precis household - intercept.only.rds")
precis_m.carer = readRDS("models/precis household - carer.rds")
precis_m.cost = readRDS("models/precis household - cost.rds")
precis_m.depend = readRDS("models/precis household - depend.rds")
precis_m.need = readRDS("models/precis household - need.rds")
precis_m.full = readRDS("models/precis household - full.rds")


##
## check the models are ok
##
# plot(m.null)
# traceplot(m.null)

##
## make a table of all coefficients
##
# srm.summary = mtable("Model 1" = precis_m.full,
#                      summary.stats=F, getSummary = getSummary.precis,
#                      coef.style="ci.se.horizontal", digits=2)
# 
# 
# 
# getSummary.precis(precis_m.full)


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

saveWorkbook(wb, "SRM household summaries.xlsx")

