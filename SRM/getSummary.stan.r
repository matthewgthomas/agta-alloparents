## ----------------------------------------------------------------------------
## Author: Matthew Gwynfryn Thomas (@matthewgthomas)
## URL: http://matthewgthomas.co.uk
## Date: 16 April 2015
##
## Description: 
##   This file contains a function for extracting BUGS (Bayesian inference Using Gibbs Sampling) 
##   social relations model (SRM) summaries according to the requirements of mtable() and toLatex() 
##   available in the memisc package (http://cran.r-project.org/web/packages/memisc/).
##
##   The model summary is geared towards Social Relations Models run using Win/OpenBUGS
##   and specified in a particular way (see comments below for details).
##
##   The summary works best when running mtable() with the coef.style="ci.se.horizontal" parameter:
##   > mtable("Model 1"=model1, "Model 2"=model2, coef.style="ci.se.horizontal")
##
## Note:
##   This code is based heavily on the examples and functions provided by
##   Martin Elff in his memisc package. If you find it useful, you should cite
##   *his* work. You can find his website here: http://www.martin-elff.net
##   This implementation also took a lot of inspiration from the getSummary methods
##   created by Jason W. Morgan, also available in the memisc package.
##
##
## Copyright 2015 Matthew Gwynfryn Thomas
## 
## This program is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the Free
## Software Foundation, either version 3 of the License, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
## more details.
##
## You should have received a copy of the GNU General Public License along with
## this program.  If not, see <http://www.gnu.org/licenses/>.
## ----------------------------------------------------------------------------


## ----------------------------------------------------------------------------
## Social relations model specification
## ----------------------------------------------------------------------------
##
## The getSummary.bugs() function expects an object of class 'bugs',
## output by bugs() in the R2WinBUGS package.
##
## The summary produced is based on the social relations model format used by Koster & Leckie (2014)
##  ("Food sharing networks in lowland Nicaragua: An application of the social relations model to count data")
## An example WinBUGS model specification is available from http://dx.doi.org/10.1016/j.socnet.2014.02.002
##
## The summary function assumes that the coefficients of interest are called 'beta'
## (e.g. in the SRM bugs model are an array beta[1], beta[2], ..., beta[n])
## 
## The summary also outputs:
## - giver/receiver/relationship variance and variance partition coefficients (VPC)
## - generalized/dyadic reciprocity coefficients
## - deviance information criterion (DIC)
## - numbers of burn-in iterations, monitoring chain iterations and chains
##
## If you want something more generic:
## - delete all the variance, correlation and VPC terms from setSummaryTemplate and getSummary.bugs
## - comment out lines 94-103
## - and uncomment line 106
##
## ----------------------------------------------------------------------------

library(memisc)
library(tidyverse)

###############################################################################
## Summary template
##
# setSummaryTemplate(precis = c("Giver variance"                      = "($sigma2_g:f#)",
#                               "Receiver variance"                   = "($sigma2_r:f#)",
#                               "Relationship variance"               = "($sigma2_dd:f#)",
#                               "Generalized reciprocity correlation" = "($rho_gr:f#)",
#                               "Dyadic reciprocity correlation"      = "($rho_dd:f#)",
#                               "Giver VPC"                           = "($vpc_giver:f#)",
#                               "Receiver VPC"                        = "($vpc_receiver:f#)",
#                               "Relationship VPC"                    = "($vpc_dyad:f#)"))


####################################################################
## getSummary() for BUGS social relations models
##
getSummary.precis <- function(obj, alpha = 0.05, ...) {
  #obj = precis_m.full
  
  intercept = "a"
  coefs = "b_"
  
  smry <- obj@output
  
  # keep mean and 95% credible intervals for the coefficient(s) of interest
  coef = smry %>% 
    tibble::rownames_to_column("Parameter") %>% 
    dplyr::filter(startsWith(Parameter, intercept) |
                  startsWith(Parameter, coefs) |
                  startsWith(Parameter, "variance") |
                  Parameter == "Rho_i__j_ID[1,2]" |
                  Parameter == "Rho_dyad_ID[1,2]" |
                  startsWith(Parameter, "VPC")) %>% 
    dplyr::select(Parameter, Mean, StdDev, dplyr::starts_with("lower"), dplyr::starts_with("upper"))
    #tibble::column_to_rownames("Parameter")
  
  #colnames(coef) <- c("est", "se", "lwr", "upr")
  
  #list(coef=coef) 
  coef
}
