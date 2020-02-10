#-----------------------------------------------------------------------------------------------
### SENSTITVITY ANALYSIS FOR COST, NEED, RANDOM-LEVEL INCLUSION AND AGEDIFF 
#-----------------------------------------------------------------------------------------------
################################################################################################
#In first set of models mother is 0.5, father, sister and brother are 0.25 everyone else is 0. 


#-----------------------------------------------------------------------------------------------
# 1. remake the COST variables - 
#-----------------------------------------------------------------------------------------------
# cost3 =  mum 1, dad 0.5 and children 0.25 - slightly stronger beta but no other effect 
# cost4 = mum and dad 1 and children 0.5  (so different spacing) - no diff for the cost only model but significantly strong beta in full
#cost 5 = same spacing but different values  - mum-1 and father and sibs 0.5  - no change. 

#none change any other result. 

# First create the new field
childcare$ecost1 <- NA
# Then recode the old field into the new one for the specified rows
childcare$ecost1[childcare$role=="mother"] <- 1
childcare$ecost1[childcare$role=="father"] <- 0.5
childcare$ecost1[childcare$role=="sister"] <- 0.5
childcare$ecost1[childcare$role=="brother"] <- 0.5
childcare$ecost1[childcare$role=="GM"] <- 0.00
childcare$ecost1[childcare$role=="GF"] <- 0.00
childcare$ecost1[childcare$role=="other"] <- 0.00
childcare$ecost1[childcare$role=="none"] <- 0.00

# number of dependents (individuals >= 11 yrs) divded by number of carers (individuals => 6 years)

childcare$cost5 <- (childcare$depend/childcare$carer)*childcare$ecost1

# weighted if infant in household or not (threshold 2.5 years as infant)
childcare$infantgiver[childcare$infantgiver == 0] <- 1 
childcare$infantchild[childcare$infantchild == 0] <- 1 
childcare$cost5 <- (childcare$cost5*childcare$infantgiver)

# make sure doesn't include from own houseuold as then circular.  
childcare$cost5 <- ifelse(childcare$family == 1, c(0), c(childcare$cost5))

#-----------------------------------------------------------------------------------------------
# 2.re-run cost and full models (with childcare 5)
#-----------------------------------------------------------------------------------------------
#cost model 
# COST2
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + cost2 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + 
                (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + need + samecluster + cost2 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
#COST3
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + cost3 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + need + samecluster + cost3 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
#COST4
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + need + samecluster + cost4 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")

#COST5
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + cost5 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit4 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit4)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + need + samecluster + cost5 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")



aictab(cand.set = c(standfit1,  standfit2, standfit3, standfit4), sort = TRUE)


#-----------------------------------------------------------------------------------------------
# 3.AGE DIFF analysis
#-----------------------------------------------------------------------------------------------
##### subset for age difference - senstivity analysis groupings
# average birth 2.4 (median 2.3)
childcare2 <- subset(childcare, agediff > 2)
childcare5 <- subset(childcare, agediff > 5)
childcare10 <- subset(childcare, agediff > 10)

#childcare 2 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare2, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare2, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

#childcare 5
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

# childcare 10

fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare10, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare10, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)


#-----------------------------------------------------------------------------------------------
# 4.Run the interaction model with and without controls 
#-----------------------------------------------------------------------------------------------

#without controls 
fita <- glmer (freq ~  need*place + hours2 + (1 | ref) + (1 | childref) +  
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)

#with  controls 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff + r  + contigencybeeps2 + samecluster + cost4 + 
                 need*place + (1 | ref) + (1 | childref) +  
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)

#-----------------------------------------------------------------------------------------------
# 5.models with and without all random levels 
#-----------------------------------------------------------------------------------------------

#without all random levels 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

#with random levels 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)


#-----------------------------------------------------------------------------------------------
# 5.models with and without all control for age-diff levels 
#-----------------------------------------------------------------------------------------------

#without age diff and age group 
fita <- glmer (freq ~ hours2 + childage  + childsex  + r  + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

# with agee diff and age group 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

#-----------------------------------------------------------------------------------------------
# 5.models rec with obs instead of motes  
#-----------------------------------------------------------------------------------------------

# with obs 
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + CBobs + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

# normal 

fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + r  + contigencybeeps2 + cost4 + need + 
                 samecluster + learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)

