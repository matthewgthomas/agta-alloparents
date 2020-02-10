#-----------------------------------------------------------------------------------------------
### CODE FOR CREATING HOUSEHOLD LEVEL INTERACTION DATA
### WRITTEN AEP MAY 2018
#-----------------------------------------------------------------------------------------------
################################################################################################


# load completed dataframe at child-alloparent level
library(readr)
alloready <- read_csv("~/Dropbox/Work/reciprocity/Cooperation 17/alloready.csv")

# reduce dataframe to nonhousehold only 

nonhouse <-  alloready [which(alloready$family == 0), ]
myvars <- c("childhouse", "freq", "giverhouse")
nonhouse <- nonhouse[myvars]

require(plyr)
householdlevel <- ddply(nonhouse, c("childhouse", "giverhouse"), summarise,
                   sum   = sum(freq),
                   mean = mean(freq, na.rm=TRUE),
                   sd   = sd(freq, na.rm=TRUE)
)
# view to confirm
householdlevel

 write.csv(householdlevel, "~/Dropbox/Work/reciprocity/Cooperation 17/householdlevel.csv")
 
 # manually put in depends and carers so re-read data 
 
 library(readr)
 householdlevel <- read_excel("~/Dropbox/Work/reciprocity/Cooperation 17/householdlevel.xlsx")
 
 
 # make need and cost variables 
 # COST
 # number of dependents (individuals >= 11 yrs) divded by number of carers (individuals => 6 years)
 
 householdlevel$cost <- (householdlevel$giverdepend/householdlevel$givercarer)
 
 # NEED
 householdlevel$need <- (householdlevel$childdepend/householdlevel$childcarer)

 # ready for analysis - write
 
 write.csv(householdlevel, "~/Dropbox/Work/reciprocity/Cooperation 17/householdlevel.csv")
