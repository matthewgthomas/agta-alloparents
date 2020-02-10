#-----------------------------------------------------------------------------------------------
### CODE FOR ANALYSIS OF MOTES DATA
### WRITTEN AEP MARCH 2017
#-----------------------------------------------------------------------------------------------
################################################################################################

#-----------------------------------------------------------------------------------------------
#1. SET UP
#-----------------------------------------------------------------------------------------------
#NB THIS IS ALL AUTOMATED 
##### write camps together  ##### 
big_output <- rbind(output_Djibbut,output_Cemento,output_Dinipan,output_Djago,output_Dipaguiden,output_Didikeg)

##### rename variables  #### 
names(big_output)[1] <- "ref"
names(big_output)[2] <- "giverhouse"
names(big_output)[3] <- "camp"
names(big_output)[4] <- "givername"
names(big_output)[5] <- "giverage"
names(big_output)[6] <- "giversex"
names(big_output)[7] <- "role"
names(big_output)[8] <- "childname"
names(big_output)[9] <- "childref"
names(big_output)[10] <- "childage"
names(big_output)[11] <- "childhouse"
names(big_output)[12] <- "childsex"
names(big_output)[13] <- "r"
names(big_output)[14] <- "cat"
names(big_output)[15] <- "props1"
names(big_output)[16] <- "props"
names(big_output)[17] <- "freq"
names(big_output)[18] <- "family"
names(big_output)[19] <- "depend"
names(big_output)[20] <- "carer"
names(big_output)[21] <- "childdepend"
names(big_output)[22] <- "childcarer"
names(big_output)[23] <- "samecluster"
names(big_output)[24] <- "givermembership"
names(big_output)[25] <- "childmembership"
names(big_output)[26] <- "pcluster11"
names(big_output)[27] <- "allcats"
names(big_output)[28] <- "affinal"
names(big_output)[29] <- "maternal"
names(big_output)[30] <- "paternal"
names(big_output)[31] <- "samesex"
names(big_output)[32] <- "infantgiver"
names(big_output)[33] <- "infantchild"
names(big_output)[34] <- "kinship"

childcare <- big_output
#### change NAs in props to 0 #### 

childcare[is.na(childcare)] <- 0

##### add in reproductive stage and make catergorical  ####

attach(childcare)
childcare$reprod[giverage >= 45 ] <- "2" # post-reproduction
childcare$reprod[giverage >= 15 & giverage < 45] <- "1" # reproductive 
childcare$reprod[giverage < 15] <- "0" # pre-reproduction
detach(childcare)
childcare$postreprod <- childcare$reprod == 2
childcare$currentreprod <- childcare$reprod == 1
childcare$prereprod <- childcare$reprod == 0

##### make costs based on importance of caregiver, number of dependents and carers #### 
childcare$infantgiver[childcare$infantgiver == 0] <- 1 
childcare$infantchild[childcare$infantchild == 0] <- 1 

# first ego's cost (ecost) based on mean motes interaction for each caregiver. Shortest link used (i.e. if both mother and grandmother, mother cat used). costs are:
# mother = 0.34
# father = 0.22
# sis = 0.22
# bro = 0.24
# GM = 0.095
# GF = 0.06
# other = 0.06 

# First create the new field
childcare$ecost <- NA
# Then recode the old field into the new one for the specified rows
childcare$ecost[childcare$role=="mother"] <- 0.5
childcare$ecost[childcare$role=="father"] <- 0.25
childcare$ecost[childcare$role=="sister"] <- 0.25
childcare$ecost[childcare$role=="brother"] <- 0.25
childcare$ecost[childcare$role=="GM"] <- 0.00
childcare$ecost[childcare$role=="GF"] <- 0.00
childcare$ecost[childcare$role=="other"] <- 0.00
childcare$ecost[childcare$role=="none"] <- 0.00

# number of dependents (individuals >= 11 yrs) divded by number of carers (individuals => 6 years)

childcare$cost1 <- (childcare$depend/childcare$carer)*childcare$ecost

# weighted if infant in household or not (threshold 2.5 years as infant)

childcare$cost <- (childcare$cost1*childcare$infantgiver)

### need variables #### 
# explore age of child. 
childcare$need1 <- (childcare$childdepend/childcare$childcarer)
childcare$need <- (childcare$need1*childcare$infantchild)

childcare$carerneed1 <- (childcare$depend/childcare$carer)
childcare$carerneed <- (childcare$carerneed1*childcare$infantgiver)


#-----------------------------------------------------------------------------------------------
# 2.  CARE RECEIVED FROM DIFFERENT CARERS #################
#-----------------------------------------------------------------------------------------------
# NB THIS IS NOT ALL AUTOMATED, NEED TO MAKE ADJUSTMENTS TO THE FILE 
# first sum the number of beeps between households - i.e total number of interactions 
# between 541 childhousehold and 546 giverhousehold 

nonhouse <-  childcare [which(childcare$family == 0), ]
myvars <- c("childhouse", "props1", "giverhouse")
nonhouse <- nonhouse[myvars]

nonhouse1 <- ddply(nonhouse, c("childhouse", "giverhouse"), summarise,
               sum   = sum(props1),
               mean = mean(props1, na.rm=TRUE),
               sd   = sd(props1, na.rm=TRUE)
)
# view to confirm
nonhouse1


# this creates a variable for each GIVING household the total number of interactions they have received from the now
# CARING household - i.e. the NOW giving household of 541 has received 11.7 beeps from the NOW caring household 546
# to do this we need to make unique dyad ID's, if not it just replaces all givers beeps regardless of the childhousehold
# automate unique IDs for childcare (As this is bigger)

childcare <- transform(childcare, Cluster_ID = as.numeric(interaction(childhouse, giverhouse, drop=TRUE)))

# write childcare so can be view fully 
write.csv(childcare,file="~/Dropbox/document/PhD/Project/2017/childcaremarch17.csv")

# write nonhouse1 so can manually input the correct clutster IDs (can't think of a way of automating this)
write.csv(nonhouse1,file="~/Dropbox/document/PhD/Project/2017/nonhouse1.csv")

# read the new nonhouse1 file so can get the dyads
nonhouse1 <- read.csv("~/Dropbox/document/PhD/Project/2016/reciprocity/reciprocity July 16/data/nonhouse1.csv")

# combine the two df back together 
# make a new empty variable
childcare$contigencybeeps <- 0

# loop through each row and match Cluster_ID 
for(row in 1:nrow(childcare)) 
{
  disease_index = which(childcare$Cluster_ID[row] == nonhouse1$Cluster_ID)
  
  if ((is.integer(disease_index)) && (length(disease_index) == 0))
  {
    # dyad not found make result NA 
    childcare$contigencybeeps[row] <- NA

  }
  else   
  {
    childcare$contigencybeeps[row] = nonhouse1$sum[disease_index]

  }
  # close loop here
}

# quality check - NB make a second variable for contigency in which households without children have 
# 0 instead of NA as there score to examine during analysis. these households are:
# 8404, 626, 630, 631, 6603, 6604, 6607, 670 & 9211
write.csv(childcare,file="~/Dropbox/document/PhD/Project/2017/childcaremarch17.csv")
# read the file 
childcare <- read.csv("~/Dropbox/document/PhD/Project/2017/childcaremarch17.csv")

#-----------------------------------------------------------------------------------------------
# 3.  Make final variables required for analysis 
#-----------------------------------------------------------------------------------------------
# learn to mother variable 
# if non-mother and if girl aged less than 25 

childcare$learn <- ifelse(childcare$giversex == 1 & childcare$role!= "mother", 
                          c("1"), c("0")) 

# costly signal variable 
# if male of reproductive age 
childcare$signal <- ifelse(childcare$giversex == 0 & childcare$currentreprod == TRUE, 
                          c("1"), c("0")) 

#### kinship variables #### 
# with cut off for non-kin at 0.125
childcare$kinrole <- ifelse(childcare$r > 0.25, c("1"), c("0")) 
childcare$distantkinrole <- ifelse(childcare$r >= 0.125 & childcare$r <= 0.25, c("1"), c("0")) 
childcare$nonkinrole <- ifelse(childcare$r < 0.125, c("1"), c("0"))

# with cut off for non-kin at 0.0625
childcare$kinrole2 <- ifelse(childcare$r > 0.25, c("1"), c("0")) 
childcare$distantkinrole2 <- ifelse(childcare$r >= 0.0625 & childcare$r <= 0.25, c("1"), c("0")) 
childcare$nonkinrole2 <- ifelse(childcare$r < 0.0625, c("1"), c("0"))

# with cut off for non-kin at 0.0313
childcare$kinrole3 <- ifelse(childcare$r > 0.25, c("1"), c("0")) 
childcare$distantkinrole3 <- ifelse(childcare$r >= 0.03125 & childcare$r <= 0.25, c("1"), c("0")) 
childcare$nonkinrole3 <- ifelse(childcare$r < 0.03125, c("1"), c("0"))

# with cut off for non-kin at 0
childcare$kinrole4 <- ifelse(childcare$r > 0.25, c("1"), c("0")) 
childcare$distantkinrole4 <- ifelse(childcare$r > 0 & childcare$r <= 0.25, c("1"), c("0")) 
childcare$nonkinrole4 <- ifelse(childcare$r == 0, c("1"), c("0"))

#one variable for kinship
childcare$place[childcare$kinrole4 == 1] <- "1"
childcare$place[childcare$distantkinrole4 == 1] <- "2"
childcare$place[childcare$nonkinrole4 == 1] <- "3"

# update cost variable 
childcare$cost2 <- ifelse(childcare$family == 1, c(0), c(childcare$cost)) 

# make location
childcare$location <- ifelse(childcare$camp == 67 | childcare$camp == 92 | childcare$camp == 66, c(1), c(0)) 

# write the finished file and save for analysis 
write.csv(childcare,file="~/Dropbox/document/PhD/Project/2016/reciprocity/reciprocity July 16/data/childcaremarch17.csv")
