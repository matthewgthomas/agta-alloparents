########################################################################
### CODE FOR ANALYSIS OF MOTES DATA
### WRITTEN MAY 2015 BY HUNTER-GATHERER RESILIENCE TEAM
########################################################################

#################
### SET UP
#################

loc = "Abbey" #To have the correct stwd. 

camps <- c("Djibbut","Cemento","Dinipan","Djago","Dipaguiden","Didikeg")

numb_camps <- length(camps)
output = NULL
big_output = NULL

for(xcamp in 1:numb_camps){
  
  #### set some parameters
  # take all kinship types
  kinship_thresh = 0
  method = "ALL" ### if we want to inc all interactions, then "ALL", if we want to exlude houshold then "noHH"
  CampName <- camps[xcamp]
  sex_select<-"" ### or "female" or "" (for both)
  
  if(loc=="Abbey") {setwd(paste("/Users/Abbey/Dropbox/Work/reciprocity/Cooperation 17/Motes raw/",CampName,"/matrix", sep=""))}
  
  #### IMPORT THE FILES
  egoinfo <- read.csv("egoinfo.csv")
  freq <- read.csv("freq9.csv", header=F)
  hours <- read.csv("hours.csv", header=F)
  kinmat <- read.csv("kinmat.csv", header=F)
  categories <- read.csv("categories.csv")
  allcats <- read.csv("allcats.csv")
  props1 <- read.csv("prop1.csv", header=F)
  
  # rename 
  ref <- egoinfo$ref
  name <- egoinfo$name
  household <- egoinfo$family
  people <- nrow(hours)
  times <- ncol(hours)
  dat <- hours
  families <- egoinfo$family
  sex <- egoinfo$sex
  age <- egoinfo$age
  camp <- egoinfo$camp
  role <- egoinfo$role
  depend <- egoinfo$depend
  carer <- egoinfo$carer
  cmembership <- egoinfo$membership 
  pmembership <- egoinfo$pmembership
  pmembership11 <- egoinfo$pmembership11
  infant <- egoinfo$infant
  
  #####################
  ### (1) 
  ### HOW LONG WAS EACH INDIVIDUAL IN THE EXPERIMENT?
  #####################
  inds_time <- 1:people
  for(i in 1:people){
    inds_time[i] <-  sum(dat[i,])
  }
  inds_beeps <- inds_time*30  ### so this is a simple list of how long each individual was in the experiment
  
  
  #####################
  ### (2) 
  ### POTENTAIL BEEPS BY DYAD - How long were each dyad in the experiment at the same time?
  #####################
  
  tally <- matrix(0,people,people)
  for(i in 1:people){
    ego <- dat[i,]
    temp <- vector("integer",people)
    for(j in 1:people){
      alter <- dat[j,]
      temp[j] <- sum(ego&alter)  
    }
    tally[i,] <- temp
  }
  totaltally <- tally*30   #### So totaltally is a matrix with the number of potential times each dyad could have beeped
  
  if(method=="noHH"){  ### this deletes between HH ties
    for(i in 1:people){
      for(j in 1:people){
        if(families[i]==families[j])  totaltally[i,j] <- 0
      }
    }  
  }
  
  
  #####################
  ### (3) 
  ### STRENGTH OF INTERACTIONS BY DYAD - a matrix of the prop of time each dyad spent together
  #####################
  
  ### So, freq is already our number of actual beeps by dyad
  
  if(method=="noHH"){
    for(i in 1:people){
      for(j in 1:people){
        if(families[i]==families[j]) freq[i,j] <- 0
      }
    }
  }
  
  ## this will give us the prop of potential beeps that were beeped by dyad
  props <- freq/totaltally
  if(method=="noHH"){
    for(i in 1:people){
      for(j in 1:people){
        if(families[i]==families[j]) props[i,j] <- 0  
      }
    }
  }
  
# if in same family or not
  
  family <- freq 
  for(i in 1:people){
    for(j in 1:people){
      if(families[i]!=families[j]) family[i,j] <- 0 else family[i,j] <- 1
    }
  }

# if in same cluster or not

cluster <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(cmembership[i]!=cmembership[j]) cluster[i,j] <- 0 else cluster[i,j] <- 1
  }
}

# if in same parental cluster or not (adults aged 15 >)

pcluster <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(pmembership[i]!=pmembership[j]) pcluster[i,j] <- 0 else pcluster[i,j] <- 1
  }
}

# if in same parental cluster or not (adults aged 11 >)

pcluster11 <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(pmembership11[i]!=pmembership11[j]) pcluster11[i,j] <- 0 else pcluster11[i,j] <- 1
  }
}

# if in same sex or not 

samesex <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(sex[i]!=sex[j]) samesex[i,j] <- 0 else samesex[i,j] <- 1
  }
}

# if  kin or not 
kinship <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(("Brother" %in% allcats[i,j + 1]) | ("Sister" %in% allcats[i,j+ 1]) | ("Mother" %in% allcats[i,j+ 1]) | ("Cousins" %in% allcats[i,j+ 1]) | ("OtherKin" %in% allcats[i,j+ 1]) 
       | ("Daughter" %in% allcats[i,j+ 1]) | ("Niece/Neph" %in% allcats[i,j+ 1])  
       | ("Son" %in% allcats[i,j+ 1]) | ("Gkid" %in% allcats[i,j+ 1])  | ("Father" %in% allcats[i,j+ 1])) kinship [i,j] <- 1 else kinship [i,j] <- 0
  }
}

# if affinal kin or not 
affinal <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(("Brother-in-law" %in% allcats[i,j + 1]) | ("Sister-in-law" %in% allcats[i,j+ 1]) | ("Mother-in-law" %in% allcats[i,j+ 1]) 
    | ("Father-in-law" %in% allcats[i,j+ 1])) affinal [i,j] <- 1 else affinal [i,j] <- 0
    }
}

# if maternal kin or not 
mat <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(("MGM" %in% categories[i,j + 1]) | ("MGF" %in% categories[i,j + 1])) mat [i,j] <- 1 else mat [i,j] <- 0
  }
}

# if pat kin or not 
pat <- freq 
for(i in 1:people){
  for(j in 1:people){
    if(("PGM" %in% categories[i,j + 1]) | ("PGF" %in% categories[i,j + 1])) pat [i,j] <- 1 else pat [i,j] <- 0
  }
}


  
  #####################
  ### (<3) 
  ### Childcare data Analysis by Wabasi.
  #####################
  
  # create empty dataframe
  childcare = NULL
  
  # row index
  k = 0
  
  # fill the matrix with values
  for (i in 1:people)
  {
    for(j in 1:people)
    {
      # only select if carer is older, more than 6 and kid less than 11
      if ((age[j] <= 11) & (age[i] >= 6) & (age[i]>age[j]))
      {
        k = k+1
        # categorie +1 as people ID is not ignored by R
        childcare = rbind(childcare, data.frame(ref[i], household[i], camp[i], name[i], age[i], sex[i], role[i], name[j], ref[j], age[j], household[j], sex[j],
                                                kinmat[i,j], categories[i,j+1], props1[i,j], props[i,j], freq[i,j], family[i,j], depend[i], carer[i], depend[j], carer[j], cluster[i,j]
                                                , cmembership[i], cmembership [j], pcluster[i,j], pcluster11[i,j], allcats[i,j+1], affinal[i,j], mat[i,j], pat[i,j], samesex[i,j], 
                                                infant[i], infant[j], kinship[i,j]))
      }
    }
  }

  output <- childcare
  
  if(camps[xcamp]=="Djibbut") output_Djibbut <- output; 
  if(camps[xcamp]=="Cemento") output_Cemento <- output; 
  if(camps[xcamp]=="Dinipan") output_Dinipan <- output; 
  if(camps[xcamp]=="Djago") output_Djago <- output; 
  if(camps[xcamp]=="Dipaguiden") output_Dipaguiden <- output; 
  if(camps[xcamp]=="Didikeg") output_Didikeg <- output; 

  # close main camp loop here.
}

