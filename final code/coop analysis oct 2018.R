#-----------------------------------------------------------------------------------------------
### CODE FOR ANALYSIS OF MOTES DATA
### WRITTEN AEP MARCH 2017
#-----------------------------------------------------------------------------------------------
################################################################################################

#### load packages ###### 
library(lme4)
library(lattice)
library (ggplot2)
library (car)
library (nlme)
library (rms)
require(lm.beta)
require(arm)
require(piecewiseSEM)
require(cowplot)
library(readr)
require(RColorBrewer)
require(plyr)
require(AICcmodavg)
require(effect)
require(vcov)
require(effects)

##### LOAD DATA - use script in 'dataframe creation step 1 and 2' to make the df
# this dataframe has both freq and props from 1.5 and 3 meters and includes all reciprocity variables 
# from motes and obs

childcare <- read_csv("data/childcare24Sept18.csv")# THIS FILE IS ALREADY SUBSET TO ONLY ALLOPARENTS 

##### subset data #### 
# all carers expect mothers and fathers 
#childcare <-  childcare [ -which(childcare$cat == "Son" | childcare$cat =="Daughter"), ] 

# costly signal variable 
# if male of reproductive age 
childcare$signal <- ifelse(childcare$giversex == 0 & childcare$currentreprod == TRUE, 
                           c("1"), c("0")) 

# Create place varibale based on different kin cut-offs 
# with cut off for non-kin at < 0.0313 (kinrole4 is = 0)

childcare$place[childcare$kinrole3 == 1] <- "1"
childcare$place[childcare$distantkinrole3 == 1] <- "2"
childcare$place[childcare$nonkinrole3 == 1] <- "3"

childcare$place <- as.factor(childcare$place)

#### variable of agegroup to run interaction with agediff
childcare$adult <- ifelse(childcare$giverage > 20, c("1"), c("0")) 
childcare$youngadult <- ifelse(childcare$giverage >= 10 & childcare$giverage <= 20, c("1"), c("0")) 
childcare$child <- ifelse(childcare$giverage < 10, c("1"), c("0"))

childcare$agegroup[childcare$child == 1] <- "1"
childcare$agegroup[childcare$youngadult == 1] <- "2"
childcare$agegroup[childcare$adult == 1] <- "3"


##### take only variables needed
myvars <- c("ref", "childref", "freq", "props1", "childage", "childsex", "r", "cost2","CBobs", "contigencybeeps", "contigencybeeps2", "place", "props3met", "hours", "camp", "hours2",
            "need", "samecluster", "learn", "signal", "location", "giverhouse", "childhouse", "agediff", "kinrole3", "distantkinrole3", "nonkinrole3", "giverage", "childage")
childcare <- childcare[myvars]

### ###### write alloparentsready to file for data release 
#write.csv(childcare,file="~/Dropbox/Work/reciprocity/Cooperation 17/data/datarelease/childcare.csv")

#-----------------------------------------------------------------------------------------------
# 1. run full models - models with freq data (no contingency), poission distribution
#-----------------------------------------------------------------------------------------------
# check dataset name as will vary for sensitivty analysis for household effect and 
# age difference of allo and child. 

# relatedness model 
fita <- glmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + r + (1 | ref) + (1 | childref) 
               + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log), correlation=corAR1())
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center") 
summary (standfit1)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r + hours2 + agediff*agegroup + (1 | ref) + (1 | childref)
                     + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit1,method="Wald")

# rec with motes
fita <- glmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + contigencybeeps2  + (1 | ref) + 
                 (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
rsquared(lme4::glmer(freq ~ childage  + childsex  + contigencybeeps2  + hours2 + agediff*agegroup + (1 | ref) + 
                       (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit2,method="Wald")


# rec with obs
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + CBobs + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit33 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit33)
confint(standfit33,method="Wald")


#cost model 
fita <- glmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + cost4  +  (1 | ref) + (1 | childref) + 
                (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)
rsquared(lme4::glmer(freq ~ childage  + childsex  + cost4  + hours2 + agediff*agegroup + (1 | ref) + (1 | childref) + 
                       (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit3,method="Wald")


#need model 
fita <- lmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + need + (1 | ref) + (1 | childref) 
              + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit4 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit4)
rsquared(lme4::glmer(freq ~ childage  + childsex  + need + hours2 + agediff*agegroup + (1 | ref) + 
                       (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit4,method="Wald")


# proximity/assoication 
fita <- glmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + samecluster + (1 | ref) + (1 | childref) 
               + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit5 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit5)
rsquared(lme4::glmer(freq ~ childage  + childsex + samecluster + agediff*agegroup + hours2 + (1 | ref) + (1 | childref) + 
                       (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit5,method="Wald")


#learning to mother model 
fita <- glmer (freq ~ hours2 + childage  + childsex + agediff*agegroup + learn + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit6 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit6)
rsquared(lme4::glmer(freq ~ childage  + childsex  + learn + agediff*agegroup + hours2 + (1 | ref) + (1 | childref)
                     + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit6,method="Wald")


# costly signalling
fita <- glmer (freq ~ hours2 + childage  + childsex  + agediff*agegroup + signal + (1 | ref) + (1 | childref) 
               + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson)
standfit7 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit7)
rsquared(lme4::glmer(freq ~ childage  + childsex  + signal +  agediff*agegroup  + hours2 + (1 | ref) + (1 | childref) + 
                       (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit7,method="Wald")


# full model 
fita <- glmer (freq ~ hours2 + childage  + childsex   + agediff*agegroup   + r + contigencybeeps2 + cost4 + need + samecluster  + 
                 learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 + (1 | childhouse), data = childcare5, 
               family = poisson(log))
standfit8 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit8)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + cost4 + need + samecluster + contigencybeeps2 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit8,method="Wald")

# full model without r 
fita <- glmer (freq ~ hours2 + childage  + childsex   + agediff*agegroup   + contigencybeeps2 + cost4 + need + samecluster  + 
                learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, 
               family = poisson(log))
standfit11 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit11)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + cost4 + need + samecluster + contigencybeeps2 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit11,method="Wald")



# full model without rec
fita <- glmer (freq ~ hours2 + childage  + childsex   + agediff*agegroup + r + cost4 + need + samecluster  + 
                 learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, 
               family = poisson(log))
standfit9 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit9)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + cost4 + need + samecluster + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit10,method="Wald")

# full model without cost 
fita <- glmer (freq ~ hours2 + childage  + childsex   + agediff*agegroup + r + contigencybeeps2 + need + samecluster  + 
                 learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, 
               family = poisson(log))
standfit10 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit10)
rsquared(lme4::glmer(freq ~ childage  + childsex  + r  + need + samecluster + contigencybeeps2 + agediff*agegroup +  hours2  + 
                       learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), 
                     data = childcare5, family = poisson), method = "delta")
confint(standfit10,method="Wald")

# tests

nonhousehold <- subset(childcare5, samehouse == 0)

fita <- glmer (freq ~ hours2 + childage  + childsex   + agediff*agegroup   + r + samecluster + carer + 
                 depend + ecost + need   + contigencybeeps2 + 
                 learn + signal + (1 | ref) + (1 | childref) + (1 | giverhouse) + 
                 + (1 | childhouse) + (1 | camp), data = nonhousehold, 
               family = poisson(log))
standfit8 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit8)



# create AIC

aictab(cand.set = c(standfit8,  standfit9, standfit10, standfit11), sort = FALSE)


# check residuals - however following the limit therom no need to be concerned. 
hist((resid(fita) - mean(resid(fita))) / sd(resid(fita)), freq = FALSE); curve(dnorm, add = TRUE)
qqnorm(resid(fita))
qqline(resid(fita))
shapiro.test(resid(fita))

# check vif
fita <- lm (freq ~ childage  + childsex  + r  + contigencybeeps2  + cost2 + need + samecluster + hours2 + agediff + agegroup +
              learn + signal, data = childcare5)
vif(fita)

# create AIC

aictab(cand.set = c(standfit1,  standfit2, standfit3, standfit4, standfit5, 
                    standfit6, standfit7, standfit9), sort = TRUE)

# plot autocorrelation 
par(mfrow=c(2,1))
# raw freq
acf(childcare5$freq, lag = 2000) 
#residuals 
acf(resid(fita), main="acf(resid(fita))", lag = 2000) 


#-----------------------------------------------------------------------------------------------
# 2. run full models - models with prop data 
#-----------------------------------------------------------------------------------------------
# check dataset name as will vary for sensitivty analysis for household effect and 
# age difference of allo and child.

# relatedness model 
fita <- lmer (props1 ~ childage  + childsex  + r  + hours + (1 | ref) + (1 | childref) + (1 | camp), data = childcare, REML = FALSE)
standfit <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary (standfit)
confint(standfit)

# contigency model with obs 
fita <- lmer (props1 ~  childage  + childsex + CBobs + (1 | ref) + (1 | childref), data = childcare, REML = FALSE)
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
confint(standfit1)

# contigency model with motes 
fita <- lmer (props1 ~  childage  + childsex + contigencybeeps2 + (1 | ref) + (1 | childref), data = childcare, REML = FALSE)
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
confint(standfit1)

#cost model 
fita <- lmer (props1 ~ childage  + childsex  + cost2  + (1 | ref) + (1 | childref), data = childcare, REML = FALSE)
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
confint(standfit2)

#need model 
fita <- lmer (props1 ~ childage  + childsex  + need + (1 | ref) + (1 | childref), data = childcare, REML = FALSE)
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)
confint(standfit3)

# proximity/assoication 
fita <- lmer (props1 ~ childage  + childsex  + samecluster + (1 | ref) + (1 | childref), data = allofar, REML = FALSE)
standfit4 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit4)
confint(standfit4)

#learning to mother model 
fita <- lmer (props1 ~ childage  + childsex + learn + (1 | ref) + (1 | childref), data = allofar, REML = FALSE)
standfit5 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit5)
confint(standfit5)

# costly signalling
fita <- lmer (props1 ~ childage  + childsex  + signal + (1 | ref) + (1 | childref), data = allofar, REML = FALSE)
standfit6 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit6)
confint(standfit6)

# full model with contingency motes
fita <- lmer (props1 ~ childage  + childsex  + r  + contigencybeeps2  + cost2 + need + samecluster + 
                learn + signal + agediff + (1 | ref) + (1 | childref) + (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare, REML = FALSE)
standfit7 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit7)
confint(standfit7)


# check residuals - however following the limit therom no need to be concerned. 
hist((resid(fita) - mean(resid(fita))) / sd(resid(fita)), freq = FALSE); curve(dnorm, add = TRUE)
qqnorm(resid(fita))
qqline(resid(fita))
shapiro.test(resid(fita))

# check vif
fita <- lm (props1 ~ childage  + childsex  + r  + contigencybeeps2  + cost2 + need + samecluster + 
                learn + signal, data = allofar)
vif(fita)

#childage         childsex                r contigencybeeps2            cost2             need 
#1.097364         1.088587         1.098044         1.185581         1.100885         1.187836 
#samecluster            learn           signal 
#1.115732         1.196673         1.149825 

# create AIC and effect sizes 
sem.model.fits(c(standfit,standfit1,  standfit2, standfit3, standfit4, standfit5, 
                 standfit6, standfit7), aicc = FALSE)

#Class   Family     Link    n    Marginal Conditional      AIC       dAIC
#R        gaussian identity 1946 0.178485574   0.4299336 7830.635 1391.70130
#REC      gaussian identity 1812 0.193118001   0.5321138 6520.828   81.89395
#COST     gaussian identity 1946 0.293997967   0.6375856 8114.586 1675.65242
#NEED     gaussian identity 1946 0.020243976   0.2637275 8247.220 1808.28623
#PROX     gaussian identity 1946 0.096050323   0.3388487 8071.949 1633.01540
#LEARN    gaussian identity 1946 0.020277350   0.2626268 8244.339 1805.40499
#COSTLY   gaussian identity 1946 0.009609693   0.2634684 8252.863 1813.92910
#FULL     gaussian identity 1812 0.224381573   0.5299461 6438.934    0.00000

#-----------------------------------------------------------------------------------------------
# 3. run interaction models 
#-----------------------------------------------------------------------------------------------
# run each model for cost, need and recprocity, standardise, and plot 

#### kinship variables #### 
# with cut off for non-kin at 0.125
childcare5$kinrole <- ifelse(childcare5$r > 0.25, c(1), c(0)) 
childcare5$distantkinrole <- ifelse(childcare5$r >= 0.125 & childcare5$r <= 0.25, c(1), c(0)) 
childcare5$nonkinrole <- ifelse(childcare5$r < 0.125, c(1), c(0))

# with cut off for non-kin at 0.0625
childcare5$kinrole2 <- ifelse(childcare5$r > 0.25, c(1), c(0)) 
childcare5$distantkinrole2 <- ifelse(childcare5$r >= 0.0625 & childcare5$r <= 0.25, c(1), c(0)) 
childcare5$nonkinrole2 <- ifelse(childcare5$r < 0.0625, c(1), c(0))

# with cut off for non-kin at 0.0313
childcare5$kinrole3 <- ifelse(childcare5$r > 0.25, c(1), c(0)) 
childcare5$distantkinrole3 <- ifelse(childcare5$r >= 0.03125 & childcare5$r <= 0.25, c(1), c(0)) 
childcare5$nonkinrole3 <- ifelse(childcare5$r < 0.03125, c(1), c(0))

# with cut off for non-kin at 0
childcare5$kinrole4 <- ifelse(childcare5$r > 0.25, c(1), c(0)) 
childcare5$distantkinrole4 <- ifelse(childcare5$r > 0 & childcare5$r <= 0.25, c(1), c(0)) 
childcare5$nonkinrole4 <- ifelse(childcare5$r == 0, c(1), c(0))

#one variable for kinship
childcare5$place[childcare5$kinrole3 == 1] <- 1
childcare5$place[childcare5$distantkinrole3 == 1] <- 2
childcare5$place[childcare5$nonkinrole3 == 1] <- 3

#### NEED ##### 
fita <- glmer (freq ~ hours2 +  need*place +
                (1 | ref) + (1 | childref) +  
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
confint(standfit1,method="Wald")

# make df 
tmp1 <- as.data.frame(effect("need:place", fita, xlevels=list(place=c(3,2,1))))
# make CI
tmp1$ci <- tmp1$fit - tmp1$lower

# make plot 
p1 <- ggplot(data=tmp1, aes(x=need, y=fit, colour=as.factor(place), fill = place)) +
  labs(colour="place") + 
  theme(legend.position="none") + 
  #geom_ribbon(aes(ymin=fit-ci, ymax = fit+ci, linetype=NA), alpha =0.2) +
  stat_smooth(method = "lm", size = 3, level = 0.75, alpha = 0.2) +
   scale_colour_discrete  (name="Role",
                       breaks=c( "3", "2", "1"),
                      labels=c( "Non kin", "Distant kin", "Close kin")) + 
  xlab("Reciever need") + 
  ylab("Total interactions") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
  theme(legend.title = element_blank()) + 
   theme(legend.text = element_text(size = 12)) + 
  coord_cartesian(ylim=c(-10, 375)) + 
  scale_y_continuous(breaks=seq(-25, 400, 50)) +
  coord_cartesian(xlim=c(0.8, 5)) + 
  scale_x_continuous(breaks=seq(1, 5, 0.5))

#### COST ##### 

fita <- glmer (freq ~ hours2 + cost4*place + (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
confint(standfit2,method="Wald")

# make df
tmp <- as.data.frame(effect("cost4:place", fita, xlevels=list(place=c(3,2,1))))
# make CI
tmp$ci <- tmp$fit - tmp$lower

p2 <-  ggplot(data=tmp, aes(x=cost4, y=fit, colour=as.factor(place), fill = place)) +
  labs(colour="place") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  stat_smooth(method = "lm", size = 3, alpha = 0.2, level = 0.75) +
  scale_colour_discrete(name="Role",
                        breaks=c( "3", "2", "1"),
                        labels=c( "Non kin", "Distant kin", "Close kin")) + 
  xlab("Giver cost") + 
  ylab(NULL) + 
  theme(legend.position="none") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
  coord_cartesian(ylim=c(-25, 100)) + 
  scale_y_continuous(breaks=seq(-25, 100, 25)) 

#### REC ##### 

fita <- glmer (freq ~ hours2 + contigencybeeps2*place +  (1 | ref) + (1 | childref) + 
                 (1 | giverhouse) + (1 | childhouse) + (1 | camp), data = childcare5, family = poisson(log))
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)
confint(standfit3,method="Wald")

# make df
tmp <- as.data.frame(effect("contigencybeeps2:place", fita, xlevels=list(place=c(3,2,1))))
# make CI
tmp$ci <- tmp$fit - tmp$lower
       
p3 <-  ggplot(data=tmp, aes(x=contigencybeeps2, y=fit, colour=as.factor(place), fill = place)) +
  labs(colour="place") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  stat_smooth(method = "lm", size = 3, alpha = 0.2) +
   scale_colour_discrete(name="Role",
                       breaks=c( "3", "2", "1"),
                      labels=c( "Non kin", "Distant kin", "Close kin")) + 
  xlab("Household reciprocity") + 
  ylab(NULL) + 
  theme(legend.position="none") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
  coord_cartesian(ylim=c(0, 50)) + 
  scale_y_continuous(breaks=seq(0, 50, 5)) + 
  coord_cartesian(xlim=c(-0.5, 80)) + 
  scale_x_continuous(breaks=seq(0, 80, 10))

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# combine plots
plot_grid(p1, p2, p3, labels=c("a", "b", "c"), nrow = 1, align = "H")


#-----------------------------------------------------------------------------------------------
# 4. EXAMINE descriptives 
#-----------------------------------------------------------------------------------------------

#Summary data for continuous 
summary(childcare5$hours2)
summary(childcare5$r)
summary(childcare5$freq)
summary(childcare5$need)
summary(childcare5$agediff)
summary(childcare5$contigencybeeps2)
summary(childcare5$cost4)
summary(childcare5$childage)
summary(childcare5$giverage)
summary(childcare5$CBobs)

# and binary
cbind(Freq = table(childcare5$childsex), Perc = prop.table(table(childcare5$childsex)),  
      Cum = cumsum(prop.table(table(childcare5$childsex))))
cbind(Freq = table(childcare5$giversex), Perc = prop.table(table(childcare5$giversex)),  
      Cum = cumsum(prop.table(table(childcare5$giversex))))
cbind(Freq = table(childcare5$samecluster), Perc = prop.table(table(childcare5$samecluster)),  
      Cum = cumsum(prop.table(table(childcare5$samecluster))))
cbind(Freq = table(childcare5$learn), Perc = prop.table(table(childcare5$learn)),  
      Cum = cumsum(prop.table(table(childcare5$learn))))
cbind(Freq = table(childcare5$signal), Perc = prop.table(table(childcare5$signal)),  
      Cum = cumsum(prop.table(table(childcare5$signal))))


# are there significantly more juveniles in different kin groups 
# summary data by kinship 

cbind(Freq = table(closekindf$age), Perc = prop.table(table(closekindf$age)),  
      Cum = cumsum(prop.table(table(closekindf$age))))

# close kin DATASET 
#Freq      Perc       Cum
#0  100 0.7352941 0.7352941
#1   36 0.2647059 1.0000000
100 + 36 
# 136 

cbind(Freq = table(distantkindf$age), Perc = prop.table(table(distantkindf$age)),  
      Cum = cumsum(prop.table(table(distantkindf$age))))


#Freq       Perc       Cum
#0  180 0.37113402 0.3711340
#1  272 0.56082474 0.9319588
#2   33 0.06804124 1.0000000
100+272+33 
# 405
# 372

cbind(Freq = table(nonkindf$age), Perc = prop.table(table(nonkindf$age)),  
      Cum = cumsum(prop.table(table(nonkindf$age))))

#Freq      Perc       Cum
#0  478 0.3356742 0.3356742
#1  840 0.5898876 0.9255618
#2  106 0.0744382 1.0000000
478+840+106
478+840
# 1318 
# 1424 

# propotion test for non-kin and distant kin  
prop.test (c(180, 478), c(405, 1424))
# RESULT 
#2-sample test for equality of proportions with continuity correction

#data:  c(180, 478) out of c(405, 1424)
#X-squared = 15.728, df = 1, p-value = 7.315e-05
#alternative hypothesis: two.sided
#95 percent confidence interval:
# 0.05292997 0.16461061
#sample estimates:
# prop 1    prop 2 
#0.4444444 0.3356742 

# propotion test for close-kin and distant kin  
prop.test (c(100, 478), c(136, 1424))
# RESULT 
# 2-sample test for equality of proportions with continuity correction
#data:  c(100, 478) out of c(136, 1424)
#X-squared = 83.297, df = 1, p-value < 2.2e-16
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  0.3174945 0.4817454
#sample estimates:
#  prop 1    prop 2 
#0.7352941 0.3356742 
