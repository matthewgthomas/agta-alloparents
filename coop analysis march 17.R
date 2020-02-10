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
require(ggplot2)
require(cowplot)
require(RColorBrewer)

#### GREYED OUT AS ONLY NEED TO LOAD THE FILE AT THE END UNLESS REDOING ANYTHING WITH THE 
#### ORIGINALLY CHILDCARE FILE. 

##### LOAD DATA - use script in 'dataframe creation march 17' to make the df
#childcare <- read.csv("~/Dropbox/document/PhD/Project/2016/reciprocity/reciprocity July 16/data/childcaremarch17.csv")

##### subset data #### 
# all carers expect mothers and fathers 
#alloparents <-  childcare [ -which(childcare$cat == "Son" | childcare$cat =="Daughter"), ]

##### save dF #### 
# once done write file for future use - this file is ready to go! 
#alloparentsjuly17 <- read_csv("~/Dropbox/document/PhD/Project/2016/reciprocity/reciprocity July 16/data/alloparentsjuly17.csv")

##### subset for age difference above 2 years 

#allonew <- subset(alloparentsjuly17, agediff > 1.85)

##### take only variables needed
#myvars <- c("props1", "ref", "childref", "childage", "childsex", "r", "contigencybeeps2", "cost2",
 #           "need", "samecluster", "learn", "signal", "place")
#alloparentsready <- allonew[myvars]

###### write alloparentsready to file for data release 
#write.csv(alloparentsready,file="~/Dropbox/document/PhD/Project/2016/reciprocity/reciprocity July 16/data/alloparentsready.csv")

#### LOAD DF FOR ANALYSIS #### 
alloparentsready <- read.csv("../alloparentsjuly17.csv")

#-----------------------------------------------------------------------------------------------
# 1. run full models 
#-----------------------------------------------------------------------------------------------
# relatedness model 
fita <- lmer (props1 ~ childage  + childsex  + r  + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary (standfit)
confint(standfit)

# contigency model 
fita <- lmer (props1 ~  childage  + childsex + contigencybeeps2 + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
confint(standfit1)

#cost model 
fita <- lmer (props1 ~ childage  + childsex  + cost2  + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
confint(standfit2)

#need model 
fita <- lmer (props1 ~ childage  + childsex  + need + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit3 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit3)
confint(standfit3)

# proximity/assoication 
fita <- lmer (props1 ~ childage  + childsex  + samecluster + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit4 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit4)
confint(standfit4)

#learning to mother model 
fita <- lmer (props1 ~ childage  + childsex + learn + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit5 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit5)
confint(standfit5)

# costly signalling
fita <- lmer (props1 ~ childage  + childsex  + signal + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit6 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit6)
confint(standfit6)


# full model 
fita <- lmer (props1 ~ childage  + childsex  + r  + contigencybeeps2  + cost2 + need + samecluster + 
                learn + signal + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
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
                learn + signal, data = alloparentsready)
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
# 2. run interaction models 
#-----------------------------------------------------------------------------------------------
# run each model for cost, need and recprocity, standardise, and plot 
# Create place varibale based on different cut-offs 
# with cut off for non-kin at < 0.0313
alloparentsready$place[alloparentsready$kinrole3 == 1] <- "1"
alloparentsready$place[alloparentsready$distantkinrole3 == 1] <- "2"
alloparentsready$place[alloparentsready$nonkinrole3 == 1] <- "3"

alloparentsready$place <- as.factor(alloparentsready$place)

require(effects)

#### NEED ##### 
fita <- lmer (props1 ~ + 
                need*place + (1 | ref) 
              + (1 | childref), data = alloparentsready, REML = FALSE)
summary(fita)
standfit1 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit1)
#confint(standfit1)

# make df 
tmp1 <- as.data.frame(effect("need:place", fita, list(place=c(3,2,1))))
# make CI
tmp1$ci <- tmp1$fit - tmp1$lower

# make plot 
p1 <- ggplot(data=tmp1, aes(x=need, y=fit, colour=as.factor(place))) +
  labs(colour="place") + 
  theme(legend.position="none") + 
  geom_ribbon(aes(ymin=fit-ci, ymax = fit+ci, linetype=NA), alpha =0.2) +
  stat_smooth(method = "lm", size = 3) +
   scale_colour_discrete  (name="Role",
                       breaks=c( "3", "2", "1"),
                      labels=c( "Non kin", "Distant kin", "Close kin")) + 
  xlab("Reciever need") + 
  ylab("Average hourly interactions") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
 # theme(legend.title = element_blank()) + 
  # theme(legend.text = element_text(size = 12)) + 
  coord_cartesian(ylim=c(0, 8)) + 
  scale_y_continuous(breaks=seq(0, 8, 1)) +
  coord_cartesian(xlim=c(0.8, 5)) + 
  scale_x_continuous(breaks=seq(1, 5, 0.5))


#### REC ##### 
fita <- lmer (props1 ~  
                + contigencybeeps2*place + (1 | ref) 
              + (1 | childref), data = alloparentsready, REML = FALSE)
summary(fita)
standfit2 <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit2)
#confint(standfit2)

# make df
tmp <- as.data.frame(effect("contigencybeeps2:place", fita, list(place=c(3,2,1))))
# make CI
tmp$ci <- tmp$fit - tmp$lower

p2 <-  ggplot(data=tmp, aes(x=contigencybeeps2, y=fit, colour=as.factor(place))) +
  labs(colour="place") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  geom_ribbon(aes(ymin=fit-ci, ymax = fit+ci, linetype=NA), alpha =0.2) +
  stat_smooth(method = "lm", size = 3) +
   scale_colour_discrete(name="Role",
                       breaks=c( "3", "2", "1"),
                      labels=c( "Non kin", "Distant kin", "Close kin")) + 
  xlab("Reciprocity") + 
  ylab(NULL) + 
  theme(legend.position="none") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
  # theme(legend.title = element_blank()) + 
   #theme(legend.text = element_text(size = 12)) + 
  coord_cartesian(ylim=c(0, 8)) + 
  scale_y_continuous(breaks=seq(0, 8, 1)) + 
  coord_cartesian(xlim=c(-0.5, 70)) + 
  scale_x_continuous(breaks=seq(0, 70, 10))

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
plot_grid(p1, p2, labels=c("a", "b"), nrow = 1, align = "H")

# compare models 
sem.model.fits(c(standfit,standfit1, standfit2), aicc = FALSE)

#Class   Family     Link    n  Marginal Conditional      AIC     dAIC
#COST  gaussian identity 1946 0.1966908   0.4370209 7851.769 1365.852
#NEED  gaussian identity 1946 0.1909908   0.4281451 7840.001 1354.084
#REC   gaussian identity 1812 0.2133229   0.5644360 6485.917    0.000


##### COST #####
fita <- lmer (props1 ~ cost2*place + (1 | ref) + (1 | childref), data = alloparentsready, REML = FALSE)
standfit <- standardize(fita, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit)
confint(standfit)

# plot 
require(cowplot)
tmp <- as.data.frame(effect("cost2:place", fita, list(place=c(3,2,1))))
p1 <- ggplot(data=tmp, aes(x=cost2, y=fit, colour=as.factor(place))) +
  labs(colour="place") + 
  theme(legend.position="none") + 
  stat_smooth(method = "lm", size = 3) +
  scale_colour_discrete  (name="Relatedness",
                          breaks=c( "3", "2", "1"),
                          labels=c( "Non-kin", "Distant kin", "Close kin")) + 
  xlab("Giver Cost") + 
  ylab("Number of hourly interactions") + 
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=12)) +
  #theme(legend.title = element_blank(size=15, face="bold")) + 
  #theme(legend.text = element_text(size = 12)) + 
  coord_cartesian(ylim=c(-1.5, 5)) + 
  scale_y_continuous(breaks=seq(-1.5, 5, 0.5))

#-----------------------------------------------------------------------------------------------
# 3. EXAMINE DIFFERENCES IN DATA SETS 
#-----------------------------------------------------------------------------------------------
# are there significantly more juveniles in different kin groups 
# seperate into age groups for children 


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
