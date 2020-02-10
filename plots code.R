require(ggplot2)
require (cowplot)
library(dplyr)

#-----------------------------------------------------------------------------------
# plots for best-fit models 
#-----------------------------------------------------------------------------------
library(readxl)
plots <- read_excel("~/Dropbox/document/PhD/Project/2016/reciprocity/EL/revisions/plots/plot results.xlsx", 
col_names = FALSE)

# rename 
colnames(plots) <- c("Variable", "factor", "R2m", "R2c", "dAIC", "Beta", "SE")

# BETA #####
ggplot(plots, aes(x=as.factor(Variable), y=Beta, colour=as.factor(factor))) + 
  geom_errorbar(aes(ymin=Beta-SE, ymax = Beta + SE), width=.25, size = 1) + 
  geom_point(size = 7, shape = 17) +
  xlab ("Variable") +
  ylab ("Standardised Beta") + 
  theme(axis.title.y=element_text(size = 16)) + 
  theme(axis.title.x=element_text(size = 16))  + 
  theme(axis.text.y=element_text(size = 16)) + 
  theme(axis.text.x=element_text(size = 16))  + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="none") +
  coord_cartesian(ylim=c(-0.7, 1.6)) + 
  scale_y_continuous(breaks=seq(-0.5, 1.5, 0.25))  

#### for effect size #### 
plotresults <- plotresults %>% arrange(marg)
plotresults$model <- as.vector(plotresults$model) #get rid of factors
plotresults$model = factor(plotresults$model,plotresults$model) #add ordered factors back

ggplot(plotresults, aes(x=as.factor(model), y=marg, colour=as.factor(group))) + 
  geom_point(size = 7, shape = 17) +
  xlab ("Model") +
  ylab ("Marginal R2") + 
  theme(axis.title.y=element_text(size = 16)) + 
  theme(axis.title.x=element_text(size = 16)) + 
  theme(axis.text.y=element_text(size = 16)) + 
  theme(axis.text.x=element_text(size = 16))  + 
  theme(legend.position="none") + 
  coord_cartesian(ylim=c(0, 0.25)) + 
  scale_y_continuous(breaks=seq(0, 0.25, 0.05))  


##### for dAIC ##### 
plotresults <- plotresults %>% arrange(daic)
plotresults$model <- as.vector(plotresults$model) #get rid of factors
plotresults$model = factor(plotresults$model,plotresults$model) #add ordered factors back

ggplot(plotresults, aes(x=as.factor(model), y=daic, colour=as.factor(group))) + 
  geom_point(size = 7, shape = 17) +
  xlab ("Model") +
  ylab ("dAIC") + 
  theme(axis.title.y=element_text(size = 16)) + 
  theme(axis.title.x=element_text(size = 16)) +
  theme(axis.text.y=element_text(size = 16)) + 
  theme(axis.text.x=element_text(size = 16))  + 
  theme(legend.position="none") +
  coord_cartesian(ylim=c(-40, 2040)) + 
  scale_y_continuous(breaks=seq(0, 2000, 250))  




