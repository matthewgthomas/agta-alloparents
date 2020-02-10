# loads packages 
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(plyr)

pclusterallo <- subset(alloparents, pcluster11 == 0)

alloparents$cost5 <- alloparents$cost2


ggplot(alloparents, aes(x=cost2, y=props1, group = pcluster11, colour = pcluster11)) +
  scale_colour_brewer(palette="Set2") + 
  geom_point() + 
  geom_smooth(method = lm, level =0.75, size = 1.5, aes(fill = factor(pcluster11)))    + 
  scale_fill_brewer(palette="Set2") + 
  theme(legend.position="none")  + 
  xlab("Cost") +
  ylab("") 
#-----------------------------------------------------------------------------------
# plots for best-fit models 
#-----------------------------------------------------------------------------------
library(readxl)
plots <- read_excel("data/plot results.xlsx", col_names = FALSE)

# rename - Beta is full model, beta1 is univariable models, beta2 is full model without rec and beta3 is 
#full model without cost 

colnames(plots) <- c("Variable", "factor", "set", "Beta", "SE", "CIl", "CIu", 
                     "Beta1", "CIl1", "CIu1", "Beta2", "CIl2", "CIu2", "Beta3", "CIl3", "CIu3")
plots$Beta2 <- as.numeric(plots$Beta2)
plots$CIl2 <- as.numeric(plots$CIl2)
plots$CIu2 <- as.numeric(plots$CIu2)
plots$Beta3 <- as.numeric(plots$Beta3)
plots$CIl3 <- as.numeric(plots$CIl3)
plots$CIu3 <- as.numeric(plots$CIu3)
plots$set <- as.factor(plots$set)

# PLOT 
ggplot(plots, aes(x=as.factor(Variable), y=Beta1, colour=as.factor(factor), shape=as.factor(set))) + 
  geom_errorbar(aes(ymin=Beta1-CIl1, ymax = Beta1 + CIu1), width=.25, size = 1) + 
  geom_point(size = 7) +
  xlab ("Variable") +
  ylab ("Standardised Beta") + 
  theme(axis.title.y=element_text(size = 16)) + 
  theme(axis.title.x=element_text(size = 16))  + 
  theme(axis.text.y=element_text(size = 16)) + 
  theme(axis.text.x=element_text(size = 16))  + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="none") +
  coord_cartesian(ylim=c(-2.25, 1)) + 
  scale_y_continuous(breaks=seq(-2.25, 1, 0.25)) 


###############################################
#### need, cost and recpoecity by kinship  #### 

p1<- ggplot(pclusterallo, aes(x=cost2, y=props1, group = kinsrole, colour = kinsrole)) +
  scale_colour_brewer(palette="Set2") + 
  #geom_point() + 
  geom_smooth(method = lm, level =0.75, size = 1.5, aes(fill = factor(kinsrole)))    + 
  scale_fill_brewer(palette="Set2") + 
  theme(legend.position="none")  + 
  xlab("Cost") +
  ylab("") + 
  theme(axis.title.x = element_text(size = 16)) + 
  coord_cartesian(ylim=c(-2, 7)) + 
  scale_y_continuous(breaks=seq(-2, 7, 1)) + 
  coord_cartesian(xlim=c(0, 2.5)) + 
  scale_x_continuous(breaks=seq(0, 2.5, 0.25))
 

p2 <- ggplot(alloparents, aes(x=need, y=props1, group = kinsrole, colour = kinsrole)) +
  scale_colour_brewer(palette="Set2") + 
  geom_smooth(method=lm, level =0.95, size = 1.5, aes(fill = factor(kinsrole))) + 
  scale_fill_brewer(palette="Set2") + 
  theme(legend.position="none") + 
  xlab("Need") +
  ylab("Hourly Interactions") + 
  theme(axis.title.x = element_text(size = 16)) + 
  theme(axis.title.y = element_text(size = 16))  + 
  coord_cartesian(ylim=c(0, 10)) + 
  scale_y_continuous(breaks=seq(0, 10, 1)) + 
  coord_cartesian(xlim=c(0.5, 5)) + 
  scale_x_continuous(breaks=seq(0.5, 5, 0.5))
  

p3 <- ggplot(alloparents, aes(x=contigencybeeps, y=props1, group = kinsrole, colour = kinsrole)) +
  scale_colour_brewer(palette="Set2") + 
  geom_smooth(method=lm, level =0.75, size = 1.5, aes(fill = factor(kinsrole))) + 
  scale_fill_brewer(palette="Set2") + 
  theme(legend.position="none") + 
  xlab("Reciprocity") +
  ylab("") + 
  theme(axis.title.x = element_text(size = 16)) +
  coord_cartesian(ylim=c(0, 8)) + 
  scale_y_continuous(breaks=seq(0, 8, 1)) + 
  coord_cartesian(xlim=c(0, 55)) + 
  scale_x_continuous(breaks=seq(0, 55, 5))
  


plot_grid(p1, p2, p3, labels=c("A", "B", "C"), nrow = 3)


########## AGE r AND RECEPCOITY ######## 

p1 <- ggplot(alloparents, aes(x=r, y=props1, group = age, colour = age)) +
   scale_colour_brewer(palette="Dark2") + 
  geom_smooth(method=lm, level =0.75, size = 1.5, aes(fill = factor(age)))    + 
  scale_fill_brewer(palette="Dark2") + 
  theme(legend.position="none") + 
  xlab("Coefficient of relatedness") +
  ylab("Hourly Interactions") + 
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  coord_cartesian(ylim=c(0, 5)) + 
  scale_y_continuous(breaks=seq(0, 5, 0.5)) + 
  coord_cartesian(xlim=c(0, 0.5)) + 
  scale_x_continuous(breaks=seq(0, 0.5, 0.1))
 


p2 <- ggplot(alloparents, aes(x=contigencybeeps, y=props1, group = age, colour = age)) +
  scale_colour_brewer(palette="Dark2") + 
  geom_smooth(method=lm, level =0.75, size = 1.5, aes(fill = factor(age)))    + 
  scale_fill_brewer(palette="Dark2") + 
  theme(legend.position="none") + 
  xlab("Reciprocity") +
  ylab("") + 
  theme(axis.title.x = element_text(size = 16)) +
  coord_cartesian(ylim=c(0, 5)) + 
  scale_y_continuous(breaks=seq(0, 5, 0.5)) + 
  coord_cartesian(xlim=c(0, 55)) + 
  scale_x_continuous(breaks=seq(0, 55, 10))
  

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2)

#-----------------------------------------------------------------------------------
# produce a correlation plot 
#-----------------------------------------------------------------------------------
library(corrplot)
# keep only important variables
myvars <- c("r", "contigencybeeps2", "cost4", "need", "samecluster")
corr <- childcare5[myvars]
names(corr)[names(corr) == "contigencybeeps2"] <- "Household reciprocity"
names(corr)[names(corr) == "cost4"] <- "Giver cost"
names(corr)[names(corr) == "need"] <- "Household Need"
names(corr)[names(corr) == "r"] <- "R"
names(corr)[names(corr) == "samecluster"] <- "Proximity"
corrna <- na.omit(corr)
M <- cor(corrna)
par(mfrow=c(1,1))

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(corrna, 0.95)
res2 <- cor.mtest(corrna, 0.99)
## specialized the insignificant value according to the significant level
library(RColorBrewer)
p1 <- corrplot2(M, p.mat = res1[[1]], insig = "p-value", sig.level = -1, 
                type = "lower", method = "color", tl.pos = "d",
                col = brewer.pal(n = 8, name = "PuOr"),
                order = "hclust", tl.col = "black", tl.cex= 1, cl.cex = 1, 
                cl.ratio=0.1, cl.length = 5)