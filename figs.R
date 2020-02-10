# loads packages 
require(ggplot2)
require(cowplot)
require(RColorBrewer)

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

