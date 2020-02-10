library(tidyverse)
library(lme4)

alloparentsready <- read.csv("../alloparentsjuly17.csv")

names(alloparentsready)

# props1 is average rate of interactions per hour ()

alloparentsready %>% 
  filter(props1 > 0) %>% 
  ggplot(aes(x=props1)) + geom_histogram(binwidth = 0.1)

summary(alloparentsready$props1)

# how many zeros?
alloparentsready %>% 
  filter(props1 == 0) %>% 
  count()

# the minimum of the non-zero values is 0.01010101

# fit Poisson model
fita <- glmer(freq ~ childage  + childsex  + r + cost2 + need + samecluster + learn + signal + 
                (1 | ref) + (1 | childref) + (1 | childhouse) + (1 | giverhouse), 
              data = alloparentsready, family = poisson)
