
# plot relationship between agediff and freq
ggplot(childcare, aes(x=agediff, y=freq)) + 
  geom_point(alpha=.4) + 
  stat_smooth(method=loess) 

# lm of agediff and freq
fit <- lm(freq ~ agediff + giversex, data=childcare)
summary(fit)

konfound(fit, giversex, alpha = 0.05, tails = 2,
          test_all = FALSE)



# threshold agediff so can see separate effects
childcare$far <- ifelse(childcare$agediff > 40, c("1"), c("0")) 
childcare$medium <- ifelse(childcare$agediff >= 10 & childcare$agediff <= 40, c("1"), c("0")) 
childcare$near <- ifelse(childcare$agediff < 10, c("1"), c("0"))

# lm of agediff and freq as threshold
fit <- lm(freq ~  near + far, data=childcare)
summary(fit)

#subset to look at graph 
agediff <- subset(childcare, agediff <= 10)
agediff <- subset(childcare, agediff > 10 & childcare$agediff <= 40)
agediff <- subset(childcare, agediff > 40)

# plot again
ggplot(agediff, aes(x=agediff, y=freq)) + 
  geom_point(alpha=.4) + 
  stat_smooth(method=loess) 

# variable of age to run interaction
childcare$adult <- ifelse(childcare$giverage > 20, c("1"), c("0")) 
childcare$youngadult <- ifelse(childcare$giverage >= 10 & childcare$giverage <= 20, c("1"), c("0")) 
childcare$child <- ifelse(childcare$giverage < 10, c("1"), c("0"))

childcare$agegroup[childcare$child == 1] <- "1"
childcare$agegroup[childcare$youngadult == 1] <- "2"
childcare$agegroup[childcare$adult == 1] <- "3"

# lm of agediff and freq with interqactino 
fit <- glm(freq ~ agediff*agegroup, data=childcare5, family=poisson(log))
standfit <- standardize(fit, unchanged = NULL, standardize.y = FALSE, binary.inputs = "center")
summary(standfit)