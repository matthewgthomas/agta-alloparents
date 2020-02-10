ggplot(childcare5, aes(x=contigencybeeps2, y=cost2)) + 
  geom_point(alpha=.4) + 
  stat_smooth(method=loess) 

# make residuals of contigency and freq to explore 
myvars <- c("freq", "cost4","r", "contigencybeeps2", "hours2",
            "need")
childcarena <- childcare5[myvars]

childcarena <- na.omit(childcarena) 

fita <- glm (freq  ~ contigencybeeps2 + hours2, data = childcarena, family = poisson(log))
summary(fita)

childcarena$CBresids <- resid(fita)

fita <- lm (CBresids  ~ need + r + cost4, data = childcarena)
summary(fita)

ggplot(childcarena, aes(x=contigencybeeps2, y=CBresids)) + 
  geom_point(alpha=.4) + 
  stat_smooth(method=loess) 

# make a recirpocity vaule which is contigeency - freq 

childcare5$cont <- (childcare5$freq-childcare5$contigencybeeps2)
fita <- lm (cont  ~ need + r + cost4, data = childcare5)
summary(fita)

#thresholds as 0 is balance, - is give more and + is give less 
myvars <- c("freq", "cost4","r", "contigencybeeps2", "cont",
            "need")
childcarena <- childcare5[myvars]

childcarena <- na.omit(childcarena) 

childcarena$contneg <- ifelse(childcarena$cont < 0, c(1), c(0)) 
childcarena$contbal <- ifelse(childcarena$cont >= 0 & childcarena$cont < 50, c(1), c(0))
childcarena$contpos <- ifelse(childcarena$cont >= 50, c(1), c(0))

childcarena$contgroup[childcarena$contneg == 1] <- 1
childcarena$contgroup[childcarena$contbal == 1] <- 2
childcarena$contgroup[childcarena$contpos == 1] <- 3

fita <- glm (need  ~ contpos + contbal, data = childcarena)
summary(fita)

fita <- glm (cost4  ~ contpos + contbal, data = childcarena)
summary(fita)

fita <- glm (r  ~ contneg + contbal, data = childcarena)
summary(fita)

fita <- lm (contigencybeeps2  ~ need + r + cost4, data = childcare5)
summary(fita)

# produce a correlation plot 
library(corrplot)
# keep only important variables
myvars <- c("r", "CBobs", "cost4", "need", "samecluster")
corr <- childcare5[myvars]
names(corr)[names(corr) == "CBobs"] <- "Household reciprocity"
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