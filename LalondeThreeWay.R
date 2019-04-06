library("Matching")
library("stargazer")
library("ggplot2")
library(randomForest)

data <- data("lalonde")

#MODELS
#PART1
#high school degree and no degree
Degree <- lalonde[lalonde$nodegr ==0,]
NoDegree <- lalonde[lalonde$nodegr ==1,]

#regression model
fit.Degree <- lm(re78 ~ treat + age + black + hisp +married +re74 + re75, data = Degree)
fit.NoDegree <- lm(re78 ~ treat + age + black + hisp +married +re74 + re75, data = NoDegree)

CI.Degree <- confint(fit.Degree)
CI.NoDegree <- confint(fit.NoDegree)

#output image
stargazer(fit.Degree, fit.NoDegree,title="Table 1. Regression Results", type = "html",
          align=TRUE, column.labels = c("Degree", "No Degree"), dep.var.labels= "Real Earnings in 1978",
          covariate.labels=c("Treatment","Age",
                             "Black","Hispanic","Married","Real Earnings in 1974", "Real Earnings in 1975"),
          omit.stat=c("LL","ser","f"), single.row= TRUE, out = "regression.htm")

#confidence intervals
stargazer(CI.Degree, type = "html", title="Degree", out ="CIDegree.htm")
stargazer(CI.NoDegree, type = "html", title="No Degree", out ="CINoDegree.htm")


#correlation matrix
correlation.matrix <- cor(Degree[,c("treat","age","black", "hisp", "married", "re74", "re75")])
stargazer(correlation.matrix, title="Correlation Matrix Degree", out ="corDegree.htm")

correlation.matrix2 <- cor(NoDegree[,c("treat","age","black", "hisp", "married", "re74", "re75")])
stargazer(correlation.matrix, title="Correlation Matrix No Degree", out ="corNoDegree.htm")



#PART2
#random forest
set.seed(1)

#DEGREE
rf.Degree <- randomForest(re78 ~ treat + age + black+ hisp+ married + re74 + re75, data = Degree, mtry = 3, importance = TRUE)
importance(rf.Degree)
varImpPlot(rf.Degree)

#predicting the counterfactual
treated.Degree <- Degree[Degree$treat == 1,]
treated.Degree$treat = 0  #change the treated value for prediction
counterfactual.degree <- predict(rf.Degree, newdata = treated.Degree)

#average treatment effects
mean(treated.Degree$re78) - mean(counterfactual.degree)

#bootstrap confidence interval of treatment effects:
storage.effect <- NULL
for (i in 1:1000) {
  sample.index <- sample(c(1:nrow(Degree)), nrow(Degree), replace= TRUE)
  sample <- Degree[sample.index,]
  rf.sample <- randomForest(re78 ~ treat + age + black+ hisp+ married + re74 + re75, data = sample, mtry = 3, importance = TRUE)
  
  #predicting the counterfactual
  treated.sample <- sample[sample$treat == 1,]
  treated.sample$treat = 0  #change the treated value for prediction
  counterfactual.sample <- predict(rf.sample, newdata = treated.sample)
  
  #average treatment effects
  storage.effect[i] <- mean(treated.sample$re78) - mean(counterfactual.sample)
}

qplot(storage.effect,
      geom="histogram",
      main = "Histogram for Estimated Treatment Effect (Degree)", 
      xlab = "Treatment Effect" )
  
quantile(storage.effect, c(.025, .975))


#NODEGREE
rf.NoDegree <- randomForest(re78 ~ treat + age + black+ hisp+ married + re74 + re75, data = NoDegree, mtry = 3, importance = TRUE)
importance(rf.NoDegree)
varImpPlot(rf.NoDegree)

#predicting the counterfactual
treated.NoDegree <- NoDegree[NoDegree$treat == 1,]
treated.NoDegree$treat = 0  #change the treated value for prediction
counterfactual.NoDegree <- predict(rf.NoDegree, newdata = treated.NoDegree)

#average treatment effects
mean(treated.NoDegree$re78) - mean(counterfactual.NoDegree)

#bootstrap confidence interval of treatment effects:
storage.effect2 <- NULL
for (i in 1:1000) {
  sample.index <- sample(c(1:nrow(NoDegree)), nrow(NoDegree), replace= TRUE)
  sample <- NoDegree[sample.index,]
  rf.sample <- randomForest(re78 ~ treat + age + black+ hisp+ married + re74 + re75, data = sample, mtry = 3, importance = TRUE)
  
  #predicting the counterfactual
  treated.sample <- sample[sample$treat == 1,]
  treated.sample$treat = 0  #change the treated value for prediction
  counterfactual.sample <- predict(rf.sample, newdata = treated.sample)
  
  #average treatment effects
  storage.effect2[i] <- mean(treated.sample$re78) - mean(counterfactual.sample)
}

qplot(storage.effect2,
      geom="histogram",
      main = "Histogram for Estimated Treatment Effect (No Degree)", 
      xlab = "Treatment Effect" )


quantile(storage.effect2, c(.025, .975))


#PART 3:
#DEGREE
#assume the sharp null hypothesis of no treatment 
test.statistic.degree <- mean(Degree[Degree$treat == 1, ]$re78) - mean(Degree[Degree$treat == 0,]$re78)
storage.vector <- NULL
for (i in 1:10000) {
  FishTreat_id <- sample(seq_len(nrow(Degree)),  nrow(Degree[Degree$treat==1,]))
  FishTreat <- Degree[FishTreat_id,]
  FishControl <- Degree[-FishTreat_id,]
  storage.vector[i] <- mean(FishTreat$re78) - mean(FishControl$re78)
}

test.statistic.degree
quantile(storage.vector, prob=c(0.975, 0.025))
plot(density(storage.vector))
abline(v = test.statistic.degree, lwd = 2, col = "red") 


#NODEGREE
#assume the sharp null hypothesis of no treatment 
test.statistic.NoDegree <- mean(NoDegree[NoDegree$treat == 1, ]$re78) - mean(NoDegree[NoDegree$treat == 0,]$re78)
storage.vector <- NULL
for (i in 1:10000) {
  FishTreat_id <- sample(seq_len(nrow(NoDegree)),  nrow(NoDegree[NoDegree$treat==1,]))
  FishTreat <- NoDegree[FishTreat_id,]
  FishControl <- NoDegree[-FishTreat_id,]
  storage.vector[i] <- mean(FishTreat$re78) - mean(FishControl$re78)
}

test.statistic.NoDegree
quantile(storage.vector, prob=c(0.975, 0.025))
plot(density(storage.vector))
abline(v = test.statistic.NoDegree, lwd = 2, col = "red") 

