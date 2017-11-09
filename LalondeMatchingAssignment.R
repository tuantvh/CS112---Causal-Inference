library(foreign)
library(Matching)
nsw_dw <- read.dta("nsw_dw.dta")

#1 
mean(nsw_dw[nsw_dw$treat == 1,]$re78) - mean(nsw_dw[nsw_dw$treat == 0,]$re78)

fit <- lm(re78 ~ treat, data = nsw_dw)
summary(fit)
confint(fit)

#2 
cps_control <- read.dta("cps_controls.dta")

#substitute cps_controlg roup
nsw_dw.treat <- nsw_dw[nsw_dw$treat == 1, ]
nsw_dw.new <- rbind(nsw_dw.treat, cps_control)

#estimate treatment effect
mean(nsw_dw.new[nsw_dw.new$treat == 1,]$re78) - mean(nsw_dw.new[nsw_dw.new$treat == 0,]$re78)

fit2 <- lm(re78 ~ treat, data = nsw_dw.new)
summary(fit2)
confint(fit2)


#3 PROPENSITY SCORE MATCHING
#without interaction terms

#estimate the propensity model
glm1  <- glm(treat~age + education  + black + hispanic + married + nodegree + re74 + re75 
             , family=binomial, data= nsw_dw.new)

#save data objects
X  <- glm1$fitted
Y  <- nsw_dw.new$re78
treat  <- nsw_dw.new$treat

#Match
rr  <- Match(Y=Y, Tr=treat, X=X, M=1);
summary(rr)

#CI
upper = rr$est + 1.96*rr$se
lower = rr$est - 1.96*rr$se


mb  <- MatchBalance(treat~age + education  + black +
                      hispanic + married + nodegree + re74 + re75 
                    , data=nsw_dw.new, match.out=rr, nboots=10)

#with interactions terms
#estimate the propensity model
glm2  <- glm(treat~age + I(age^2) + education + I(education^2) + black +
               hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2) 
             , family=binomial, data= nsw_dw.new)

#save data objects
X  <- glm2$fitted
Y  <- nsw_dw.new$re78
treat  <- nsw_dw.new$treat

rr  <- Match(Y=Y, Tr=treat, X=X, M=1);
summary(rr)

#CI
rr$est + 1.96*rr$se 
rr$est - 1.96*rr$se

mb  <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                      hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2) 
                    , data=nsw_dw.new, match.out=rr, nboots=10)


#4 Multivaraite Matching
attach(nsw_dw.new)

#without interaction terms
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75, glm1$fitted)
rr2  <- Match(Y=Y, Tr=treat, X=X, M=1);
summary(rr2)

rr2$est + 1.96*rr2$se
rr2$est - 1.96*rr2$se


mb  <- MatchBalance(treat~age + education + black +
                      hispanic + married + nodegree + re74  + re75
                    , data=nsw_dw.new, match.out=rr, nboots=10)


#with interaction terms
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75, glm2$fitted)
rr2  <- Match(Y=Y, Tr=treat, X=X, M=1);
summary(rr2)

rr2$est + 1.96*rr2$se
rr2$est - 1.96*rr2$se


mb  <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                      hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2) 
                    , data=nsw_dw.new, match.out=rr, nboots=10)


#5. GEN MATCH
#matching on only propensity score with Genmatch

X  <- glm2$fitted
BalanceMat <-glm2$fitted 

#genmatch
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=50, max.generations=10, wait.generations=1)
mout <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74+ glm2$fitted, data = nsw_dw.new,
                    match.out=mout, nboots=500)


#Match on all covariates with Genmatch
#without interaction terms
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75, glm1$fitted)
BalanceMat <- cbind(age, education, black, hispanic, married, nodegree, re75, re74, glm1fitted)

##genmatch
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1, replace = TRUE)

## Match
mout2 <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout, M=1, replace = TRUE)
summary(mout2)

##match balance                
mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74 + glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)


#with interaction terms
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75, glm2$fitted)
BalanceMat <- cbind(age, education, black, hispanic, married, nodegree, re75, re74, glm2$fitted)
                    
##genmatch
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1, replace = TRUE)

## Match
mout2 <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout, M=1, replace = TRUE)
summary(mout2)

##match balance                
mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74 + glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)


#EXTRA CREDIT:
#exact matching
X <- cbind(age, education, black, hispanic, married, nodegree, re74, re75, glm2$fited)
BalanceMat <- cbind(age, education, black, hispanic, married, nodegree, re74,re75, glm2$fitted)

genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1, 
                   pop.size=100, max.generations=10, wait.generations=1, exact = TRUE)
mout2 <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout, M=1, exact = TRUE)

summary(mout2)

mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74, glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)


#caliper 
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat,  caliper = 0.5)
mout2 <- Match(Y=Y,Tr=treat, X=X, Weight.matrix=genout, caliper = 0.5)

summary(mout2)

mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74, glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)

#replace = FALSE
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1, replace = FALSE)
mout2 <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout, M=1, replace = FALSE)
summary(mout2)

mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74 + glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)

#pop.size = 1000
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                  pop.size=1000, max.generations=10, wait.generations=1, replace = TRUE)
mout2 <- Match(Y=Y,Tr=treat, X=X, estimand="ATT", Weight.matrix=genout, M=1, replace = TRUE)
summary(mout2)

mb2 <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree +
                      re75+ re74 + glm2$fitted, data = nsw_dw.new,
                    match.out=mout2, nboots=500)
