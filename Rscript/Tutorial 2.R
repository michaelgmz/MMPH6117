# Tutorial 2
flu <- read.csv("../Data/fluvaccine.csv")

# Q1 descriptive analysis
summary(flu)

# cross-tabulation between vaccination and flu infection
table(flu$vac, flu$flu)
prop.table(table(flu$vac, flu$flu),1)

with(flu,boxplot(shealth~vac))
with(flu,boxplot(shealth~flu))

# Q2 fitting a regression to estimate crude vaccination effect
lr.fv <- glm(flu~vac, data=flu, family=binomial)

summary(lr.fv)

exp(coef(lr.fv)["vac"])
exp(confint(lr.fv)) # based on likelihood method
exp(confint.default(lr.fv)) # based on normality

# The crude vaccine effect is 32.6% (95% CI: 0-43.3%) protection

# Q3 confirming confounding for self-reported health
# standard rules
lr.vs <- glm(vac~shealth, data=flu, family=binomial)
lr.fs <- glm(flu~shealth, data=flu, family=binomial)

summary(lr.vs)
summary(lr.fs)

# change in estimate
lr.fv <- glm(flu~vac, data=flu, family=binomial)
lr.fvs <- glm(flu~vac+shealth, data=flu, family=binomial)

summary(lr.fv)
summary(lr.fvs)

(coef(lr.fvs)[2]-coef(lr.fv)[2])/coef(lr.fv)[2]

# automatic variable selection
require(MASS)
stepAIC(lr.fvs)

# all 3 methods will select shealth as a confounder

# Q4 Stratification by shealth
flu$shealth.cat <- cut(flu$shealth, c(0,5,7,10), label=1:3, right=T, include.lowest=T)

lr.fv.strat <- list()
for (i in 1:3) {
lr.fv.strat[[i]] <- glm(flu~vac, data=flu, family=binomial, subset=(shealth.cat==i))
print(summary(lr.fv.strat[[i]]))
}

# approximate pooled estimate
(coef(lr.fv.strat[[1]])[2]+coef(lr.fv.strat[[2]])[2]+coef(lr.fv.strat[[3]])[2])/3

# Q5 Assessing potential confouder - suppose age is a suspected confounder
# standard rules
lr.va <- glm(vac~age, data=flu, family=binomial)
lr.fa <- glm(flu~age, data=flu, family=binomial)

summary(lr.va)
summary(lr.fa)

# there was no significant association between age and vaccination

# change in estimate
lr.fvsa <- glm(flu~vac+shealth+age, data=flu, family=binomial)

summary(lr.fvs)
summary(lr.fvsa)

(coef(lr.fvsa)[2]-coef(lr.fvs)[2])/coef(lr.fvs)[2]

# there was < 10% change in the vaccine effect estimate

# automatic variable selection
require(MASS)
stepAIC(lr.fvsa)

# Based on AIC, age will be selected (age is also significant)
# but no obvious association between age and vaccination
# similar results for BMI, smoking and sex
# antibody titers lie in the causal pathway from vaccination to infection

# Q6 effect modification by smoking and age
lr.fvs.s <- glm(flu~vac+shealth+vac*smoking, data=flu, family=binomial)
summary(lr.fvs.s)

lr.fvs.a <- glm(flu~vac+shealth+vac*age, data=flu, family=binomial)
summary(lr.fvs.a)

# there is evidence of the interaction effect between vaccination and smoking
# indicating the effect modification of smoking
# there is no significant age-vaccine interaction

# Q7 vaccination effect for smoker and non-smoker.
# non-smoker
exp(lr.fvs.s$coef["vac"])
exp(confint.default(lr.fvs.s)["vac",])

# AOR = 0.36, 95% CI = 0.23-0.58

# smoker
exp(lr.fvs.s$coef["vac"]+lr.fvs.s$coef["vac:smoking"])
se.vac.smoker <- sqrt(vcov(lr.fvs.s)[2,2]+vcov(lr.fvs.s)[5,5]+2*vcov(lr.fvs.s)[2,5])
# or alternatively, using matrix
# se.vac.smoker <- sqrt(t(c(1,1)) %*% vcov(lr.fvs.s)[c(2,5), c(2,5)] %*% c(1,1))
exp(lr.fvs.s$coef["vac"]+lr.fvs.s$coef["vac:smoking"] + c(-1,1)*qnorm(0.975)*se.vac.smoker)

# AOR = 0.96, 95% CI = 0.42-2.22

# Q8 assess collinearity 
require(car)
vif(lr.fvs.a)

# vif for vac and vac:age is large
flu$age.ct <- scale(flu$age, scale=F)

lr.fvs.act <- glm(flu~vac+shealth+vac*age.ct, data=flu, family=binomial)
summary(lr.fvs.act)

# cannot reduce collinearity with binary variable by centering

vif(lr.fvs.s)
# no collinearity problem for model with vaccine x smoking interaction

# Q9 antibody titer as mediator
flu$logabT <- log(flu$abT, base=2)

# antibody titer associated with vaccination?
lr.abTv <- glm(logabT~vac, data=flu)
summary(lr.abTv)

# antibody titer associated with flu infection?
lr.fabT <- glm(flu~logabT, data=flu, family=binomial)
summary(lr.fabT)

# vaccine associated with flu infection?
summary(lr.fvs.s)

# vaccine effect attenuated?
lr.fvsabT.s <- glm(flu~vac+shealth+logabT+vac*smoking, data=flu, family=binomial)
summary(lr.fvsabT.s)

# The data suggests that antibody titer is a mediator

# Q10 stepAIC - stepwise selection
# start from the basic model, to 
step.model <- stepAIC(lr.fv,
    scope = list(upper = ~vac+shealth+bmi+vac*age+vac*smoking, lower = ~vac))

# Q11 residual plot
final.model <- lr.fvs.s

resid <- rstudent(final.model)
plot(resid)

# Residual plots for logistic regression, along with binary variables are not straightforward
# to interpret

# Q12 ROC curve [for reference only] 
require(ROCR)
fm.pred <- predict(final.model, type="response")

# ROC curve
pred <- prediction(fm.pred, flu$flu)
perf <- performance(pred,"tpr","fpr")
plot(perf)

# AUROC
performance(pred,"auc")@y.values[[1]]

# Q13 Summarize results
exp(final.model$coef)
exp(confint.default(final.model))

round(exp(cbind(final.model$coef,confint.default(final.model))),2)



