fish <- read.csv('../Data/fishconsumption.csv')

# mark stratified analysis
fish$strat <- 0
fish$strat[c(21, 22, 24, 25)] <- 1

fish$logrr <- log(fish$rr)
fish$se.logrr <- (log(fish$rr.ub)-log(fish$rr.lb))/(2*1.96)

library(metafor)

# fit a fixed effects model
fish.fe <- rma(yi=logrr, sei=se.logrr, slab=study, method="FE", 
data=fish, subset=strat==0)

fish.fe

# Overall estimate by fixed effects model
with(fish.fe, exp(c(b, ci.lb, ci.ub)))

# calculate the overall effect by inverse variance weighting
ivw.est <- with(fish[fish$strat==0,], sum(logrr/se.logrr^2)/sum(1/se.logrr^2))
ivw.var <- with(fish[fish$strat==0,], 1/sum(1/se.logrr^2))

# Q-statistics and I2
summary(fish.fe)

Q <- fish.fe$QE
I2 <- (Q-(fish.fe$k-1))/Q * 100

I2

# fit a random effects model
fish.re <- rma(yi=logrr, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=strat==0)

# Overall estimate by random effects model
with(fish.re, exp(c(b, ci.lb, ci.ub)))

# forest plot based on the random effects model
forest(fish.re, transf=exp, refline=1)

# x axis in log scale, with weights
forest(fish.re, atransf=exp, refline=0, at=log(c(0.1,0.25,1,4,20)), showweights=T)

fish.re <- rma(yi=logrr, sei=se.logrr, slab=paste(study, year, country, sex, sep=", "), method="REML", 
data=fish, subset=strat==0)

forest(fish.re, atransf=exp, refline=0, at=log(c(0.1,0.25,1,4,20)), 
xlim=c(-6,5.5), showweights=T, xlab="Relative risk (log scale)")
text(-6, 28, "Study", pos=4, font=2)
text(5.5, 28, "Relative risk [95% CI]", pos=2, font=2)

# Funnel plot
funnel(fish.re, atransf=exp)

# Egger's test
regtest(fish.re)

# Meta regression on sex
fish$sex.spec <- 1*(fish$sex!="Both")
fish.sex <- fish[fish$sex.spec==1,]
fish.sex$sex <- droplevels(fish.sex$sex)

fish.re.sex <- rma(yi=logrr~sex, sei=se.logrr, slab=study, method="REML", 
data=fish.sex)

fish.re.sex

fish.re.year <- rma(yi=logrr~year, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=strat==0)

fish.re.year 

# Sensitivity analysis
fish$lq <- 0
fish$lq[fish$study=="Smith"|fish$study=="Colangelo"] <- 1

fish.re.sen <- rma(yi=logrr, sei=se.logrr, slab=study, method="REML", 
data=fish, subset=(strat==0 & lq==0))

with(fish.re, exp(c(b, ci.lb, ci.ub)))
with(fish.re.sen, exp(c(b, ci.lb, ci.ub)))

# Leave one out sensitivity analysis
leave1out(fish.re, transf=exp)

# Trim and fill sensitivity analysis
fish.cut <- fish[fish$study!="Albanese",]

fish.cut.re <- rma(yi=logrr, sei=se.logrr, slab=paste(study, year, country, sex, sep=", "), method="REML", 
data=fish.cut, subset=strat==0)

funnel(fish.cut.re, atransf=exp)
regtest(fish.cut.re)

fish.cut.tf <- trimfill(fish.cut.re)
with(fish.cut.tf, exp(c(b, ci.lb, ci.ub)))

funnel(fish.cut.tf)

