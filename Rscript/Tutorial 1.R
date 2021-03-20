# Question 1
# 1(a)
u0 <- runif(3,1,3)
mean(u0)

# 1(b)
u1 <- matrix(runif(3*1000,1,3), nrow=3)
u1.mean <- colMeans(u1)

# 1(c)
mean(u1.mean)
var(u1.mean)

plot(density(u1.mean))
hist(u1.mean)

# 1(e)
u2 <- matrix(runif(30*1000,1,3), nrow=30)
u2.mean <- colMeans(u2)

mean(u2.mean)
var(u2.mean)

plot(density(u2.mean))
hist(u2.mean)

# 1(f)
hist(sample(1:5, 1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), breaks=0:5, freq=F, main="")

b1 <- matrix(sample(1:5, 3*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=3)
b1.mean <- colMeans(b1)

plot(density(b1.mean))
hist(b1.mean, breaks=0:50/10)


b2 <- matrix(sample(1:5, 30*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=30)
b2.mean <- colMeans(b2)

plot(density(b2.mean))
hist(b2.mean, breaks=0:50/10)

# 1(g)
??normality

shapiro.test(u1.mean)
shapiro.test(u2.mean)

shapiro.test(b1.mean)
shapiro.test(b2.mean)

b3 <- matrix(sample(1:5, 50*1000, replace=T, prob=c(0.1,0.35,0.1,0.35,0.1)), nrow=50)
b3.mean <- colMeans(b3)

plot(density(b3.mean))
hist(b3.mean, breaks=0:50/10)

shapiro.test(b3.mean)


# Question 2
mvc <- read.csv("http://web.hku.hk/~ehylau/mvc.csv")

summary(mvc)

# 2(a)
mvc$younger <- cut(mvc$age, c(min(mvc$age), 40, max(mvc$age)), lab = c(1, 0), include.lowest=T)

# 2(b)
aggregate(mvc$MVC,by=list(mvc$younger),mean)
# alternatively
aggregate(MVC~younger,data=mvc,mean)

# 2(c)
boxplot(MVC~younger, data=mvc, xlab = 'Younger adults', ylab = 'MVC', main = 'Boxplot of MVC by age group' )

# 2(d)
plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', main='Scatter plot between Height and MVC')
abline(lm(MVC ~ height, data = mvc))

# 2(e)
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
}


# 2 (f)
par(mfrow=c(2,2), mar=c(4,4,1,1))
age.lower <- 2:5*10

for (i in 1:4){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}

# example: by 5 years age groups
#windows(width=6, height=10)
pdf('d:/figure1.pdf', width=6, height=10)
par(mfrow=c(4,2), mar=c(4,4,1,1))
age.lower <- 4:11*5

for (i in 1:8){
  plot(mvc$height, mvc$MVC, xlab = 'Height', ylab = 'MVC', type='n', las=1)
  temp.mvc <- mvc[mvc$age>age.lower[i], ]
  points(temp.mvc$height, temp.mvc$MVC, col=gray(0.7), pch=19)
  temp.lm.mvc <- lm(MVC ~ height, data = temp.mvc)
  abline(temp.lm.mvc)
  legend("topleft", paste('age > ', age.lower[i], 'y', sep=''))
  legend("bottomright",paste('MVC =', format(round(temp.lm.mvc$coef[1],1),nsmall=1), 
' + ', format(round(temp.lm.mvc$coef[2],1),nsmall=1), 'age', sep=''), bty='n', text.font=2)
}
dev.off()

# 2(g)
lm.mvc <- lm(MVC ~ height, data=mvc)

mvc$height2 <- mvc$height^2
lm.mvc2 <- lm(MVC ~ height + height2, data=mvc)

summary(lm.mvc2)

# 2(h)
AIC(lm.mvc, lm.mvc2) #AIC difference < 2 -> prefer the more parsimonious model

# 2(i)
require(MASS)
lm.mvc3 <- lm(MVC ~ age + height + height2, data=mvc)
step.mvc <- stepAIC(lm.mvc3, direction="both")

summary(step.mvc)

# scope: specific the range of models to be selected
step.mvc2 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height))
step.mvc3 <- stepAIC(lm.mvc3, direction="both", scope=list(lower=~height, upper=~age*height+age*height2))


# 2(j)
lm.mvc4 <- lm(MVC ~ age + height, data=mvc)

new <- data.frame(age=50, height=170)
predict(lm.mvc4, new, interval="prediction")


# 2(k): extrapolation
new2 <- data.frame(age=50, height=220)
predict(lm.mvc4, new2, interval="prediction")

new3 <- data.frame(age=50, height=150:220)
pred.mvc4 <- predict(lm.mvc4, new3, interval="prediction")

plot(new3$height, pred.mvc4[,"fit"], type='l', ylim=c(0,1000), xlab="height", ylab="predicted MVC", las=1)
lines(new3$height, pred.mvc4[,"lwr"], lty=2)
lines(new3$height, pred.mvc4[,"upr"], lty=2)
text(220, 1000, "at age 50y", adj=1, font=2)
polygon(c(rep(min(mvc$height),2),rep(max(mvc$height),2)),c(-50,1100,1100,-50), border=NA, col=rgb(0,0.5,0,0.2))





