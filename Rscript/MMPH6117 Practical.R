# Problem 1.1 -------
for (i in c(1:10)) {
    print (-3 * i + 7)
}

# Problem 1.2 -------
coor_x <- c(1:10)
gn <- 2 * (-0.5) ^ (coor_x - 1)
plot(coor_x, gn, type = 'l')

# Problem 2.1 -------
x <- rnorm(10000, 0, sqrt(2))
y <- rnorm(10000, 2, sqrt(3))
z <- x + y

# A - D 
par(mfrow = c(2, 2))
plot(density(x), xlab = "x", main = "Distribution of X", ylim = c(0, 0.4), las = 1)
plot(density(y), xlab = "y", main = "Distribution of Y", ylim = c(0, 0.4), xlim = c(-6, 10), las = 1)
plot(x, y, xlab = "x", ylab = "y", main = "Scatter plot between X and Y", cex = 0.1, las = 1)
plot(density(z), xlab = "x + y", main = "Distribution of X + Y", ylim = c(0, 0.4), xlim = c(-6, 10), las = 1)

# E 
print (paste("X mean ", mean(x), " X var ", var(x)))
print (paste("Y mean ", mean(y), " Y var ", var(y)))
print (paste("X + Y mean ", mean(z), " X + Y var ", var(z)))

# F
par(mfrow = c(1, 1))
plot(x, z, xlab = "x", ylab = "x + y", main = "Scatter plot between X and X + Y", cex = 0.1, las = 1)

# Problem 2.2 -------
# A
mvc$height.cat <- cut(mvc$height, c(155, 167, 172, 180), include.lowest = T)

# B
summary(lm(MVC ~ age + height.cat, data = mvc))

# C
summary(lm(MVC ~ age + relevel(height.cat, ref = 2), data = mvc))

# E
summary(lm(MVC ~ age + relevel(height.cat, ref = 2), data = mvc, subset = (age <= 40)))

