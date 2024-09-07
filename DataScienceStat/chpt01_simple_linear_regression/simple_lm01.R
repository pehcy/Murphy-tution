setwd('./DataScienceStat')
juice.dat <- read.table(file="L01orjuice.txt", header=TRUE)

# mean of x (Pectin) S_XX = (x - \bar(x))^2
SXX = sum((juice.dat$Pectin - mean(juice.dat$Pectin)) ** 2)
SXY = sum((juice.dat$Pectin - mean(juice.dat$Pectin)) 
          * (juice.dat$Sweet - mean(juice.dat$Sweet)))

bar_x <- mean(juice.dat$Pectin)
bar_y <- mean(juice.dat$Sweet)

beta_1 <- SXY / SXX
beta_0 <- bar_y - beta_1 * bar_x

# beta_0 = 6.252068
# beta_1 = -0.002310626
# regression line Y = 6.252068 + (-0.002310626)X + err
# Sweetness = 6.252068 + (-0.002310626) * Pectin 

# Regression
# lm (Y ~ X)
juiceReg <- lm(formula = Sweet ~ Pectin, data = juice.dat)
summary(juiceReg)

anova(juiceReg)

plot(x=juice.dat$Pectin, y=juice.dat$Sweet, xlab="Pectin", ylab="Sweet",
     main="Scatterplot of Pectin vs Sweetness", col="purple", pch=1, cex=1.5)
abline(juiceReg, col="red")

# prediction X = 320, Y =?
Y_320 = 6.252068 + (-0.002310626) * 320
