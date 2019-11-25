library(car)
library(MASS)

data <- read.csv('BostonHousePrice.csv')
data <- data[2:15]

# 包含定性变量拟合模型，R^2为0.7406，CHAS变量不能剔除
lm <- lm(MEDV~.,data)
summary(lm)
# 不包含定性变量CHAS直接拟合模型，R^2为0.7355
dataEXchas <- data[c(c(1:3),c(5:14))]
lmEXchas <- lm(MEDV~.,dataEXchas)
summary(lmEXchas)

# 方差分析 NOX和RAD未通过显著性检验
anova(lm)

# 选模型
lm.for <- step(lm,direction = "both")
summary(lm.for)

#多重共线性检验 RAD和TAX的方差扩大因子较大，相关系数0.91,RAD和TAX存在多重共线性
vif(lm.for)
cor(data$RAD,data$TAX)

#岭迹图
datas <- data.frame(scale(data))
ridge <- lm.ridge(MEDV~.-1,data=datas,lambda = seq(0,200,10))
beta <- coef(ridge)
beta
k <- ridge$lambda
plot(k,k,type="n",xlab="岭参数",ylab="岭回归系数",ylim=c(-0.5,0.5))
linetype <- c(1:13)
char <- c(18:30)
for (i in 1:13)
  lines(k, beta[,i], type="o", lty = linetype[i], pch = char[i], cex = 0.75)
legend(locator(1),inset = 0.5, legend = c("CRIM","ZN","INDUS","CHAS","NOX","RM",
                                          "AGE","DIS","RAD","TAX","PTRATIO","B",
                                          "LSTAT","MEDV"), 
       cex = 0.56, pch = char, lty = linetype)
