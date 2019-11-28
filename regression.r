#####################
# 数据集说明：
#     data       : 剔除NOX后的全部数据，506个观测，12个自变量，1个因变量
#     datafor    : 经过变量选择后所保留的自变量和因变量
#     dataExchas : 全模型数据剔除定性变量CHAS后的数据
#
#####################
library(car)
library(lmtest)
library(MASS)

data <- read.csv('BostonHousePrice.csv')
data <- data[c(c(2:5),c(7:15))]
# 包含定性变量拟合模型，R^2为0.7406，CHAS变量不能剔除
lm <- lm(MEDV~.,data)
summary(lm)
# 不包含定性变量CHAS直接拟合模型，R^2为0.7355
dataEXchas <- data[c(c(1:3),c(5:13))]
lmEXchas <- lm(MEDV~.,dataEXchas)
summary(lmEXchas)

# 方差分析 全部通过显著性检验
anova(lm)

# 选模型
lm.for <- step(lm,direction = "backward")
#lm.for <- step(lm,direction = "forward")
#lm.for <- step(lm,direction = "both")
summary(lm.for)
datafor <- data[c(c(1:2),c(4:5),c(7:13))]

# 异方差检验
data1 <- data.frame(data)
lm1 <- lm(MEDV~.,data = data1)
e <- resid(lm1,digits = 5)
e2 <- e^2
data2 <- data.frame(datafor,e2)
lm2 <- lm(e2~.,data = data2)
an1 <- anova(lm2)

#多重共线性检验 RAD和TAX的方差扩大因子较大，相关系数0.91,RAD和TAX存在多重共线性
vif(lm.for)
cor(datafor$RAD,datafor$TAX)

#岭迹图
datas <- data.frame(scale(datafor))
ridge <- lm.ridge(MEDV~.-1,data=datas,lambda = seq(0,200,10))
beta <- coef(ridge)
beta
k <- ridge$lambda
plot(k,k,type="n",xlab="岭参数",ylab="岭回归系数",ylim=c(-0.5,0.5))
linetype <- c(1:10)
char <- c(18:27)
for (i in 1:10)
  lines(k, beta[,i], type="o", lty = linetype[i], pch = char[i], cex = 0.75)
legend(locator(1),inset = 0.5, legend = c("CRIM","ZN","CHAS","RM","DIS",
                                          "RAD","TAX","PTRATIO","B","LSTAT"), 
       cex = 0.6, pch = char, lty = linetype)
# 确定加权函数的自变量
e <- resid(lm.for)
abse <- abs(e)
value <- as.null()
field <- c("CRIM","ZN","CHAS","RM","DIS","RAD","TAX","PTRATIO","B","LSTAT")
for (i in 1:length(field)){
  a <- data.frame()
  a <- cor.test(datafor[,i],abse,method = "spearman")
  print(a$estimate)
  if(is.null(value)){
    value <- a$estimate
    var <- i
  }else if(value <= a$estimate){
    value <- a$estimate
    var <- i
  }
}
# 寻找最优权函数，加权最小二乘
s <-seq(-2,2,0.2)
result1 <- vector(length = 21,mode = "list")
result2 <- vector(length = 21,mode = "list")
for (j in 1 : 21)
{
  w <- datafor[,var] ^ (-s[j])
  lm4 <- lm(MEDV~.,weights = w,data = datafor)
  result1[[j]] <- logLik(lm4)
  result2[[j]] <- summary(lm4)
}
result1
result2[9]

# DW检验
dwtest(lm.for,alternative = "two.sided")
# BOX-COX
bc <- boxcox(MEDV~.,data = datafor,lambda = seq(-2,2,0.01))
lambda <- bc$x[which.max(bc$y)]
MEDV_bc <- (datafor$MEDV^lambda-1)/lambda
lm_bc <- lm(MEDV_bc~.-MEDV,datafor)
summary(lm_bc)
abse_bc <- abs(resid(lm_bc))
for (i in 1:length(field)){
  print(cor.test(datafor[,i],abse_bc,method = "spearman"))
}


# 定性变量的回归系数相等性检验
lmQual <- lm(MEDV~.+I(CRIM*CHAS)+I(ZN*CHAS)+I(RM*CHAS)+I(DIS*CHAS)+I(RAD*CHAS)
             +I(TAX*CHAS)+I(PTRATIO*CHAS)+I(B*CHAS)+I(LSTAT*CHAS),datafor)
summary(lmQual)
