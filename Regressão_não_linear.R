library(lattice)
library(latticeExtra)
library(car)
library(alr4)
library(nlstools)
library(nls2)
library(rootSolve)
library(readxl)
library(MASS)

library(devtools)
install_github(repo = "walmes/wzRfun", ref = "master")
library(wzRfun)

library(tidyverse)
library(manipulate)


library(growthmodels)

library(minpack.lm)

generalisedRichard

richard

dados1 = read_excel('dados.xlsx')
dados1 = dados1 %>% filter(dias < 750 )

str(dados1)
summary(dados1)



### volta aqui

par(mfrow = c(1,1))

plot(Cumulative_cases~dias, dados1, pch = 19)

# Modelo logistico
logi <- function(x, b1, b2, b3){
  b1 / (1 + exp(b2 - b3 * x))
}
start=list()
manipulate({
  plot(Cumulative_cases~dias,data = dados1)
  curve(logi(x, b1=b1,b2=b2,b3=b3),add=TRUE, col='red')
  start<<-list(b1=b1,b2=b2,b3=b3)},
  b1=slider(1,max(dados1$Cumulative_cases),initial=max(dados1$Cumulative_cases)),
  b2=slider(5, 100,initial=19, step = 0.01),
  b3=slider(0,1.5,initial=0.2, step = 0.001)
)
start


n0 <- nls(Cumulative_cases ~ (b1 / (1 + exp(b2 - b3 * dias))),
          data = dados1,
          start = start)
summary(n0)


as.list(coef(n0))


plot(Cumulative_cases~dias, dados1, pch = 19)

lines(logi(dados1$dias, b1=as.list(coef(n0))$b1,b2=as.list(coef(n0))$b2,b3=as.list(coef(n0))$b3), col = 'red', lwd = 2)


R2nls(n0)


## residuos

res_n0 = residuals(n0)
shapiro.test(res_n0)

res_n01 = nlsResiduals(n0)
AIC(n0)
BIC(n0)

plot(res_n01)


##Modelo Logístico e de Richards após a transformação cúbica da variável y

dados1$Cumulative_cases = (dados1$Cumulative_cases)^3

par(mfrow = c(1,1))

plot(Cumulative_cases~dias, dados1, pch = 19)

## outro modelo

modRic = function(x,b1,b2,b3,b4,b5,b6){
  b1 + (b2 - b1) / (1 + b3 * exp(-b4 * (x - b5)))^(1/b6)
}
startModRic=list()
manipulate({
  plot(Cumulative_cases~dias,data = dados1)
  curve(modRic(x, b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6),add=TRUE, col='red')
  startModRic<<-list(b1=b1,b2=b2,b3=b3,b4=b4,b5=b5,b6=b6)},
  b1=slider(0,max(dados1$Cumulative_cases),initial=0),
  b2=slider(1,max(dados1$Cumulative_cases),initial=max(dados1$Cumulative_cases)),
  b3=slider(0, max(dados1$Cumulative_cases),initial=1, step = 0.01),
  b4=slider(-1,10,initial=0.2, step = 0.001),
  b5=slider(0,60,initial = 0,step = 1),
  b6=slider(-1,10,initial = 0.1,step = 0.01)
)
startModRic


n0 <- nls(Cumulative_cases ~ (b1 / (1 + exp(b2 - b3 * dias))),
          data = dados1,
          start = start)
summary(n0)


as.list(coef(n0))



n2 = nlsLM(Cumulative_cases ~ (b1 + (b2 - b1) / (1 + b3 * exp(-b4 * (dias - b5)))^(1/b6)),
           data = dados1,
           start = startModRic,
           control = list(maxiter=100, maxfev = 100))
summary(n2)

as.list(coef(n2))

plot(Cumulative_cases~dias, dados1, pch = 19)
lines(modRic(dados1$dias, b1=as.list(coef(n2))$b1,b2=as.list(coef(n2))$b2,b3=as.list(coef(n2))$b3, 
             b4=as.list(coef(n2))$b4, b5=as.list(coef(n2))$b5, b6=as.list(coef(n2))$b6), col = 'blue', lwd = 2)

lines(logi(dados1$dias, b1=as.list(coef(n0))$b1,b2=as.list(coef(n0))$b2,b3=as.list(coef(n0))$b3), col = 'red', lwd = 2)


R2nls(n0)

R2nls(n2)

## residuos

res_n0 = residuals(n0)
shapiro.test(res_n0)

res_n01 = nlsResiduals(n0)
AIC(n0)
BIC(n0)

plot(res_n01)

res_n2 = residuals(n2)
shapiro.test(res_n2)

res_n02 = nlsResiduals(n2)
AIC(n2)
BIC(n2)

plot(res_n02)