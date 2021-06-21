## Bayes 2

library(LearnBayes)

## Escrevendo a função posteriori para uma
## amostra Normal com priori normal na média
## e priori "chata" na variância

# primeiro escreve uma função para calcular
# a verossimilhança
logf = function(y, mu, sigma)
  dnorm(y,mean=mu,sd=sigma,log=TRUE)

# agora o log da posteriori
mylogposterior=function(theta,data)
{
  n=length(data)
  mu=theta[1]; sigma=exp(theta[2])
  logf = function(y, mu, sigma)
    dnorm(y,mean=mu,sd=sigma,log=TRUE)
  val=dnorm(mu, mean=10, sd=20,log=TRUE)+sum(logf(data,mu,sigma))
  return(val)
}

# volta às notas

betabinexch0=function (theta, data)
{
  eta = theta[1]
  K = theta[2]
  y = data[, 1]
  n = data[, 2]
  N = length(y)
  logf = function(y, n, K, eta) lbeta(K * eta + y, K * (1 -
                                                          eta) + n - y) - lbeta(K * eta, K * (1 - eta))
  val = sum(logf(y, n, K, eta))
  val = val - 2 * log(1 + K) - log(eta) - log(1 - eta)
  return(val)
}

data(cancermortality)

windows()
mycontour(betabinexch0,c(.0001,.003,1,20000),cancermortality,
          xlab="eta",ylab="K")

# volta às notas

betabinexch=function (theta, data)
{
  eta = exp(theta[1])/(1 + exp(theta[1]))
  K = exp(theta[2])
  y = data[, 1]
  n = data[, 2]
  N = length(y)
  logf = function(y, n, K, eta) {
    lbeta(K * eta + y, K * (1 - eta) + n - y) - 
    lbeta(K * eta, K * (1 - eta))
  }
  val = sum(logf(y, n, K, eta))
  val = val + theta[2] - 2 * log(1 + exp(theta[2]))
  return(val)
}

mycontour(betabinexch,c(-8,-4.5,3,16.5),cancermortality,
          xlab="logit eta",ylab="log K")

# melhorou a assimetria

# volta às notas

## Aproximações no modo
# achar o modo e variância
fit=laplace(betabinexch,c(-7,6),cancermortality)
fit

npar=list(m=fit$mode,v=fit$var)
mycontour(lbinorm,c(-8,-4.5,3,16.5),npar,
            xlab="logit eta", ylab="log K")

# rápida aproximação de intervalos de confiança
se=sqrt(diag(fit$var))
fit$mode-1.645*se
fit$mode+1.645*se

# volta às notas

## Uma cadéia de Markov Discreta
# a matriz de transição
P=matrix(c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,
           0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),
           nrow=6,ncol=6,byrow=TRUE)
P
# um local para armazenar resultados de um passeio de 50000 passos
s=integer(50000)
# começa da posição 3 e vai andando
s[1]=3
# amostra dos estados com as probabilidades de transição para o estado anterior
for (j in 2:50000)
  s[j]=sample(1:6,size=1,prob=P[(s[j-1]),])

# para cadeias de comprimentos diferentes vamos olhar a distribuição dos estados
m = c(500,2000,8000,50000)
for (i in 1:4)
  print(table(s[1:m[i]])/m[i])

# parece estar convergindo para 10%, 20%, 20%, 20%, 20%, 10%.
# de fato essa é a distribuição estacionária
w=matrix(c(.1,.2,.2,.2,.2,.1),nrow=1,ncol=6)
w%*%P

# volta às notas

## MCMC Metropolis-Hastings (dois parâmetros)
# Ajustar uma Normal de um histograma

# uma função para calcular a posteriori
# os dados tem as frequencias, limites inferiores e superioris
# os parâmetros são mu e lambda = log(sigma)
groupeddatapost=function(theta,data)
{
  dj = function(f, int.lo, int.hi, mu, sigma)
    f * log(pnorm(int.hi, mu, sigma) -
              pnorm(int.lo, mu, sigma))
  mu = theta[1]
  sigma = exp(theta[2])
  sum(dj(data$f, data$int.lo, data$int.hi, mu, sigma))
}

# define os dados
d=list(int.lo=c(-Inf,seq(66,74,by=2)),
       int.hi=c(seq(66,74,by=2), Inf),
       f=c(14,30,49,70,33,15))

# para usar Laplace para achar o modo, precisamos de um chute
# inicial bom.
# Vamos criar um banco artificial com todos os dados no ponto médio de
# cada intervalo para estimar a média e variância
y=c(rep(65,14),rep(67,30),rep(69,49),rep(71,70),rep(73,33),rep(75,15))
mean(y)
log(sd(y))

start=c(70,1)
fit=laplace(groupeddatapost,start,d)
fit


# estimativas dos desvios padrão
modal.sds=sqrt(diag(fit$var))


# metropolis de passeio aleatório
proposal=list(var=fit$var,scale=2)
fit2=rwmetrop(groupeddatapost,proposal,start,10000,d)
fit2$accept
# varia a escala para ver como varia a aceitação

# as médias posterioris
post.means=apply(fit2$par,2,mean)
# os desvio padrão posterioris
post.sds=apply(fit2$par,2,sd)

# aproximação normal
cbind(c(fit$mode),modal.sds)

# cadeia MCMC
cbind(post.means,post.sds)

windows()
mycontour(groupeddatapost,c(69,71,.6,1.3),d,xlab="mu",ylab="log sigma")
points(fit2$par[5001:10000,1],fit2$par[5001:10000,2])

## Se começar com um valor ruim, precisa usar diagnósticos para
## saber quando passou burn-in
start=c(65,1)
proposal=list(var=fit$var,scale=0.2)
bayesfit=rwmetrop(groupeddatapost,proposal,start,10000,d)
bayesfit$accept
# a taxa de aceitação é muito alta, algo não está certa

# traceplots
library(lattice)
library(coda)
dimnames(bayesfit$par)[[2]]=c("mu","log sigma")
windows()
xyplot(mcmc(bayesfit$par),col="black")
xyplot(mcmc(bayesfit$par[-c(1:2000),]),col="black")
# os dados ficam numa região por muito tempo antes
# de mudar para outra; muito autocorrelação
windows()
par(mfrow=c(2,1))
autocorr.plot(mcmc(bayesfit$par[-c(1:2000),]),auto.layout=FALSE)
windows()
par(mfrow=c(2,1))
autocorr.plot(mcmc(fit2$par),auto.layout=FALSE)

# e o erro padrão das estimativas?
# sem levar correlações em conta
apply(bayesfit$par[-c(1:2000),],2,sd)/sqrt(8000)
# levando em conta
batchSE(mcmc(bayesfit$par[-c(1:2000),]))

# voltar às notas

## Modelos Hierárquicos
# taxa de mortalidade nos hospitais
data("hearttransplants")
with(hearttransplants, {
  windows()
  plot(log(e), y/e, xlim=c(6,9.7), xlab="log(e)", ylab="y/e")
  text(log(e),y/e,labels=as.character(y),pos=4)
})
# grande variação de taxas

# voltar às notas

# taxas iguias (Poisson com priori 1/lambda)
with(hearttransplants,{
  print(sum(y))
  print(sum(e))
})

# tira uma amostra de 1000 valores de lambda
lambda=rgamma(1000,shape=277,rate=294681)
# gera 1000 valores da Poisson com os valores diferentes de lambda e 
# exposições diferentes dos hospitias (veja hospital 94)
with(hearttransplants, {ys94 <<- rpois(1000,e[94]*lambda)})

# onde fica o valor observado nessa distribuição?
with(hearttransplants,{
  hist(ys94,breaks=seq(0.5,max(ys94)+0.5))
  lines(c(y[94],y[94]),c(0,120),lwd=3)
})

# esse valor é perto da cauda. Como são os outros 93 valores?
# para resumir vamos calcular a probabilidade de ter valore mais
# extremos do que o observado para cada hospital

lambda=rgamma(1000,shape=277,rate=294681)
prob.out=function(i)
  with(hearttransplants, {
    ysi=rpois(1000,e[i]*lambda)
    pleft=sum(ysi<=y[i])/1000
    pright=sum(ysi>=y[i])/1000
    min(pleft,pright)
    })
pout=sapply(1:94,prob.out)

with(hearttransplants, {
  plot(log(e),pout,ylab="Prob(extreme)")
})

# o grande número de valores abaixo de 0.1 (15 dos 94) sugere
# que esse modelo não é adequada

# voltar às notas

# distribuição gamma com hiper parâmetro ligando as lambdas
# gráfico da distribuição conjunta de lambda 1 e 2
pgexchprior=function(lambda,pars)
{
  alpha=pars[1]; a=pars[2]; b=pars[3]
  (alpha-1)*log(prod(lambda))-(2*alpha+a)*log(alpha*sum(lambda)+b)
}

# quatro valores diferentes de alpha e a=b=10
alpha=c(5,20,80,400); par(mfrow=c(2,2))
for (j in 1:4)
  mycontour(pgexchprior,c(.001,5,.001,5),c(alpha[j],10,10),
            main=paste("ALPHA = ",alpha[j]),xlab="LAMBDA 1",ylab="LAMBDA 2")

# maior que seja alpha, maior a correlação entre as lambdas

# voltar às notas

# posteriori conjunta de theta1=log(alpha) e theta2=log(mu)
poissgamexch=function (theta, datapar)
{
  y = datapar$data[, 2]
  e = datapar$data[, 1]
  z0 = datapar$z0
  alpha = exp(theta[1])
  mu = exp(theta[2])
  beta = alpha/mu
  logf = function(y, e, alpha, beta)
    lgamma(alpha + y) - (y + alpha) * log(e + beta) +
    alpha * log(beta) - lgamma(alpha)
  val = sum(logf(y, e, alpha, beta))
  val = val + log(alpha) - 2 * log(alpha + z0)
  return(val)
}

# encontra o modo dessa função
datapar = list(data = hearttransplants, z0 = 0.53)
start=c(2, -7)
fit = laplace(poissgamexch, start, datapar)
fit

# visualisa a densidade
par(mfrow = c(1, 1))
mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar,
          xlab="log alpha",ylab="log mu")

# como é bem assimétrica, vamos simular usando Metropolis em Gibbs
start = c(4, -7)
fitgibbs = gibbs(poissgamexch, start, 1000, c(2,.30), datapar)
fitgibbs$accept
# uma taxa um pouco alto, aumentei as escalas por um fator de 2

# compara as amostras com a densidade
mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar,
          xlab="log alpha",ylab="log mu")
points(fitgibbs$par[, 1], fitgibbs$par[, 2])

# gráfico da densidade de log(alpha) o parâmetro de precisão
plot(density(fitgibbs$par[, 1], bw = 0.2))

# para amostrar as lambdas, primeiro, gera vetores de alpha e mu
alpha = exp(fitgibbs$par[, 1])
mu = exp(fitgibbs$par[, 2])
# depois geram as lambdas para o primeiro hospital
with(hearttransplants,{
lam1 <<- rgamma(1000, y[1] + alpha, e[1] + alpha/mu)
})

# essa simulação pode ser feita para cada hospital e um intervalo
# de credibilidade para lambda pode ser colocado num gráfico para
# comparações
with(hearttransplants,{
  plot(log(e), y/e, pch = as.character(y))
  for (i in 1:94) {
    lami = rgamma(1000, y[i] + alpha, e[i] + alpha/mu)
    probint = quantile(lami, c(0.05, 0.95))
    lines(log(e[i]) * c(1, 1), probint)
  }
})

# voltar às notas

# shrinkage vs. exposição
with(hearttransplants,{
  shrink=function(i) mean(alpha/(alpha + e[i] * mu))
  shrinkage=sapply(1:94, shrink)
  plot(log(e), shrinkage)
})

# voltar às notas

# qual o hospital com a menor taxa média?

#essa função calcula a taxa média para hospital i
mrate=function(i) with(hearttransplants,{ mean(rgamma(1000, y[i] + alpha, e[i]
                              + alpha/mu)) })
# aplica essa função para as 94 hospitais
hospital=1:94
meanrate=sapply(hospital,mrate)
# qual o hospital com a taxa menor?
hospital[meanrate==min(meanrate)]
# qual essa taxa?
meanrate[85]
range(meanrate)

# vamos criar uma matriz 94 x 94 onde entrada ij é a probabilidade
# da taxa de hospital i ser menor que a de hospital j
sim.lambda=function(i) with(hearttransplants, {
  rgamma(1000,y[i]+alpha,e[i]+alpha/mu)
})
LAM=sapply(1:94,sim.lambda)
compare.rates <- function(x) {
  nc <- NCOL(x)
  ij <- as.matrix(expand.grid(1:nc, 1:nc))
  m <- as.matrix(x[,ij[,1]] > x[,ij[,2]])
  matrix(colMeans(m), nc, nc, byrow = TRUE)
  }
better=compare.rates(LAM)
better[1:24,85]
# para os primeiros 24 hospitais, 85 é melhor mais frequentemente

# voltar às notas

## SIR para reamostrar da amostra considerando uma priori nova
sir.old.new=function(theta, prior, prior.new)
{
  log.g=log(prior(theta))
  log.g.new=log(prior.new(theta))
  wt=exp(log.g.new-log.g-max(log.g.new-log.g))
  probs=wt/sum(wt)
  n=length(probs)
  indices=sample(1:n,size=n,prob=probs,replace=TRUE)
  theta[indices]
}

# para usar sir.old.new, precisamos das prioris velhos e novos
prior=function(theta)
  0.53*exp(theta)/(exp(theta)+0.53)^2
prior.new=function(theta)
  5*exp(theta)/(exp(theta)+5)^2

# reamostra dos dados originais
log.alpha=fitgibbs$par[, 1]
log.alpha.new=sir.old.new(log.alpha, prior, prior.new)

curve(prior, from=-3, to=5, ylim=c(0,0.7))
curve(prior.new, from=-3, to=5, add=TRUE, lty=2)
lines(density(log.alpha,bw=0.2),lwd=2)
lines(density(log.alpha.new,bw=0.2),lty=2,lwd=2)
legend("topleft", legend=c("priori original", "priori novo",
                           "posteriori original", "posteriori novo"),
       lty=c(1,2,1,2), lwd = c(1,1,2,2))

# não afetou a posteriori muito

# voltar às notas

## Preditiva da posteriori
# Hospital 94 teve 17 mortes, isso é muito?


with(hearttransplants, {
  # geram as lambdas para esse hospital
  lam94 <- rgamma(1000,y[94]+alpha,e[94]+alpha/mu)
  # geram possiveis valores de mortes
  ys94 <- rpois(1000,e[94]*lam94)
  # compara o valor observado com a distribuição de mortes
  hist(ys94,breaks=seq(-0.5,max(ys94)+0.5))
  lines(y[94]*c(1,1),c(0,100),lwd=3)
})

# está bem no meio da distribuição

# vamos calcular as probabilidades de ter tido um valor mais extremo
# que o observado para todas as hospitais
prob.out=function(i) with(hearttransplants, {
    lami=rgamma(1000,y[i]+alpha,e[i]+alpha/mu)
    ysi=rpois(1000,e[i]*lami)
    pleft=sum(ysi<=y[i])/1000
    pright=sum(ysi>=y[i])/1000
    min(pleft,pright)
    })
pout.exchange=sapply(1:94,prob.out)

# compara esses valores com os da taxa constante
plot(pout,pout.exchange,xlab="P(extreme), equal means",
     ylab="P(extreme), exchangeable")
abline(0,1)

# somente dois valores estão abaixo de 0.1.
# de fato isso pode indicar um modelo não tão bom, explicarei na aula.
