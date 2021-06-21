## Primeira de três aulas sobre a estimação de modelos Bayesianos

## Instalar o pacote "LearnBayes"
#install.packages("LearnBayes")

## Carregar o pacote
library(LearnBayes)

## Priori discreta
p = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
windows()
plot(p, prior, type = "h", ylab="Prior Probability")

# volta aos slides

data = c(11, 16)
post = pdisc(p, prior, data)
round(cbind(p, prior, post),2)

library(lattice)
PRIOR=data.frame("prior",p,prior)
POST=data.frame("posterior",p,post)
names(PRIOR)=c("Type","P","Probability")
names(POST)=c("Type","P","Probability")
data=rbind(PRIOR,POST)
xyplot(Probability~P|Type,data=data,layout=c(1,2),type="h",lwd=3,col="black")

# volta aos slides

## Priori contínua (Distribuição Beta)
# Escolha a e b pelos quantis
quantile2=list(p=.9,x=.5)
quantile1=list(p=.5,x=.3)
beta.select(quantile1,quantile2)
# a = 3.26 e b = 7.19

# volta aos slides

a = 3.26
b = 7.19
s = 11
f = 16
curve(dbeta(x,a+s,b+f), from=0, to=1,xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,s+1,f+1),add=TRUE,lty=2,lwd=4)
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
legend(.7,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))

# volta aos slides

# probabilidade de mais da metade dormir 8h ou mais?
1 - pbeta(0.5, a + s, b + f)

# intervalo de credibilidade posteriori de 90%?
qbeta(c(0.05, 0.95), a + s, b + f)

# volta aos slides

# via Monte Carlo
# gera uma amostra da posteriori
ps = rbeta(1000, a + s, b + f)

# como é a densidade?
hist(ps,xlab="p",main="")

# probabilidade de mais da metade dormir 8h ou mais?
sum(ps >= 0.5)/1000
mean(ps>=0.5)

# intervalo de credibilidade posteriori de 90%?
quantile(ps, c(0.05, 0.95))

# volta aos slides

## Priori de Histograma
# usa os mesmos pesos da priori discreta, mas agora trata
# o peso como sendo associado ao intervalo. Assim, este
# algoritmo precisa de pesos e pontos médios para definir
# a priori.
midpt = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
curve(histprior(x,midpt,prior), from=0, to=1,ylab="Prior density",ylim=c(0,.3))

# calcular a posteriori
curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),from=0, to=1, ylab="Posterior density")

# para amostrar da posteriori, faça uma sequência de valores de 'p'
# com passos bem pequenos entre valores e calcular a posteriori para esse valores
# depois normaliza 
# e depois amostrar dessa distribuição discreta usando o comando 'sample'
p = seq(0, 1, length=500)
post = histprior(p, midpt, prior) * dbeta(p, s+1, f+1)
post = post/sum(post)
ps = sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="")
mean(ps >=0.5)
quantile(ps,c(0.05, 0.95))

# volta aos slides

# probabilidade preditiva com distribuição (priori ou posteriori) discreta
p=seq(0.05, 0.95, by=.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior=prior/sum(prior)
m=20; ys=0:20
pred=pdiscp(p, prior, m, ys)
round(cbind(0:20,pred),3)

# volta aos slides

# probabilidade preditiva com priori Beta
ab=c(3.26, 7.19)
m=20; ys=0:20
predb=pbetap(ab, m, ys)
round(cbind(0:20,pred,predb),3)

# volta aos slides

# probabilidade preditiva via simulação
# amostra os 'p's
p=rbeta(1000, 3.26, 7.19)
# amostra os 'y's
y = rbinom(1000, 20, p)
# gera a distribuição dos 'y's
table(y)
freq=table(y)
ys=as.integer(names(freq))
predprob=freq/sum(freq)
plot(ys,predprob,type="h",xlab="y",ylab="Predictive Probability")

# volta aos slides

# resumos da distribuição preditiva
# a função 'discint' precisa da distribuição preditiva
dist = cbind(ys,predprob)
dist
# também precisa da probabilidade mínima de cobertura (90%)
covprob = 0.9
# retorna o intervalo e sua probabilidade
discint(dist,covprob)
# nesse caso tem uma chance de 91.2% do número de alunos que dormem
# 8h ou mais numa amostre de 20 alunos ser entre 1 e 11.
# ou seja a amostra terá entre 5% e 55% de sucessos com chance de 91.2%.

# volta aos slides

# estimação da variância (futebol americano)
data(footballscores)
with(footballscores, {d <<- favorite - underdog - spread} )
n = length(d)
v = sum(d^2)
# simular P = Qui-quadrado/v
P = rchisq(1000, n)/v
# verificar a distribuição de sigma
s = sqrt(1/P)
hist(s,main="")

# podemos obter uma estimativa pontual e intervalo de confiança
# pelos quantis
quantile(s, probs = c(0.025, 0.5, 0.975))

# volta aos slides

# Taxa de Mortalidade (Transplantes)
# Considere um hospital com uma morte em 66 exposições
# Primeiro alpha e beta dos dez hospitais semelhantes
alpha=16;beta=15174
# os dados observados
yobs=1; ex=66
# os valores possíveis
y=0:10
# estimativa priori de lambda
lam=alpha/beta
# distribuição preditiva de lambda
py=dpois(y, lam*ex)*
  dgamma(lam, shape = alpha,rate = beta)/
  dgamma(lam, shape= alpha + y,rate = beta + ex)
cbind(y, round(py, 3))
# o valor observado (y=1) está no meio da distribuição, então não temos
# razões para duvidar do modelo.

# Vamos guardar 1000 amostras da posteriori para hospital A
lambdaA = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

# Agora considera um outro hospital com mais mortes e exposições
ex = 1767; yobs=4
# os valores possíveis
y = 0:10
py = dpois(y, lam * ex) * 
  dgamma(lam, shape = alpha,rate = beta)/
  dgamma(lam, shape = alpha + y,rate = beta + ex)
cbind(y, round(py, 3))

# de novo, podemos ver que 4 está no meio da distribuição preditiva

# Vamos guardar 1000 amostras da posteriori para hospital B
lambdaB = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

par(mfrow = c(2, 1))
plot(density(lambdaA), main="HOSPITAL A",xlab="lambdaA", lwd=3)
curve(dgamma(x, shape = alpha, rate = beta), add=TRUE)
legend("topright",legend=c("prior","posterior"),lwd=c(1,3))
plot(density(lambdaB), main="HOSPITAL B",xlab="lambdaB", lwd=3)
curve(dgamma(x, shape = alpha, rate = beta), add=TRUE)
legend("topright",legend=c("prior","posterior"),lwd=c(1,3))

# volta às notas

## Robustez Bayesiano
# QI do João
quantile1=list(p=.5,x=100); quantile2=list(p=.95,x=120)
normal.select(quantile1, quantile2)

# volta às notas

# Estimativas para três resultados diferentes dos dados
mu = 100
tau = 12.16
sigma = 15
n = 4
se = sigma/sqrt(4)
ybar = c(110, 125, 140)
tau1 = 1/sqrt(1/se^2 + 1/tau^2)
mu1 = (ybar/se^2 + mu/tau^2) * tau1^2
summ1=cbind(ybar, mu1, tau1)
summ1

# volta às notas

# Distribuição T com dois graus de liberdade (para 90% dos dados entre
# 80 e 120 o parâmetro de escala é dado abaixo)
tscale = 20/qt(0.95, 2)
tscale
# comparação das distribuições Normal e T com 2 graus de liberdade
par(mfrow=c(1,1))
curve(1/tscale*dt((x-mu)/tscale,2),from=60, to=140, xlab="theta", ylab="Prior Density")
curve(dnorm(x,mean=mu,sd=tau), add=TRUE, lwd=3)
legend("topright",legend=c("t density","normal density"),lwd=c(1,3))

# volta às notas
# estimativas da posteriori para T usando uma aproximação discreta.
# Segue uma função que faz o produto direto entre priorio e verossimilhança:
norm.t.compute=function(ybar) {
  theta = seq(60, 180, length = 500)
  like = dnorm(theta,mean=ybar,sd=sigma/sqrt(n))
  prior = dt((theta - mu)/tscale, 2)
  post = prior * like
  post = post/sum(post)
  m = sum(theta * post)
  s = sqrt(sum(theta^2 * post) - m^2)
  c(ybar, m, s) }
summ2=t(sapply(c(110, 125, 140),norm.t.compute))
dimnames(summ2)[[2]]=c("ybar","mu1 t","tau1 t")
summ2

cbind(summ1,summ2)

# comparar as densidades posterioris para mu igual 140
theta=seq(60, 180, length=500)
normpost = dnorm(theta, mu1[3], tau1)
normpost = normpost/sum(normpost)
plot(theta,normpost,type="l",lwd=3,ylab="Posterior Density")
like = dnorm(theta,mean=140,sd=sigma/sqrt(n))
prior = dt((theta - mu)/tscale, 2)
tpost = prior * like / sum(prior * like)
lines(theta,tpost)
legend("topright",legend=c("t prior","normal prior"),lwd=c(1,3))

# para observações consistentes com a priori, não há grande
# diferença entre a Normal e a T. Mas quando os dados observados são
# distantes do esperado, os resultados divergem.

# volta às notas

# Mistura de distribuições conjugadas
probs=c(.5,.5)
beta.par1=c(6, 14)
beta.par2=c(14, 6)
betapar=rbind(beta.par1, beta.par2)
# isso representa g(p) = 0.5 beta(6,14) + 0.5 beta(14,6)
data=c(7,3)
post=binomial.beta.mix(probs,betapar,data)
post
# g(p|data) = 0.093 beta(13, 17) + 0.907 beta(21, 9).

# compara a priori e a posteriori
curve(post$probs[1]*dbeta(x,13,17)+post$probs[2]*dbeta(x,21,9),
      from=0, to=1, lwd=3, xlab="P", ylab="DENSITY")
curve(.5*dbeta(x,6,12)+.5*dbeta(x,12,6),0,1,add=TRUE)
legend("topleft",legend=c("Prior","Posterior"),lwd=c(1,3))

# volta às notas

# Probabilidade de uma moeda justa
# (no que segue eu não entendo porque avaliar m1 somente em p=0.5)
n = 20
y = 5
a = 10
p = 0.5
m1 = dbinom(y, n, p) * dbeta(p, a, a)/dbeta(p, a + y, a + n - y)
lambda = dbinom(y, n, p)/(dbinom(y, n, p) + m1)
lambda

# a função 'pbetat' faz o mesmo cálculo
pbetat(p,.5,c(a,a),c(y,n-y))

# será que a escolha de 'a' afeta a decisão?
prob.fair=function(log.a)
  {
    a = exp(log.a)
    m2 = dbinom(y, n, p) * dbeta(p, a, a)/
    dbeta(p, a + y, a + n - y)
    dbinom(y, n, p)/(dbinom(y, n, p) + m2)
}

n = 20; y = 5; p = 0.5
curve(prob.fair(x), from=-4, to=5, xlab="log a", 
      ylab="Prob(coin is fair)", lwd=2)

# a decisão não é sensivel ao valor de a

# volta às notas

# usando a probabilidade de ser menor ou igual a 5, não somente igual a 5
n=20
y=5
a=10
p=.5
m2=0
for (k in 0:y)
 m2=m2+dbinom(k,n,p)*dbeta(p,a,a)/dbeta(p,a+k,a+n-k)
lambda=pbinom(y,n,p)/(pbinom(y,n,p)+m2)
lambda
# H0 é menos provável.

# volta às notas

# Mais que um parâmetro
# Normal com média e variância desconhecidas
data(marathontimes)
with(marathontimes, { 
  d <<- mycontour(normchi2post, c(220, 330, 500, 9000), time, xlab="mean",ylab="variance")
})

# Simular a posteriori e colocar os pontos na curva de nível
with(marathontimes, {
  S <<- sum((time - mean(time))^2)
  n <<- length(time)
  sigma2 <<- S/rchisq(1000, n - 1)
  mu <<- rnorm(1000, mean = mean(time), sd = sqrt(sigma2)/sqrt(n))
  points(mu, sigma2)
})

# Intervalos de credibilidade para cada parâmetro
quantile(mu, c(0.025, 0.975))
quantile(sqrt(sigma2), c(0.025, 0.975))

# voltar às notas

# Simulando a fração que apoia Bush
alpha = c(728, 584, 138)
theta = rdirichlet(1000, alpha)
hist(theta[, 1] - theta[, 2], main="")
# A distribuição parece concentrada acima de zero
mean(theta[, 1] > theta[, 2])

# voltar às notas

# Votos Eleitorais Para Obama
# votos para ganhar
538/2+1
data(election.2008)
# Escreve uma função que simula do Dirichlet daquele estado e dá a
# probabilidade de Obama ganhar
with(election.2008, {
prob.Obama=function(j){
  p=rdirichlet(5000, 500*c(M.pct[j],O.pct[j],100-M.pct[j]-O.pct[j])/100+1)
  mean(p[,2]>p[,1])
}
  Obama.win.probs = sapply(1:51,prob.Obama)
  # simula 1000 lançamentos de 51 dados viesadas e conta o número de
  # votos ganhos pelo Obama
  sim.election=function()
    {
      winner=rbinom(51,1,Obama.win.probs)
      sum(EV*winner)
  }
  sim.EV <<- replicate(1000,sim.election())
})

hist(sim.EV,min(sim.EV):max(sim.EV),col="blue")
abline(v=365,lwd=3) # Obama received 365 votes
text(375,30,"Actual \n Obama \n total")
abline(v=270,lwd=3) # Precisa de 270 votos para ganhar

# voltar às notas

## Comparação de duas proporções possívelmente correlacionadas
# Prioris de Howard com sigmas diferentes
sigma=c(2,1,.5,.25)
plo=.0001;phi=.9999
par(mfrow=c(2,2))
for (i in 1:4)
 mycontour(howardprior,c(plo,phi,plo,phi),c(1,1,1,1,sigma[i]),
              main=paste("sigma=",as.character(sigma[i])),
              xlab="p1",ylab="p2")

# Posterioris para os dados de Pearson com sigmas diferentes
sigma=c(2,1,.5,.25)
par(mfrow=c(2,2))
for (i in 1:4)
{
 mycontour(howardprior,c(plo,phi,plo,phi),
 c(1+3,1+15,1+7,1+5,sigma[i]),
 main=paste("sigma=",as.character(sigma[i])),
 xlab="p1",ylab="p2")
 lines(c(0,1),c(0,1))
}

# A função 'simcontour' simula da posteriori, depois podemos
# contar a fração de vezes que p1>p2 para cada valor de sigma
pp1gtp2 = numeric(4)
for (i in 1:4) {
s=simcontour(howardprior,c(plo,phi,plo,phi),
               c(1+3,1+15,1+7,1+5,sigma[i]),1000)
pp1gtp2[i] = sum(s$x>s$y)/1000
}
cbind(sigma,pp1gtp2)
