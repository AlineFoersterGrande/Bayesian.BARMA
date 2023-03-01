set.seed(24)

# Cenário 1: (phi1=-0.3 e phi2=-0.9)
n=500
nu=100 # nu verdadeiro
phi=c(-0.3,-0.9) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit1 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)


stan_logit_log_beta_model_fit1
result_est=summary(stan_logit_log_beta_model_fit1,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit1, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result1 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result1[i,] = abs(polyroot(c(1,-x[i,])))
}

sum(apply(result1,1,function(x) any(x<=1))) # 7 casos

mean(apply(result1,1,function(x) any(x<=1)))

mean(apply(result1,1,function(x) any(x<=1.05)))
mean(apply(result1,1,function(x) any(x<=1.1)))
boxplot(result1)
abline(h=1.054,col="red")

mean(apply(result1,1,function(x) any(x<=1.01))) 
mean(apply(result1,1,function(x) any(x<=1.02))) 
mean(apply(result1,1,function(x) any(x<=1.03))) 
mean(apply(result1,1,function(x) any(x<=1.04))) 
mean(apply(result1,1,function(x) any(x<=1.05))) 




# Cenário 2: (phi1=-0.1 e phi2=0.8)
n=500
nu=100 # nu verdadeiro
phi=c(-0.1,0.8) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit2 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit2
result_est=summary(stan_logit_log_beta_model_fit2,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit2, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result2 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result2[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result2,1,function(x) any(x<=1.1))) # 18 casos
mean(apply(result2,1,function(x) any(x<=1.05))) 

mean(apply(result2,1,function(x) any(x<=1.01))) 
mean(apply(result2,1,function(x) any(x<=1.02))) 
mean(apply(result2,1,function(x) any(x<=1.03))) 
mean(apply(result2,1,function(x) any(x<=1.04))) 
mean(apply(result2,1,function(x) any(x<=1.05))) 




# Cenário 3: (phi1=0.4 e phi2=0.5)
n=500
nu=100 # nu verdadeiro
phi=c(0.4,0.5) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit3 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit3
result_est=summary(stan_logit_log_beta_model_fit3,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit3, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result3 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result3[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result3,1,function(x) any(x<=1.1))) # 2 casos
mean(apply(result3,1,function(x) any(x<=1.05))) 

mean(apply(result3,1,function(x) any(x<=1.01))) 
mean(apply(result3,1,function(x) any(x<=1.02))) 
mean(apply(result3,1,function(x) any(x<=1.03))) 
mean(apply(result3,1,function(x) any(x<=1.04))) 
mean(apply(result3,1,function(x) any(x<=1.05))) 





# Cenário 4: (phi1=0.1 e phi2=0.8)
n=500
nu=100 # nu verdadeiro
phi=c(0.1,0.8) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit4 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit4
result_est=summary(stan_logit_log_beta_model_fit4,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit4, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result4 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result4[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result4,1,function(x) any(x<=1))) # 10 casos
mean(apply(result4,1,function(x) any(x<=1.05))) 
mean(apply(result4,1,function(x) any(x<=1.1))) 

mean(apply(result4,1,function(x) any(x<=1.01))) 
mean(apply(result4,1,function(x) any(x<=1.02))) 
mean(apply(result4,1,function(x) any(x<=1.03))) 
mean(apply(result4,1,function(x) any(x<=1.04))) 
mean(apply(result4,1,function(x) any(x<=1.05))) 






# Cenário 5: (phi1=-0.9 e phi2=-0.6)
n=500
nu=100 # nu verdadeiro
phi=c(-0.9,-0.6) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit5 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit5
result_est=summary(stan_logit_log_beta_model_fit5,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit5, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result5 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result5[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result5,1,function(x) any(x<=1))) # 0 casos
mean(apply(result5,1,function(x) any(x<=1.05))) 
mean(apply(result5,1,function(x) any(x<=1.1))) 


mean(apply(result5,1,function(x) any(x<=1.01))) 
mean(apply(result5,1,function(x) any(x<=1.02))) 
mean(apply(result5,1,function(x) any(x<=1.03))) 
mean(apply(result5,1,function(x) any(x<=1.04))) 
mean(apply(result5,1,function(x) any(x<=1.05))) 






# Cenário 6: (phi1=0.1 e phi2=-0.3)
n=500
nu=100 # nu verdadeiro
phi=c(0.1,-0.3) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit6 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit6
result_est=summary(stan_logit_log_beta_model_fit6,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit6, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result6 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result6[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result6,1,function(x) any(x<=1))) # 0 casos

mean(apply(result6,1,function(x) any(x<=1.05))) 
mean(apply(result6,1,function(x) any(x<=1.1))) 

mean(apply(result6,1,function(x) any(x<=1.01))) 
mean(apply(result6,1,function(x) any(x<=1.02))) 
mean(apply(result6,1,function(x) any(x<=1.03))) 
mean(apply(result6,1,function(x) any(x<=1.04))) 
mean(apply(result6,1,function(x) any(x<=1.05))) 







# Cenário 7: (phi1=0.3 e phi2=0.1)
n=500
nu=100 # nu verdadeiro
phi=c(0.3,0.1) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit7 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit7
result_est=summary(stan_logit_log_beta_model_fit7,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit7, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result7 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result7[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result7,1,function(x) any(x<=1))) # 9 casos

mean(apply(result7,1,function(x) any(x<=1.05))) 
mean(apply(result7,1,function(x) any(x<=1.1))) 

mean(apply(result7,1,function(x) any(x<=1.01))) 
mean(apply(result7,1,function(x) any(x<=1.02))) 
mean(apply(result7,1,function(x) any(x<=1.03))) 
mean(apply(result7,1,function(x) any(x<=1.04))) 
mean(apply(result7,1,function(x) any(x<=1.05))) 






# Cenário 8: (phi1=0.6 e phi2=-0.1)
n=500
nu=100 # nu verdadeiro
phi=c(0.6,-0.1) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)


# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit8 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit8
result_est=summary(stan_logit_log_beta_model_fit8,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit8, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result8 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result8[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result8,1,function(x) any(x<=1))) # 3 casos

mean(apply(result8,1,function(x) any(x<=1.05))) 
mean(apply(result8,1,function(x) any(x<=1.1))) 


mean(apply(result8,1,function(x) any(x<=1.01))) 
mean(apply(result8,1,function(x) any(x<=1.02))) 
mean(apply(result8,1,function(x) any(x<=1.03))) 
mean(apply(result8,1,function(x) any(x<=1.04))) 
mean(apply(result8,1,function(x) any(x<=1.05))) 







# Cenário 9: (phi1=0.2 e phi2=0.75) - poster
n=500
nu=100 # nu verdadeiro
phi=c(0.2,0.75) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)

# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit9 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                       data = list(N = N,
                                                   y = y,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   phi_dim=phi_dim,
                                                   p=p),
                                       verbose = TRUE)

stan_logit_log_beta_model_fit9
result_est=summary(stan_logit_log_beta_model_fit9,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit9, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result9 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result9[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result9,1,function(x) any(x<=1))) # 12 casos

mean(apply(result9,1,function(x) any(x<=1.1))) 

mean(apply(result9,1,function(x) any(x<=1.01))) 
mean(apply(result9,1,function(x) any(x<=1.02))) 
mean(apply(result9,1,function(x) any(x<=1.03))) 
mean(apply(result9,1,function(x) any(x<=1.04))) 
mean(apply(result9,1,function(x) any(x<=1.05))) 






# Cenário 10: (phi1=-0.25 e phi2=-0.95) - poster
n=500
nu=100 # nu verdadeiro
phi=c(-0.25,-0.95) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)

# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit10 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                        data = list(N = N,
                                                    y = y,
                                                    phi_mean=phi_mean,
                                                    phi_sd=phi_sd,
                                                    nu_alfa=nu_alfa,
                                                    nu_beta=nu_beta,
                                                    phi_dim=phi_dim,
                                                    p=p),
                                        verbose = TRUE)

stan_logit_log_beta_model_fit10
result_est=summary(stan_logit_log_beta_model_fit10,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit10, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result10 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result10[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result10,1,function(x) any(x<=1))) # 7 casos

mean(apply(result10,1,function(x) any(x<=1.1))) 


mean(apply(result10,1,function(x) any(x<=1.01))) 
mean(apply(result10,1,function(x) any(x<=1.02))) 
mean(apply(result10,1,function(x) any(x<=1.03))) 
mean(apply(result10,1,function(x) any(x<=1.04))) 
mean(apply(result10,1,function(x) any(x<=1.05))) 






# Cenário 11: (phi1=-0.15 e phi2=0.8)
n=500
nu=100 # nu verdadeiro
phi=c(-0.15,0.8) # phis verdadeiros
alpha=0
theta=0
y=BARFIMA.sim(n=n,burn = 30,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
y
N=length(y)
res1=BARFIMA.fit(y,p=2,q=0,d=FALSE) 

phi_dim=length(phi)

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)

# Ordem
p=2 # ordem do AR
stan_logit_log_beta_model_fit11 <- stan(file = 'Teste5_AR_update_prioris.stan', chains = 2,
                                        data = list(N = N,
                                                    y = y,
                                                    phi_mean=phi_mean,
                                                    phi_sd=phi_sd,
                                                    nu_alfa=nu_alfa,
                                                    nu_beta=nu_beta,
                                                    phi_dim=phi_dim,
                                                    p=p),
                                        verbose = TRUE)

stan_logit_log_beta_model_fit11
result_est=summary(stan_logit_log_beta_model_fit11,pars = c("nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit11, pars=c("nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,2:3],a[,2,2:3]) # apenas a parte AR (phi1 e phi2) - justamente para calcular as raízes - aqui juntamos as duas cadeias (linhas 1-2000 são da cadeia 1 e linhas 2001-4000 são da cadeia 2)

#x=matrix(runif(8000), nrow=4000)
result11 = matrix(nrow = 4000, ncol = 2)
for(i in 1:4000){
  result11[i,] = abs(polyroot(c(1,-x[i,])))
}
sum(apply(result11,1,function(x) any(x<=1))) # 17 casos

mean(apply(result11,1,function(x) any(x<=1.05))) 
mean(apply(result11,1,function(x) any(x<=1.1))) 


mean(apply(result11,1,function(x) any(x<=1.01))) 
mean(apply(result11,1,function(x) any(x<=1.02))) 
mean(apply(result11,1,function(x) any(x<=1.03))) 
mean(apply(result11,1,function(x) any(x<=1.04))) 
mean(apply(result11,1,function(x) any(x<=1.05))) 
