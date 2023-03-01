# EMPIRICAL HYDROLOGICAL APPLICATION - BARMA(1,1)

set.seed(2020)
u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

phi_dim=1 # pois p=1
theta_dim=1 # pois q=1

# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)


# Ordem
p=1 # ordem do AR
q=1 # ordem do MA
stan_logit_log_beta_model_fit1_aplicacao_barma11 <- stan(file = 'Testeeee_intercept.stan', chains = 4,
                                       data = list(N = N,
                                                   y = y,
                                                   #phi_dim=phi_dim,
                                                   #theta_dim=theta_dim,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   theta_mean=theta_mean,
                                                   theta_sd=theta_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   p=p,
                                                   q=q),
                                       verbose = TRUE)
summary(stan_logit_log_beta_model_fit1_aplicacao_barma11)$summary
traceplot(stan_logit_log_beta_model_fit1_aplicacao_barma11, pars = c("alpha","nu","phi", "theta"), inc_warmup = TRUE) # lp__ reune a informacao de todos os parâmetros
traceplot(stan_logit_log_beta_model_fit1_aplicacao_barma11, pars = c("alpha","nu","phi", "theta"),inc_warmup = TRUE, use_ggplot = TRUE) +
  scale_color_discrete(name = 'Chain')+ scale_color_manual(values=c("purple", "black","#1E90FF","#40E0D0")) 

# Estimativas no contexto clássico: alpha=0.3452, nu=11.7593, phi=0.5235 e theta=0.3588.
# Estimativas no contexto bayesiano: alpha=0.36, nu=10.73, phi=0.52 e theta=0.35 (4 chains, iter=2000, warmup=1000, prioris normais para phi e theta, priori gama para nu onde alfa=5 e beta=0.1 e priori uniforme para alpha, onde a= -1 e b=1).


library(shinystan)
launch_shinystan(stan_logit_log_beta_model_fit1_aplicacao_barma11)
stan_dens(stan_logit_log_beta_model_fit1_aplicacao_barma11,pars = c("alpha","nu","phi", "theta"),fill = "darkturquoise") # densidades estimadas 
#############################

# BARMA(1,2)

set.seed(2020)

u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

# Ordem
p=1 # ordem do AR
q=2 # ordem do MA


# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, p)
phi_sd <- rep(20000, p)
theta_mean <- rep(0, q)
theta_sd <- rep(20000, q)


stan_logit_log_beta_model_fit1_aplicacao_barma12 <- stan(file = 'Testeeee_Normal_generico.stan', chains = 4,
                                                         data = list(N = N,
                                                                     y = y,
                                                                     phi_mean=phi_mean,
                                                                     phi_sd=phi_sd,
                                                                     theta_mean=theta_mean,
                                                                     theta_sd=theta_sd,
                                                                     nu_alfa=nu_alfa,
                                                                     nu_beta=nu_beta,
                                                                     p=p,
                                                                     q=q),
                                                         verbose = TRUE)

#############################

# BARMA(2,1)

set.seed(2020)

u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

# Ordem
p=2 # ordem do AR
q=1 # ordem do MA


# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, p)
phi_sd <- rep(20000, p)
theta_mean <- rep(0, q)
theta_sd <- rep(20000, q)


stan_logit_log_beta_model_fit1_aplicacao_barma21 <- stan(file = 'Testeeee_Normal_generico.stan', chains = 4,
                                                         data = list(N = N,
                                                                     y = y,
                                                                     phi_mean=phi_mean,
                                                                     phi_sd=phi_sd,
                                                                     theta_mean=theta_mean,
                                                                     theta_sd=theta_sd,
                                                                     nu_alfa=nu_alfa,
                                                                     nu_beta=nu_beta,
                                                                     p=p,
                                                                     q=q),
                                                         verbose = TRUE)

#############################

# BARMA(1,0)

set.seed(2020)

u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

# Ordem
p=1 # ordem do AR
q=0 # ordem do MA


# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, p)
phi_sd <- rep(20000, p)


stan_logit_log_beta_model_fit1_aplicacao_barma10 <- stan(file = 'Teste5_AR_update_prioris_intercept.stan', chains = 4,
                                                         data = list(N = N,
                                                                     y = y,
                                                                     phi_mean=phi_mean,
                                                                     phi_sd=phi_sd,
                                                                     nu_alfa=nu_alfa,
                                                                     nu_beta=nu_beta,
                                                                     p=p,
                                                                     q=q),
                                                         verbose = TRUE)
stan_dens(stan_logit_log_beta_model_fit1_aplicacao_barma10,pars = c("alpha","nu","phi"),fill = "darkturquoise") # densidades estimadas 

#############################


# BARMA(0,1)

set.seed(2020)

u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

# Ordem
p=0 # ordem do AR
q=1 # ordem do MA


# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
theta_mean <- rep(0, q)
theta_sd <- rep(20000, q)


stan_logit_log_beta_model_fit1_aplicacao_barma01 <- stan(file = 'Teste5_MA_update_prioris_intercept.stan', chains = 4,
                                                         data = list(N = N,
                                                                     y = y,
                                                                     theta_mean=theta_mean,
                                                                     theta_sd=theta_sd,
                                                                     nu_alfa=nu_alfa,
                                                                     nu_beta=nu_beta,
                                                                     p=p,
                                                                     q=q),
                                                         verbose = TRUE)

#############################


#### Bayes Factor 

# Vamos considerar H0 o modelo BARMA(1,1), H1 os demais modelos (BARMA(1,2), BARMA(2,1), BARMA(1,0) e BARMA(0,1))

## Primeiro caso: BARMA(1,2)
set.seed(2020)

library(bridgesampling)
# compute log marginal likelihood via bridge sampling for H0
H0.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma11, silent = TRUE)

# compute log marginal likelihood via bridge sampling for H1
H1.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma12, silent = TRUE)

print(H0.bridge)
print(H1.bridge)

# compute percentage errors
H0.error <- error_measures(H0.bridge)$percentage
H1.error <- error_measures(H1.bridge)$percentage

print(H0.error)
print(H1.error)

# compute Bayes factor
BF_BARMA12 <- bf(H0.bridge, H1.bridge)
print(BF_BARMA12)
# Vemos acima que o valor 168772.83546 é muito superior à 1, evidenciando H0.

# compute posterior model probabilities (assuming equal prior model probabilities)
post1 <- post_prob(H0.bridge, H1.bridge)
print(post1)
#################

## Segundo caso: BARMA(2,1)

# compute log marginal likelihood via bridge sampling for H0
H0.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma11, silent = TRUE)

# compute log marginal likelihood via bridge sampling for H1
H1.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma21, silent = TRUE)

print(H0.bridge)
print(H1.bridge)

# compute percentage errors
H0.error <- error_measures(H0.bridge)$percentage
H1.error <- error_measures(H1.bridge)$percentage

print(H0.error)
print(H1.error)

# compute Bayes factor
BF_BARMA21 <- bf(H0.bridge, H1.bridge)
print(BF_BARMA21)
# Vemos acima que o valor 28456.67094 é muito superior à 1, evidenciando H0.

# compute posterior model probabilities (assuming equal prior model probabilities)
post1 <- post_prob(H0.bridge, H1.bridge)
print(post1)

#################

## Terceiro caso: BARMA(1,0)

# compute log marginal likelihood via bridge sampling for H0
H0.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma11, silent = TRUE)

# compute log marginal likelihood via bridge sampling for H1
H1.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma10, silent = TRUE)

print(H0.bridge)
print(H1.bridge)

# compute percentage errors
H0.error <- error_measures(H0.bridge)$percentage
H1.error <- error_measures(H1.bridge)$percentage

print(H0.error)
print(H1.error)

# compute Bayes factor
BF_BARMA10 <- bf(H0.bridge, H1.bridge)
print(BF_BARMA10)
# Vemos acima que o valor 28456.67094 é muito superior à 1, evidenciando H0.

# compute posterior model probabilities (assuming equal prior model probabilities)
post1 <- post_prob(H0.bridge, H1.bridge)
print(post1)


#################

## Quarto caso: BARMA(0,1)

# compute log marginal likelihood via bridge sampling for H0
H0.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma11, silent = TRUE)

# compute log marginal likelihood via bridge sampling for H1
H1.bridge <- bridge_sampler(stan_logit_log_beta_model_fit1_aplicacao_barma01, silent = TRUE)

print(H0.bridge)
print(H1.bridge)

# compute percentage errors
H0.error <- error_measures(H0.bridge)$percentage
H1.error <- error_measures(H1.bridge)$percentage

print(H0.error)
print(H1.error)

# compute Bayes factor
BF_BARMA01 <- bf(H0.bridge, H1.bridge)
print(BF_BARMA01)
# Vemos acima que o valor 28456.67094 é muito superior à 1, evidenciando H0.

# compute posterior model probabilities (assuming equal prior model probabilities)
post1 <- post_prob(H0.bridge, H1.bridge)
print(post1)


############################################################


# Forecast Code BARMA(1,1)

set.seed(2020)
h=6
u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

phi_dim=1 # pois p=1
theta_dim=1 # pois q=1



# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)



# Ordem
p=1 # ordem do AR
q=1 # ordem do MA

stan_logit_log_beta_model_fit1_aplicacao <- stan(file = 'Forecast.stan', 
                                       data = list(N = N,
                                                   h = h,
                                                   y = y,
                                                   #phi_dim=phi_dim,
                                                   #theta_dim=theta_dim,
                                                   phi_mean=phi_mean,
                                                   phi_sd=phi_sd,
                                                   theta_mean=theta_mean,
                                                   theta_sd=theta_sd,
                                                   nu_alfa=nu_alfa,
                                                   nu_beta=nu_beta,
                                                   p=p,
                                                   q=q),
                                       verbose = TRUE)

summary(stan_logit_log_beta_model_fit1_aplicacao, pars=c("alpha","nu", "phi", "theta","pred"))$summary
previsoes=summary(stan_logit_log_beta_model_fit1_aplicacao, pars=c("alpha","nu", "phi", "theta","pred"))$summary[c(5:10),1]
valores_reais=u[191:196] # dados verdadeiros (comparar com os resultados de pred acima)
res=BARFIMA.fit(y,p=1,q=1,d=FALSE,nnew = 6,m = 1) # caso classico BARMA(1,1) com intercepto
banco_previsoes_dadosreais=cbind(previsoes,valores_reais,classico=res$forecast)
banco_previsoes_dadosreais=as.data.frame(banco_previsoes_dadosreais)

# Não reportar gráfico no artigo
colors <- c("Observed" = "blue", "βARMA(1,1)" = "red", "Classic"= "magenta")

graf_prev <- ggplot(banco_previsoes_dadosreais, aes(x = c(1:6))) +
  geom_line(aes(y = valores_reais, color = "Observed"), size = 0.7) +
  geom_line(aes(y = previsoes, color = "βARMA(1,1)"), size = 0.7) +
  geom_line(aes(y = classico, color = "Classic"), size = 0.7) +
  labs(x = "", y = "Proportion of Stocked Energy", title = "Out-of-sample forecasts",color = "Models", ylim=c(0,1)) +
  scale_x_continuous(limit = c(1,6),
                     breaks = 1:6)
graf_prev + ylim(0.3, 0.85)


# Reportar MAE 
1/(1:6)*abs(cumsum(previsoes-valores_reais)) # nossas previsões BARMA(1,1) com intercepto
1/(1:6)*abs(cumsum(res$forecast-valores_reais)) # caso classico BARMA(1,1) com intercepto

###########################################

# Forecast BARMA(1,0) model

set.seed(2020)
h=6
u <-read.table("EA_mensal_2001a2016.txt", h=F)
u=as.numeric(u$"V1"/100)
y=u[1:190] # dados
n=length(y)
N=length(y)

phi_dim=1 # pois p=1


# Chutes iniciais
nu_alfa=5
nu_beta=0.1 
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)




# Ordem
p=1 # ordem do AR
q=0 # ordem do MA

stan_logit_log_beta_model_fit1_aplicacao_ar1 <- stan(file = 'Forecast_AR1.stan', 
                                                     data = list(N = N,
                                                                 h = h,
                                                                 y = y,
                                                                 #phi_dim=phi_dim,
                                                                 #theta_dim=theta_dim,
                                                                 phi_mean=phi_mean,
                                                                 phi_sd=phi_sd,
                                                                 nu_alfa=nu_alfa,
                                                                 nu_beta=nu_beta,
                                                                 p=p,
                                                                 q=q),
                                                     verbose = TRUE)
summary(stan_logit_log_beta_model_fit1_aplicacao_ar1, pars=c("alpha","nu", "phi","pred"))$summary



summary(stan_logit_log_beta_model_fit1_aplicacao_ar1, pars=c("alpha","nu", "phi","pred"))$summary
previsoes=summary(stan_logit_log_beta_model_fit1_aplicacao_ar1, pars=c("alpha","nu", "phi","pred"))$summary[c(4:9),1]
valores_reais=u[191:196] # dados verdadeiros (comparar com os resultados de pred acima)
res=BARFIMA.fit(y,p=1,q=0,d=FALSE,nnew = 6,m = 1) # caso classico BARMA(1,1) com intercepto
banco_previsoes_dadosreais=cbind(previsoes,valores_reais,classico=res$forecast)
banco_previsoes_dadosreais=as.data.frame(banco_previsoes_dadosreais)

# Não reportar gráfico no artigo
colors <- c("Observed" = "blue", "βARMA(1,0)" = "red", "Classic"= "magenta")

graf_prev <- ggplot(banco_previsoes_dadosreais, aes(x = c(1:6))) +
  geom_line(aes(y = valores_reais, color = "Observed"), size = 0.7) +
  geom_line(aes(y = previsoes, color = "βARMA(1,0)"), size = 0.7) +
  geom_line(aes(y = classico, color = "Classic"), size = 0.7) +
  labs(x = "", y = "Proportion of Stocked Energy", title = "Out-of-sample forecasts",color = "Models", ylim=c(0,1)) +
  scale_x_continuous(limit = c(1,6),
                     breaks = 1:6)
graf_prev + ylim(0, 1)


# Reportar MAE 
1/(1:6)*abs(cumsum(previsoes-valores_reais)) # nossas previsões BARMA(1,0) com intercepto
1/(1:6)*abs(cumsum(res$forecast-valores_reais)) # caso classico BARMA(1,0) com intercepto































# Obs: Root Code:

# Modelo BARMA(1,0)
stan_logit_log_beta_model_fit1_aplicacao_barma10
result_est=summary(stan_logit_log_beta_model_fit1_aplicacao_barma10,pars = c("alpha","nu","phi"))$summary
a=extract(stan_logit_log_beta_model_fit1_aplicacao_barma10, pars=c("alpha","nu","phi"),permuted=F,inc_warmup=T)
x=rbind(a[,1,3],a[,2,3],a[,3,3],a[,4,3]) # apenas a parte AR (phi1) - justamente para calcular as raízes - aqui juntamos as quatro cadeias (linhas 1-2000 são da cadeia 1, linhas 2001-4000 são da cadeia 2 e a mesma lógica segue para as cadeias 3 e 4)

#x=matrix(runif(8000), nrow=4000)
result13 = matrix(nrow = 8000, ncol = 1)
for(i in 1:8000){
  result13[i] = abs(polyroot(c(1,-x[i])))
}
sum(apply(result13,1,function(x) any(x<=1.05))) # 0 casos
mean(apply(result13,1,function(x) any(x<=1.05))) # 0



# Modelo BARMA(1,1)
stan_logit_log_beta_model_fit1_aplicacao_barma11
result_est=summary(stan_logit_log_beta_model_fit1_aplicacao_barma11,pars = c("alpha","nu","phi","theta"))$summary
a=extract(stan_logit_log_beta_model_fit1_aplicacao_barma11, pars=c("alpha","nu","phi","theta"),permuted=F,inc_warmup=T)
x=rbind(a[,1,3],a[,2,3],a[,3,3],a[,4,3]) # apenas a parte AR (phi1) - justamente para calcular as raízes - aqui juntamos as quatro cadeias (linhas 1-2000 são da cadeia 1, linhas 2001-4000 são da cadeia 2 e a mesma lógica segue para as cadeias 3 e 4)

#x=matrix(runif(8000), nrow=4000)
result14 = matrix(nrow = 8000, ncol = 1)
for(i in 1:8000){
  result14[i] = abs(polyroot(c(1,-x[i])))
}
sum(apply(result14,1,function(x) any(x<=1.05))) # 5 casos
mean(apply(result14,1,function(x) any(x<=1.05))) # 0.000625
