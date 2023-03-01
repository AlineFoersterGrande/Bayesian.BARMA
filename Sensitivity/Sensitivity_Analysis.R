# Simulando ARMA (prioris normais para phi e theta)

library(BTSR)
library(rstan)
options(mc.cores = parallel::detectCores())

# Cenário 1: alfa=0.002 e beta=0.002

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=0.002
nu_beta=0.002
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


#rep=2
rep=50

for(i in 1:length(phi1)){
  for(j in 1:length(theta1)){
    for(l in 1:length(nu1)){
      phi=phi1[i]
      theta=theta1[j]
      nu=nu1[l]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario1",".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}

####################


# Cenário 2: alfa=20 e beta=0.2

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=20
nu_beta=0.2
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


#rep=2
rep=50

for(i in 1:length(phi1)){
  for(j in 1:length(theta1)){
    for(l in 1:length(nu1)){
      phi=phi1[i]
      theta=theta1[j]
      nu=nu1[l]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario2",".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}

####################


# Cenário 3: alfa=0.0005 e beta=0.0005

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=0.0005
nu_beta=0.0005
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


#rep=2
rep=50

for(i in 1:length(phi1)){
  for(j in 1:length(theta1)){
    for(l in 1:length(nu1)){
      phi=phi1[i]
      theta=theta1[j]
      nu=nu1[l]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario3",".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}


####################


# Cenário 4: alfa=1.25 e beta=0.025

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=1.25
nu_beta=0.025
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


#rep=2
rep=50

for(i in 1:length(phi1)){
  for(j in 1:length(theta1)){
    for(l in 1:length(nu1)){
      phi=phi1[i]
      theta=theta1[j]
      nu=nu1[l]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario4",".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}


####################


# Cenário 5: alfa=5 e beta=0.05

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=5
nu_beta=0.05
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


#rep=2
rep=50

for(i in 1:length(phi1)){
  for(j in 1:length(theta1)){
    for(l in 1:length(nu1)){
      phi=phi1[i]
      theta=theta1[j]
      nu=nu1[l]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario5",".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}


###############


# Cenário 7: alfa=0.04 e beta=0.04

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
matriz_valores_parametros=matrix(NA, nrow=4,ncol=3)
matriz_valores_parametros[,1]=nu1
matriz_valores_parametros[,2]=phi1
matriz_valores_parametros[,3]=theta1
matriz_valores_parametros[3:4,1]=c(100,50)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=0.04
nu_beta=0.04
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


rep=2
#rep=50

for(i in 1:nrow(matriz_valores_parametros)){
      phi=matriz_valores_parametros[i,2]
      theta=matriz_valores_parametros[i,3]
      nu=matriz_valores_parametros[i,1]
      for(k in 1:rep){
        flag=T
        contador=0
        while(flag & contador<200){
          contador=contador+1
          semente=2000+k
          set.seed(semente)
          y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
          res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
          flag = ifelse(class(res)[1]=="try-error",T,F)
        }
        mod <- stan(file = 'Testeeee.stan', chains = 2,
                    data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                                theta_sd=theta_sd, nu_alfa=nu_alfa,
                                nu_beta=nu_beta,q=q,p=p),verbose = F)
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario7",".Rdata", sep=""))
      }
      cat(i," - ",i,"\n")
}



################################################################################



# Cenário 8: alfa=100 e beta=2

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
matriz_valores_parametros=matrix(NA, nrow=4,ncol=3)
matriz_valores_parametros[,1]=nu1
matriz_valores_parametros[,2]=phi1
matriz_valores_parametros[,3]=theta1
matriz_valores_parametros[3:4,1]=c(100,50)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=100
nu_beta=2
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


rep=2
#rep=50

for(i in 1:nrow(matriz_valores_parametros)){
  phi=matriz_valores_parametros[i,2]
  theta=matriz_valores_parametros[i,3]
  nu=matriz_valores_parametros[i,1]
  for(k in 1:rep){
    flag=T
    contador=0
    while(flag & contador<200){
      contador=contador+1
      semente=2000+k
      set.seed(semente)
      y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
      res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
      flag = ifelse(class(res)[1]=="try-error",T,F)
    }
    mod <- stan(file = 'Testeeee.stan', chains = 2,
                data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                            theta_sd=theta_sd, nu_alfa=nu_alfa,
                            nu_beta=nu_beta,q=q,p=p),verbose = F)
    
    save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario8",".Rdata", sep=""))
  }
  cat(i," - ",i,"\n")
}

################################################################################



# Cenário 9: alfa=400 e beta=4

N=200
#N=500

alpha=0

nu1=c(50,100)
phi1=c(0.4,-0.3)
theta1=c(0.6,-0.3)
matriz_valores_parametros=matrix(NA, nrow=4,ncol=3)
matriz_valores_parametros[,1]=nu1
matriz_valores_parametros[,2]=phi1
matriz_valores_parametros[,3]=theta1
matriz_valores_parametros[3:4,1]=c(100,50)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

# Chutes iniciais
nu_alfa=400
nu_beta=4
phi_mean <- rep(0, phi_dim)
phi_sd <- rep(20000, phi_dim)
theta_mean <- rep(0, theta_dim)
theta_sd <- rep(20000, theta_dim)

# Ordem
p=1 # ordem do AR
q=1 # ordem do MA


rep=2
#rep=50


for(i in 1:nrow(matriz_valores_parametros)){
  phi=matriz_valores_parametros[i,2]
  theta=matriz_valores_parametros[i,3]
  nu=matriz_valores_parametros[i,1]
  for(k in 1:rep){
    flag=T
    contador=0
    while(flag & contador<200){
      contador=contador+1
      semente=2000+k
      set.seed(semente)
      y=BARFIMA.sim(n=N,burn = 50,coefs = list(alpha=alpha,phi=phi,nu=nu,theta=theta))
      res=try(BARFIMA.fit(y,p=p,q=q,d=FALSE,report=F),silent = T)
      flag = ifelse(class(res)[1]=="try-error",T,F)
    }
    mod <- stan(file = 'Testeeee.stan', chains = 2,
                data = list(N = N,y = y,phi_mean=phi_mean,phi_sd=phi_sd,theta_mean=theta_mean,
                            theta_sd=theta_sd, nu_alfa=nu_alfa,
                            nu_beta=nu_beta,q=q,p=p),verbose = F)
    
    save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,"_Cenario9",".Rdata", sep=""))
  }
  cat(i," - ",i,"\n")
}

