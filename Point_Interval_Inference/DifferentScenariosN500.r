# Com replicações 
library(rstan)
library(BTSR)
options(mc.cores = parallel::detectCores())


# Simulando ARMA (prioris normais para phi e theta)

# Cenário 1:
N=500
#N=200

alpha=0

nu1=100
phi1=c(0.4,0.6)
theta1=c(0.4,0.6)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

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
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}
########################################################################

# Cenário 2
N=500
#N=200

alpha=0

nu1=50
phi1=c(0.4,0.6)
theta1=c(0.4,0.6)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

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
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}


########################################################################

# Cenário 3
N=500
#N=200

alpha=0

nu1=50
phi1=c(-0.5,-0.3)
theta1=c(-0.3,-0.4)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

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
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}

########################################################################

# Cenário 4

N=500
#N=200

alpha=0

nu1=100
phi1=c(-0.5,-0.3)
theta1=c(-0.3,-0.4)
phi_dim=1 # pois modelo é BARMA(p=1,1)
theta_dim=1 # pois modelo é BARMA(1,q=1)

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
        
        save(list=c("mod","y","res"), file = paste(10*phi,"_",10*theta,"_",nu,"rep_",k,".Rdata", sep=""))
      }
      cat(i," - ",j,"\n")
    }
  }
}
