data {  // ARMA sem coeficientes e prioris normais para phi e theta - p>1 e q>1
    /* Outcome data */ 
     int<lower=0> N; 
     real<lower=0,upper=1> y[N]; // definindo que a variável resposta y é de tamanho N e seu intervalo varia entre 0 e 1
     real<lower=0> nu_alfa;
     real<lower=0> nu_beta;
     int<lower=0> p; 
     int<lower=0> q; 
     
     // p=1 e q>1
  //  real phi_mean; // para o caso p=1
  //  real<lower=0> phi_sd; // para o caso p=1
  //  real theta_mean[q]; // para o caso q>1
  //  real<lower=0> theta_sd[q]; // para o caso q>1

     
     // q=1 e p>1
    real theta_mean; // para o caso q=1
    real<lower=0> theta_sd; // para o caso q=1
    real phi_mean[p]; // para o caso p>1
    real<lower=0> phi_sd[p]; // para o caso p>1

} 
  
  
 transformed data {
   vector[N] logity;
   for (i in 1:N){
   
      logity[i] = logit(y[i]);
      }
} 
  
 parameters { 
     real alpha; 
     real<lower=0> nu; // nu grande, variância pequena, pois é parâmetro de precisão
     //vector<lower=-0.5,upper=0.5> [p] phi;
     vector[p] phi;
     vector[q] theta;
 } 
  
 transformed parameters { 
    
    vector[q] tm;
    vector[p] tr;
    vector[N] ar_part = logity;
    vector[N] eta_t;
    vector[N] mu; 
    
    vector[N] ar_sum;
    vector[N] ma_sum;
    vector[N] rt; 

  
   eta_t[1]=0;
   mu[1]=inv_logit(eta_t[1]);
   rt[1]=logity[1]-mu[1];
   ar_sum[1]=0;
   ma_sum[1]=0;


  // for(t in 2:N){
  // for(j in 1:p){
  //   if(t-j>0){ car[j] = ar_part[t-j];}
  //   else{car[j]=0;}
  //   tr[j]=phi[j]*car[j];
  //   print("Car=",car[j]);
  // }
  for(t in 2:N){
    for(j in 1:p){
      if(t-j>0){ tr[j] = phi[j]*ar_part[t-j];}
      else{tr[j]=0;}
      //print("tr=",tr[j]);
    }
    ar_sum[t]=sum(tr);
  //print("ArSum=",ar_sum[t]);
  // Depende de rt
  for(i in 1:q){
    if(t>i){ tm[i]=theta[i]*rt[t-i];}
    else{tm[i]=0;}
  }
  ma_sum[t] = sum(tm);
  eta_t[t] =  alpha + ar_sum[t] + ma_sum[t];
  mu[t] = inv_logit(eta_t[t]);
  rt[t] = logity[t] - eta_t[t];
}
} 
  
  
  
 model {  
      /* Priors */ 
    target += uniform_lpdf(alpha|-1,1);  
    target += gamma_lpdf(nu|nu_alfa,nu_beta);
    
    
    
  // p=1 e q>1
 // target += normal_lpdf(phi | phi_mean, phi_sd);

 // for (k in 1:q){
   // target += normal_lpdf(theta[k] | theta_mean[k], theta_sd[k]); 
//}      
 


  // p>1 e q=1
   target += normal_lpdf(theta | theta_mean, theta_sd);
     
   for (j in 1:p){
     target += normal_lpdf(phi[j] | phi_mean[j], phi_sd[j]); 
}
         

     



     /* Mean model */ 
     for (i in 1:(N)) { 
         target += beta_lpdf(y[i] | (mu * nu)[i], ((1-mu) * nu)[i]); // beta log posterior density function (lpdf). Temos o vetor y dados os parâmetros alfa=mu*phi e beta=(1-mu)*phi (padronização da dist beta). Seria a verossimilhança?
     } 
  
 } 
 
 generated quantities { 
  
 } 
