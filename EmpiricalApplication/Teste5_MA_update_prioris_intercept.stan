data {  // MA Pumi sem coeficientes e priori improprias para theta
    /* Outcome data */ 
     int<lower=0> N; 
     real<lower=0,upper=1> y[N]; // definindo que a variável resposta y é de tamanho N e seu intervalo varia entre 0 e 1
     real<lower=0> nu_alfa;
     real<lower=0> nu_beta;  
     int<lower=0> q; 

   // q=1 
     real theta_mean;
     real<lower=0> theta_sd;

   // q>1
   // real theta_mean[q]; // para o caso p>1
   // real<lower=0> theta_sd[q]; // para o caso p>1
     
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
     vector[q] theta;
 } 
  
 transformed parameters { 
    
    vector[q] tm;
    vector[N] eta_t;
    vector[N] mu; 
    
    vector[N] ma_sum;
    vector[N] rt; 

  
   eta_t[1]=0;
   mu[1]=inv_logit(eta_t[1]);
   rt[1]=logity[1]-mu[1];
   ma_sum[1]=0;


  // for(t in 2:N){
  // for(j in 1:p){
  //   if(t-j>0){ car[j] = ar_part[t-j];}
  //   else{car[j]=0;}
  //   tr[j]=phi[j]*car[j];
  //   print("Car=",car[j]);
  // }
  for(t in 2:N){
  //print("ArSum=",ar_sum[t]);
  // Depende de rt
  for(i in 1:q){
    if(t>i){ tm[i]=theta[i]*rt[t-i];}
    else{tm[i]=0;}
  }
  ma_sum[t] = sum(tm);
  eta_t[t] = alpha + ma_sum[t];
  mu[t] = inv_logit(eta_t[t]);
  rt[t] = logity[t] - eta_t[t];
}
} 
  
  
 model { 
  
      // Priori impropria para os phis da parte AR

  /* Priors */ 
     target += gamma_lpdf(nu|nu_alfa,nu_beta);
     target += uniform_lpdf(alpha|-1,1);

    // q=1
     target += normal_lpdf(theta | theta_mean, theta_sd);
     
  // q>1
   // for (k in 1:q){
   //  target += normal_lpdf(theta[k] | theta_mean[k], theta_sd[k]); 
//}


     /* Mean model */ 
     for (i in 1:(N)) { 
         target += beta_lpdf(y[i] | (mu * nu)[i], ((1-mu) * nu)[i]); // beta log posterior density function (lpdf). Temos o vetor y dados os parâmetros alfa=mu*phi e beta=(1-mu)*phi (padronização da dist beta). Seria a verossimilhança?
     } 
  
 } 
 
 generated quantities { 
  
 } 
