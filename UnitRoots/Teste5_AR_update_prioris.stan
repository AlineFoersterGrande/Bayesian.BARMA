data {  // AR Pumi sem coeficientes e prioris Normais para phi 
    /* Outcome data */ 
     int<lower=0> N; 
     real<lower=0,upper=1> y[N]; // definindo que a variável resposta y é de tamanho N e seu intervalo varia entre 0 e 1
     real<lower=0> nu_alfa;
     real<lower=0> nu_beta;
     int<lower=0> p; 
     int<lower=1> phi_dim;
     real phi_mean[phi_dim];
     real<lower=0> phi_sd[phi_dim];

 } 
  
 transformed data { 
   vector[N] logity;
   for (i in 1:N){
   
      logity[i] = logit(y[i]);
      }
} 
  
 parameters { 
     real<lower=0> nu; // nu grande, variância pequena, pois é parâmetro de precisão
     //vector<lower=-0.5,upper=0.5> [p] phi;
     vector[p] phi;
 } 
  
 transformed parameters { 
    
    vector[p] tr;
    vector[N] ar_part = logity;
    vector[N] eta_t;
    vector[N] mu; 
    
    vector[N] ar_sum;

  
   eta_t[1]=0;
   mu[1]=inv_logit(eta_t[1]);
   ar_sum[1]=0;


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
  eta_t[t] = ar_sum[t];
  mu[t] = inv_logit(eta_t[t]);
}
} 
  
  
 model {  
  

  /* Priors */ 
     for (j in 1:phi_dim) { // definindo as prioris para os parâmetros do modelo de mi (betas), ou seja, se tivermos por exemplo 2 covariáveis explicativas então teremos 3 parâmetros (beta0, beta1 e beta2), logo 3 prioris
         target += normal_lpdf(phi[j] | phi_mean[j], phi_sd[j]); // log probability density function. y dado média e desvio padrão
     }
  
    // target += normal_lpdf(phi | phi_mean, phi_sd);
     target += gamma_lpdf(nu|nu_alfa,nu_beta);
     

     /* Mean model */ 
     for (i in 1:(N)) { 
         target += beta_lpdf(y[i] | (mu * nu)[i], ((1-mu) * nu)[i]); // beta log posterior density function (lpdf). Temos o vetor y dados os parâmetros alfa=mu*phi e beta=(1-mu)*phi (padronização da dist beta). Seria a verossimilhança?
     } 
  
 } 
 
 generated quantities { 
  
 } 
