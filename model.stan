data {
  int<lower=1> ng;
  int<lower=1> nt;
  int x[ng,2];
  int y[ng,2];
  real prior_o_mean[nt];
  real prior_o_sd[nt];
  real prior_d_mean[nt];
  real prior_d_sd[nt];
  real prior_h_mean;
  real prior_h_sd;
}
  
parameters {
  real o[nt];
  real d[nt];
  real h;
}
  
model {
  int t1;
  int t2;
  real eta1;
  real eta2;
  real nu1;
  real nu2;
  // priors
  o~normal(prior_o_mean,prior_o_sd);
  d~normal(prior_d_mean,prior_d_sd);
  h~normal(prior_h_mean,prior_h_sd);
  // likelihood
  for (i in 1:ng)
  { 
    t1=x[i,1];
    t2=x[i,2];
    nu1=h+o[t1]-d[t2];
    nu2=o[t2]-d[t1];
    eta1=exp(nu1);
    eta2=exp(nu2);
    y[i,1]~poisson(eta1);
    y[i,2]~poisson(eta2);
  }
}  
