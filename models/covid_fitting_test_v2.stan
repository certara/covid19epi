#include function_seir_single.stan

data {
  real gamma1;
  real gamma2_1;
  real gamma2_2;
  real gamma2_3;
  real p_h;
  real p_s;
  real p_d;
  int Ntimes;
  real t[Ntimes];
  real y0[7];
  int estimate;
  int obs_cases[Ntimes];
}
transformed data {
  int x_i[3];
  real t0;
  real x_r[0];
  x_i[1] = 0;
  x_i[2] = 0;
  x_i[3] = 0;
  t0 = 0;
}

parameters {
  real<lower=0> beta;
  real<lower=0, upper=1> asc_pr[estimate];
}
transformed parameters {
  real theta[8];
  real y[Ntimes, 7];
  theta[1:8] = {beta, gamma1, gamma2_1, gamma2_2, gamma2_3, p_h, p_s, p_d};
  y = integrate_ode_bdf(SEIR, y0, t0, t, theta, x_r, x_i);
}
model {
  beta ~ normal(0.25, .1);
  if(estimate) 
    obs_cases ~ binomial(y[,3], asc_pr[1]);
}

generated quantities {
  // real y_sim[Ntimes, 7];
  // y_sim = integrate_ode_bdf(SEIR, y0, t0, t, theta, x_r, x_i);
}
