// Simple SEIR model in Stan
// Date: March 2020
// Author: Witold Wiecek

functions {
  real[] SEIR(real t,
                    real[] y,     // current states of different compartments 
                    real[] theta, // parameters of the epi model (minus vaccination/contacts)
                    real[] x_r,   // real vector with contact scaling matrix (N_groups x N_weeks)
                                  // followed by contacts (N_groups x N_groups)
                    int[] x_i) {  // integer vector c(N, N_groups, N_weeks)

    // unpack integer arguments
    int N_groups = x_i[1]; //age groups, then
    int N_weeks  = x_i[2]; //number of weeks
    int N_c      = x_i[3]; //N compartments

    //unpack parameters (theta vector)
    real q[N_groups]          = theta[1:(N_groups)];
    real gamma1[N_groups]     = theta[(1*N_groups+1):(2*N_groups)];
    real gamma2_i1[N_groups]  = theta[(2*N_groups+1):(3*N_groups)];
    real gamma2_i2[N_groups]  = theta[(3*N_groups+1):(4*N_groups)];
    real gamma2_i3[N_groups]  = theta[(4*N_groups+1):(5*N_groups)];
    real p_severe[N_groups]   = theta[(5*N_groups+1):(6*N_groups)];
    real p_hosp[N_groups]     = theta[(6*N_groups+1):(7*N_groups)];
    real p_death[N_groups]    = theta[(7*N_groups+1):(8*N_groups)];
    
    // helpers
    real dydt[N_c*N_groups];
    int i; //helper index
    real force; //for force of infection calculations
    //proportion of indivd.'s of different groups (helper variables for clarity of notation)
    real S; real E; real I1; real I2; real I3; real D; real R;
    
    /* For now this is disabled / set to 0 everywhere  ----- but it will be useful soon */
    //find which 30-day period we are in and apply the vaccination rate as necessary:
    /*
    real t2 = t - 30;
    int i_time = 1;
    real vacrates[N_groups];
    while(t2 > 0) {
      i_time = i_time + 1;
      t2 = t2 - 30;
    }
    vacrates = x_r[((i_time-1)*N_groups + 1):((i_time-1)*N_groups + N_groups)];
    */
    /* End of vaccinations */
    
    /* Contact reductions: vector of contacts scaling (in different groups) */
    //need to extract the appropriate indices
    real contacts_scaling[N_groups];
    real t2 = t - 7;
    int i_week = 1; //can't do floor() as that returns real
    while(t2 > 0) {
      i_week = i_week + 1;
      t2 = t2 - 7;
    }
    contacts_scaling = x_r[((i_week-1)*N_groups + 1):((i_week-1)*N_groups + N_groups)];
    
    
    //matrices programmed as 1D arrays
    //by using index i
    for(k in 1:N_groups) {
      i=(k-1)*N_c; //N_c = number of compartments per age group
      
      //force of infection in group i
      //q * sigma_i * sum_k(contacts_ik * I_k)
      force=0;
      for(j in 1:N_groups) {
        //we use I1 _temporarily_ to store _all_ infected  in group j
        I1 = y[N_c*(j-1) + 2] + y[N_c*(j-1) + 3] + y[N_c*(j-1) + 4] + y[N_c*(j-1) + 5];
        //we use column k of contacts matrix, because columns denote "contacts to ..."
        // transmis. * suscept. * proportion inf. in k * contacts with 'k'
        force += q[k] * (I1) * contacts_scaling[k] * x_r[(N_groups * N_weeks) + (j-1)*N_groups + k];
                 
        /* To clean up notation you can use this:
           real contacts[N_groups*N_groups] = 
             x_r[(N_groups*N_weeks + 1):(N_groups*N_weeks + N_groups*N_groups)];
        */
      }
      
      //store current proportions (y), for clarity of notation
      S    = y[i+1];
      E   = y[i+2]; I1 = y[i+3];
      I2   = y[i+4]; I3 = y[i+5];
      D = y[i+6]; R = y[i+7];
      
      //flows (dy)
      //Susceptible:
      dydt[i + 1] = - (force * S); // - (vacrates[k] * S); 
      //Exposed:
      dydt[i + 2] = + (force * S) - (gamma1[k] * E);
      //Infected, three stages:
      dydt[i + 3] = gamma1[k]*E - I1*gamma2_i1[k];
      dydt[i + 4] = gamma2_i1[k]*I1*p_severe[k] - gamma2_i2[k]*I2;
      dydt[i + 5] = gamma2_i2[k]*I2*p_hosp[k] - gamma2_i3[k]*I3;
      // Dead
      dydt[i + 6] = gamma2_i3[k]*I3*p_death[k];
      //Recovered
      dydt[i + 7] = gamma2_i1[k]*I1*(1-p_severe[k]) + gamma2_i2[k]*I2*(1-p_hosp[k]) + gamma2_i3[k]*I3*(1-p_death[k]); 
    }
    return dydt;
  }
}

data {
  int<lower=0> N_groups;
  int<lower=0> N_weeks;
  int<lower=0> N_t;
  real t[N_t];
  real x_r[(N_weeks*N_groups) + (N_groups*N_groups)];
  real theta[8*N_groups];
  real y0[7*N_groups]; //initial state of compartments
}

transformed data {
  int x_i[3];
  real t0;
  x_i[1] = N_groups;
  x_i[2] = N_weeks;
  x_i[3] = 7;
  t0 = 0;
}

model {

}

generated quantities {
  real y[N_t, 7*N_groups];
  y = integrate_ode_rk45(SEIR, y0, t0, t, theta, x_r, x_i);
}
