functions {
  real[] SEIR(real t,
                    real[] y,     
                    real[] theta, 
                    real[] x_r,   
                    int[] x_i) {  

    // unpack integer arguments
    // int N_groups = x_i[1]; //age groups, then
    // int N_weeks  = x_i[2]; //number of weeks
    // int N_c      = x_i[3]; //N compartments

    //unpack parameters (theta vector)
    real beta           = theta[1];
    real gamma1         = theta[2];
    real gamma2_i1      = theta[3];
    real gamma2_i2      = theta[4];
    real gamma2_i3      = theta[5];
    real p_hosp         = theta[6];
    real p_severe       = theta[7];
    real p_death        = theta[8];
    
    // helpers
    real dydt[7];
    
    //flows (dy)
    dydt[1] = -(beta * y[1]);
    dydt[2] = +(beta * y[1]) - (gamma1 * y[2]);
    dydt[3] = gamma1*y[2] - y[3]*gamma2_i1;
    dydt[4] = gamma2_i1*y[3]*p_severe - gamma2_i2*y[4];
    dydt[5] = gamma2_i2*y[4]*p_hosp - gamma2_i3*y[5];
    dydt[6] = gamma2_i3*y[5]*p_death;
    dydt[7] = gamma2_i1*y[3]*(1-p_severe) + gamma2_i2*y[4]*(1-p_hosp) + gamma2_i3*y[5]*(1-p_death); 

    return dydt;
  }
}
