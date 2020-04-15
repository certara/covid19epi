# A simple deterministic SEIR model re-programmed with deSolve for simplicity

# BTW see here for some good ideas of stochastic modelling: 
# http://epirecip.es/epicookbook/chapters/sir-stochastic-discretestate-continuoustime/r

# Odin also has some examples:
# https://github.com/mrc-ide/odin/blob/master/tests/testthat/examples/seir_odin.R

library(deSolve)

sir_ode <- function(times,init,parms){
  with(as.list(c(parms,init)), {
    dS  <- -beta*S*I1
    dE  <- beta*S*I1 - gamma1*E
    dI1 <- gamma1*E - gamma2_i1*I1
    dI2 <- gamma2_i1*I1*p_hosp - gamma2_i2*I2
    dI3 <- gamma2_i2*I2*p_severe  - gamma2_i3*I3
    dR  <- gamma2_i3*I3*(1-p_death) + gamma2_i1*I1*(1-p_hosp) + gamma2_i2*I2*(1-p_severe)
    dD  <- gamma2_i3*I3*p_death
    list(c(dS,dE, dI1,dI2, dI3, dR, dD))
  })
}

sir_ode_ages <- function(times,init,parms){
  with(as.list(c(parms)), {
    ll <- vector(length = length(init))
    
      
      # for(j in 1:N_groups) {
      #   //we use I1 _temporarily_ to store _all_ infected  in group j
      #   I1 = y[N_c*(j-1) + 2] + y[N_c*(j-1) + 3] + y[N_c*(j-1) + 4] + y[N_c*(j-1) + 5];
      #   //we use column k of contacts matrix, because columns denote "contacts to ..."
      #   // transmis. * suscept. * proportion inf. in k * contacts with 'k'
      #   force += q[k] * (I1) * contacts_scaling[k] * x_r[(N_groups * N_weeks) + (j-1)*N_groups + k];
      #   
      #   /* To clean up notation you can use this:
      #     real contacts[N_groups*N_groups] = 
      #     x_r[(N_groups*N_weeks + 1):(N_groups*N_weeks + N_groups*N_groups)];
      #   */
      # }
    

    
    for(i in 1:Ngroups){
      j <- (i-1)*7
      S    <- init[j+1];
      E    <- init[j+2]; I1 <- init[j+3];
      I2   <- init[j+4]; I3 <- init[j+5];
      D    <- init[j+6]; R  <- init[j+7];
      
      force <- beta[i]
      
      ll[(i-1)*7 + 1]  <- -force*S*I1
      ll[(i-1)*7 + 2]  <- force*S*I1 - gamma1[i]*E
      ll[(i-1)*7 + 3]  <- gamma1[i]*E - gamma2_i1[i]*I1
      ll[(i-1)*7 + 4]  <- gamma2_i1[i]*I1*p_hosp[i] - gamma2_i2[i]*I2
      ll[(i-1)*7 + 5]  <- gamma2_i2[i]*I2*p_severe[i]  - gamma2_i3[i]*I3
      ll[(i-1)*7 + 6]  <- gamma2_i3[i]*I3*(1-p_death[i]) + gamma2_i1[i]*I1*(1-p_hosp[i]) + gamma2_i2[i]*I2*(1-p_severe[i])
      ll[(i-1)*7 + 7]  <- gamma2_i3[i]*I3*p_death[i]
    }
    list(ll)
  })
}
# times <- seq(0,200,length.out=2001)
# sir_out <- lsoda(init,times,sir_ode,parms)


run_covid_desolve <- function(times = 1:100, 
                              y0 = c(S=1, E=1e-06, I1=0, I2=0, I3=0, R=0, D=0), 
                              parms) {
  # Without age groups:
  # parms <- lapply(parms, function(x) if(length(x) > 1) x <- x[1])
  # lsoda(y0,times,sir_ode,parms)
  # With age groups:
  # parms <- lapply(parms, function(x) if(length(x) > 1) x <- x[1])
  parms$Ngroups <- 9
  y0_long <- y0
  for(i in 1:8)
    y0_long <- append(y0_long, y0)
  lsoda(y0_long,times,sir_ode_ages,parms)
  
}
