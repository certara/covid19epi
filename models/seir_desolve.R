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
    for(k in 1:Ngroups){
      # j <- (k-1)*7
      i <- (k-1)*N_c
      S    <- init[i+1];
      E    <- init[i+2]; I1 <- init[i+3];
      I2   <- init[i+4]; I3 <- init[i+5];
      D    <- init[i+6]; R  <- init[i+7];
      
      force <- 0
      for(j in 1:Ngroups)
        force <- force + q[k] * contacts[j,k] * (init[N_c*(j-1) + 3] + init[N_c*(j-1) + 4] + init[N_c*(j-1) + 5])
      
      ll[i + 1]  <- -force*S
      ll[i + 2]  <- force*S - gamma1[k]*E
      ll[i + 3]  <- gamma1[k]*E - gamma2_i1[k]*I1
      ll[i + 4]  <- gamma2_i1[k]*I1*p_hosp[k] - gamma2_i2[k]*I2
      ll[i + 5]  <- gamma2_i2[k]*I2*p_severe[k]  - gamma2_i3[k]*I3
      ll[i + 6]  <- gamma2_i3[k]*I3*p_death[k]
      ll[i + 7]  <- gamma2_i3[k]*I3*(1-p_death[k]) + gamma2_i1[k]*I1*(1-p_hosp[k]) + gamma2_i2[k]*I2*(1-p_severe[k])
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
  parms$Ngroups <- 9
  parms$N_c <- 7
  # y0_long <- y0
  # for(i in 1:8)
    # y0_long <- append(y0_long, y0)
  lsoda(y0,times,sir_ode_ages,parms)
  
}
