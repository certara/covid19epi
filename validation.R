# Validation of the deSolve models

# Age-specific model is the same as seir ODE model:
t <- 1:300
p <- list(beta=2.2/(6.5), q = 2.2/(13.5*6.5),
           gamma1=1/5.1, gamma2_i1=1/6.5, gamma2_i2=1/6.5, gamma2_i3=1/6.5, 
           p_severe=0, p_hosp=0, p_death=0, contacts=matrix(13.5,1,1), Ngroups=1, N_c=7)
initial_infected_prop <- 1e-05

y0 <- matrix(0, 7, 1)
y0[1,] <- rep(1,1) - initial_infected_prop
y0[2,] <- initial_infected_prop

# p <- listN(q, gamma1, gamma2_i1, gamma2_i2, gamma2_i3, p_severe, p_hosp, p_death, contacts, Ngroups)
seir_ode(t,c(1-2e-07,2e-07,0,0,0,0,0),p)


# Same result for both:
lsoda(y0,t,seir_ode,p)
lsoda(y0,t,seir_ode_ages,p) 
plot(lsoda(y0,t,seir_ode,p))

# Now introduce realistic age structure -----
p_age <- default_seir_parameters
p_age$N_c <- 7
y0 <- matrix(0, 7, 9)
y0[1,] <- rep(1,9) - initial_infected_prop
y0[2,] <- initial_infected_prop
lsoda(y0,t,seir_ode_ages,p_age) 

y <- run_covid_simulation()
plot_rcs(y, compartment = "R")
y <- run_covid_simulation(contacts = default_cm)
plot_rcs(y, compartment = "R")

# This is also OK.



