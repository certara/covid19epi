# This scripts contains one function -- used to run SEIR model compiled inside Stan
# The function's role is to provide a consistent API for inputs (SEIR parameters) and 
# outputs - an array of counts over time in different compartments
library(rstan)
rstan_options(auto_write = TRUE)

# Where the models are loaded:
# covid_model <-  rstan::stan_model('models/covid_seiiir_dev_v2.stan')
# library(stanseir)
# covid_model <- stanmodels$covid_seiiir_dev_v2
source("models/seir_desolve.R")

# Setting up parameters for the models:
run_covid_simulation <- function(method = "desolve", 
                                 Nweeks = 26,
                                 Ngroups = 9,
                                 contacts = matrix(13.5/Ngroups, Ngroups, Ngroups),
                                 contacts_scaling = matrix(1, Ngroups, Nweeks),
                                 group_names = paste("Group", 1:Ngroups),
                                 initial_infected_prop = 2e-07,
                                 vaccination_rates = NA, #for now ignored
                                 # arguments that will be packed into theta
                                 q = rep(.011, Ngroups), 
                                 gamma1 = rep(1/5, Ngroups), 
                                 gamma2_i1 = rep(1/5, Ngroups), 
                                 gamma2_i2 = rep(1/5, Ngroups), 
                                 gamma2_i3 = rep(1/5, Ngroups),
                                 p_severe = rep(0, Ngroups),
                                 p_hosp = rep(0, Ngroups),
                                 p_death = rep(0, Ngroups)) {
  Ncompartments <- 7
  times <- c(1, seq(2, Nweeks*7, 2))
  
  # Initial state:
  y0 <- matrix(0, Ncompartments, Ngroups)
  y0[1,] <- rep(1,Ngroups) - initial_infected_prop
  y0[2,] <- initial_infected_prop
  
  if(method == "desolve") {
    # beta <- q
    parms <- listN(q, gamma1, gamma2_i1, gamma2_i2, gamma2_i3, p_severe, p_hosp, p_death, contacts)
    y <- run_covid_desolve(times, y0 = c(y0), parms = parms)
    times_ode <- y[,"time"]
    y <- y[,-1] #remove time column!
    y <- array(y, dim = c(length(times_ode), Ncompartments, Ngroups),
               dimnames = list(times_ode, compartment_names, group_names))
    # dimnames(y) <- list(times_ode, compartment_names, group_names)
    return(y)
  } else if(method == "stan"){
    # Set up for Stan:
    stan_inputs <- list(
      N_t = length(times), N_groups = Ngroups, N_weeks = Nweeks, #=0 because for now we omit vaccination_rates
      t = times, 
      x_r = c(contacts_scaling, contacts), 
      y0 = c(y0),
      theta = c(q, gamma1, gamma2_i1, gamma2_i2, gamma2_i3, p_severe, p_hosp, p_death)) 
    
    # Run 1 epidemic:
    fit <- rstan::sampling(covid_model, data=stan_inputs, algorithm = "Fixed_param", iter=1, chains=1, refresh=0)
    y <- rstan::extract(fit, "y")[[1]]
    # Extract outputs:
    y <- array(y, dim = c(dim(y)[1], dim(y)[2], Ncompartments, Ngroups),
               dimnames = list(1:dim(y)[1], times, compartment_names, group_names))
    return(y[1,,,])
  }
}

rescale_rcs <- function(y, pop_sizes) {
  y*rep(pop_sizes, each = dim(y)[1]*dim(y)[2])
}

plot_rcs <- function(y, compartment = "R", shade_weeks = c(0,0), 
                     start_date = NULL, end_date = start_date + 300) {
  gg_data <- as.data.frame(y[,compartment,]) %>%
    rownames_to_column("time") %>%
    mutate(time = as.numeric(time)) %>%
    gather(age_group, prevalence, -time)
  
  if(!is.null(start_date))
    gg_data$time <- as.Date(gg_data$time, origin = start_date)
  
  ggplot(gg_data) + 
    # geom_rect(aes(xmin = as.Date(7*shade_weeks[1], origin = start_date), 
    # xmax = as.Date(7*shade_weeks[2], origin = start_date),
    # ymin=0, ymax=Inf), fill = "skyblue", alpha = .2) +
    geom_line(aes(x=time, y=prevalence, group=age_group, color=age_group)) +
    {if(!is.null(start_date)) scale_x_date(limits = c(as.Date(start_date), as.Date(end_date)))} +
    labs(y = compartment) +
    scale_color_viridis_d()
}

