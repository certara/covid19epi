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
                                 Nweeks = 52,
                                 Ngroups = 9,
                                 r0 = NULL,
                                 y0 = NULL,
                                 N  = c("S" = 1 - 1e-05, "E" = 1e-05, "As" = 0, 
                                        "I1" = 0, "I2" = 0, "I3" = 0, "D" = 0, "R" = 0, "Im" = 0),
                                 contacts = matrix(13.5/Ngroups, Ngroups, Ngroups),
                                 contacts_scaling = matrix(1, Ngroups, Nweeks),
                                 group_names = paste("Group", 1:Ngroups),
                                 vaccination_rates = NA, #for now ignored
                                 # arguments that will be packed into theta
                                 q = rep(2.2/(13.5*6.5), Ngroups), 
                                 gamma1 = rep(1/5, Ngroups), 
                                 gamma2_i1 = rep(1/6.5, Ngroups), 
                                 gamma2_i2 = rep(1/6.5, Ngroups), 
                                 gamma2_i3 = rep(1/6.5, Ngroups),
                                 kappa     = rep(0, Ngroups), delta = rep(0, Ngroups),
                                 p_as     = rep(0, Ngroups),
                                 p_severe = rep(0, Ngroups),
                                 p_hosp   = rep(0, Ngroups),
                                 p_death  = rep(0, Ngroups)) {
  N_c <- 9 #number of compartments
  times <- c(1, seq(2, Nweeks*7 - 1, 2))
  
  
  # Initial state:
  if(is.null(y0)){
    # if(sum(N) != 1 && normaliseN){
    #   warning("normalised N's to sum to 1")
    #   N <- N/sum(N)
    # }
    y0 <- matrix(rep(N, Ngroups), N_c, Ngroups)
  }
  
  if(method == "desolve") {
    # beta <- q
    parms <- listN(q, gamma1, gamma2_i1, gamma2_i2, gamma2_i3, kappa, delta,
                   p_as, p_severe, p_hosp, p_death, contacts, Ngroups, N_c,
                   contacts_scaling)
    y <- run_covid_desolve(times, y0 = c(y0), parms = parms)
    times_ode <- y[,"time"]
    y <- y[,-1] #remove time column!
    y <- array(y, dim = c(length(times_ode), N_c, Ngroups),
               dimnames = list(times_ode, compartment_names_short, group_names))
    # dimnames(y) <- list(times_ode, compartment_names_short, group_names)
    return(y)
  } else if(method == "stan"){
    # This is now outdated. Do not use.
    
    # Set up for Stan:
    stan_inputs <- list(
      N_t = length(times), N_groups = Ngroups, N_weeks = Nweeks, 
      t = times, 
      x_r = c(contacts_scaling, contacts), 
      y0 = c(y0),
      theta = c(q, gamma1, gamma2_i1, gamma2_i2, gamma2_i3, p_severe, p_hosp, p_death)) 
    
    # Run 1 epidemic:
    fit <- rstan::sampling(covid_model, data=stan_inputs, algorithm = "Fixed_param", iter=1, chains=1, refresh=0)
    y <- rstan::extract(fit, "y")[[1]]
    # Extract outputs:
    y <- array(y, dim = c(dim(y)[1], dim(y)[2], N_c, Ngroups),
               dimnames = list(1:dim(y)[1], times, compartment_names_short, group_names))
    return(y[1,,,])
  }
}

# Y is a 3D matrix, with times x compartments x groups
# we can rescale each group by a vector pop_sizes (of length N groups)
# we can then also merge all groups into one
rescale_rcs <- function(y, pop_sizes=rep(1, dim(y)[3]), merge = FALSE) {
  y_new <- y*rep(pop_sizes, each = dim(y)[1]*dim(y)[2])
  if(merge)
    y_new <- replicate(1, apply(y_new, c(1,2), sum))
  y_new
}

plot_rcs <- function(y, compartment = "R", shade_weeks = c(0,0), 
                     start_date = as.Date("01-01-2020", format="%d-%m-%Y"), 
                     end_date = start_date + 300,
                     lab_type = "", overlay_data = NULL) {
  gg_data <- as.data.frame(y[,compartment,]) %>%
    rownames_to_column("time") %>%
    mutate(time = as.numeric(time)) %>%
    gather(age_group, value, -time)
  
  if(!is.null(start_date))
    gg_data$time <- as.Date(gg_data$time, origin = start_date)
  
  # Remove rows that we don't plot (to have the right Y scale)
  gg_data <- gg_data[gg_data$time <= as.Date(end_date),]
  
  gg <- ggplot() + 
    # geom_rect(aes(xmin = as.Date(7*shade_weeks[1], origin = start_date), 
    # xmax = as.Date(7*shade_weeks[2], origin = start_date),
    # ymin=0, ymax=Inf), fill = "skyblue", alpha = .2) +
    geom_line(data = gg_data, aes(x=time, y=value, group=age_group, color=age_group)) +
    {if(!is.null(start_date)) scale_x_date(limits = c(as.Date(start_date), as.Date(end_date)))} +
    labs(y = paste0(compartment_names[compartment], lab_type)) +
    {if(dim(y)[3] == 1) theme(legend.position = "none")} +
    scale_color_viridis_d()
  
  if(!is.null(overlay_data) && nrow(overlay_data) > 0){
    # overlay_data$time <- as.Date(overlay_data$time, origin = start_date)
    gg <- gg +
      geom_point(aes(x=time, y=value), data = overlay_data)
  }
  gg
}

