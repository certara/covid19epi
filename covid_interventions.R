
# Possible policies:

# School closure policy (OPTIONAL)
# Isolation of severe cases
# Isolation of high risk cases
# Isolation of symptomatic cases
# Social distancing for all (BASELINE)


# Partition all age groups into 2:
# Some proportion without intervention
# Some proportion with intervention

add_npi <- function(params, prop = 0, scaling_const = 1){
  Nint <- 2
  new_int <- params
  Ngroups <- params$Ngroups
  
  if(is.null(Ngroups))
    stop("Need Ngroups to add interventions.")
  # browser()
  if(length(prop) == 1)
    prop <- rep(prop, Ngroups)
  if(length(scaling_const) == 1)
    scaling_const <- rep(scaling_const, Ngroups)
  if(any(prop > 1) || any(scaling_const > 1) || any(prop < 0) || any(scaling_const < 0)
     || length(prop) != Ngroups || length(scaling_const) != Ngroups)
    stop("Wrong inputs to intervention function")
  
  split   <- rbind(1 - prop, prop)
  scaling <- rbind(rep(1, Ngroups), scaling_const)
  
  # split <- matrix(c(1-prop, prop), Nint, Ngroups)
  # scaling <- matrix(c(1, scaling_const), Nint, Ngroups)
  
  new_int$contacts <- rbind(
    cbind(new_int$contacts*split[1,]*scaling[1,], new_int$contacts*split[2,]*scaling[1,]),
    cbind(new_int$contacts*split[1,]*scaling[2,], new_int$contacts*split[2,]*scaling[2,]))
  new_int$Ngroups <- Nint*Ngroups
  new_int$group_names <- c(paste(new_int$group_names, ", no intervention"),
                           paste(new_int$group_names, ", with intervention"))
  new_int <- lapply(new_int, function(x) if(length(x) == 9) return(c(x,x)) else return(x))
}
# ww <- do.call(run_covid_simulation, add_one_int())
# plot_rcs(ww, "D", start_date = "2020-01-01", end_date = "2020-05-01")



# Adding pharmaceutical interventions -----

add_prophylaxis_fixed <- function(params, prop, efficacy) {
  params$prop
}

