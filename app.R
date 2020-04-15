#	Shiny app for COVID-19 SEIR model
# By Witold Wiecek
# April 2020



# loading libraries -----
library(shiny)
library(rstan)
library(devtools)
# install_github("wwiecek/stanseir")

# set-up code -----
source("default_covid_parameters.R")
source("covid_model_function.R")
source("covid_interventions.R")
source("helper_functions.R")

convert_settings_to_par <- function(settings, pars) {
  Ngroups <- pars[["Ngroups"]]
  
  if(!is.null(settings$inv_gamma1))  
    pars[["gamma1"]] <- rep(1/settings$inv_gamma1, Ngroups)
  if(!is.null(settings$inv_gamma2))
    pars[["gamma2_i1"]] <- rep(1/settings$inv_gamma2, Ngroups)
  
  # R0 = q*avg_contacts*gamma2
  if(!is.null(settings$r0))  
    pars[["q"]] <- settings$r0/(13.5*(1/pars[["gamma2_i1"]]))
  
  # Contacts
  if(!is.null(settings$country))
    if(settings$country != "All countries (recommended)")
    pars[["contacts"]] <- contact_matrix(polymod, 
                                         countries = settings$country,
                                         age.limits = c(0, 10, 20, 30, 40, 50, 60, 70, 80))$matrix
  
    # Initially infected proportion
  if(!is.null(settings$initial_infected_prop)){
    pars[["initial_infected_prop"]] <- as.numeric(settings$initial_infected_prop)
  }
  # Other settings which do not need conversion:
  # ... for now this is empty 
    
  pars
}
  



# running the NMA tool -----
source("shiny_server.R", local=TRUE)
source("shiny_ui.R", local=TRUE)
shinyApp(server = server, ui = ui)
