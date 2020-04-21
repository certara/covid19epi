library(tidyverse)
theme_set(theme_minimal())
# source("vaccination_rates.R")

load("transformed_data/demo_inputs.Rdata")

# Social mixing -----
# poly_contacts <- readRDS("transformed_inputs/poly_contacts.rds")
# poly_contacts_weighted <- readRDS("transformed_inputs/poly_contacts_weighted.rds")
# contact_matrix(polymod, countries="United Kingdom", age.limits = age_group_breaks, 
#                missing.participant.age = "keep", missing.contact.age = "keep")
# THIS IS NOW LOADED FROM DATA PREPARATION SCRIPT
# avg_contacts_number <- contact_matrix(polymod, age.limits = c(0), quiet = TRUE)$matrix[1,1]
avg_contacts_number <- 13.5

# Agr groups -----
age_group_names <- rownames(default_cm)
Ngroups <- length(age_group_names)


# Configure names of SEIR compartments -----
compartment_names <- c("S", "E", "As", "I1", "I2", "I3", "D", "R", "Im")
names(compartment_names) <- c("Susceptible", "Exposed", "Asymptomatic", "Infected (mild symptoms)", 
                              "Infected (sever symptoms)", "Critical care", 
                              "Deaths (cumulative)", "Recovered", "Immunised")


# Hospitalisation and death risk (Ferguson et al) -----

# % symptomatic cases requiring hospital care
# for 9 groups every 10 years, starting from 0 to 9, ending at 80 and over
i1_to_i2 <- c(0.1, 0.3, 1.2, 3.2, 4.9, 10.2, 16.6, 24.3, 27.3)/100
i2_to_i3 <- c(5.0, 5.0, 5.0, 5.0, 6.3, 12.2, 27.4, 43.2, 70.9)/100
i3_to_d  <- rep(0.5, 9) #50% of critical care die
# Assuming deaths from I1, I2 stages is very small difference to Ferguson et a l
ifr      <- c(0.002, 0.006, 0.03, 0.08, 0.15, 0.60, 2.2, 5.1, 9.3)/100
# i1_to_i2*i2_to_i3*i3_to_d - ifr



# Default values -----
default_seir_parameters <- list(
  Ngroups = 9, Nweeks = 52,
  group_names = age_group_names,
  contacts = default_cm, 
  r0 = 2.6,
  N = c("S" = 1 - 1e-06, "E" = 1e-06, "As" = 0, "I1" = 0, "I2" = 0, "I3" = 0, "D" = 0, "R" = 0, "Im" = 0),
  q = rep(2.6/(avg_contacts_number*6.5), Ngroups), #R0 (2.4) = contacts (13.48) * average length (6.5) * q
  gamma1    = rep(1/5.1, Ngroups), 
  gamma2_i1 = rep(1/6.5, Ngroups), 
  gamma2_i2 = rep(1/6.5, Ngroups), 
  gamma2_i3 = rep(1/6.5, Ngroups),
  delta     = rep(0, Ngroups),
  kappa     = rep(0, Ngroups),
  p_as      = rep(0.25, Ngroups),
  p_severe  = i1_to_i2,
  p_hosp    = i2_to_i3,
  p_death   = i3_to_d
)
