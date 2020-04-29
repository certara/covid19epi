server <- shinyServer(function(input, output, session) {
  # inputs_to_manipulate will later be generated based on external file config, to make this more 
  # dynamically customisable
  inputs_to_manipulate <- list(
    list("sliderInput", list("r0", label = "r0 (approximate)", 
                             min = 0.1, max = 4, value = default_seir_parameters$r0, step = .1)),
    # list("selectInput", 
    # list("country", label = "Social mixing patterns", 
    # choices = c("All countries (recommended)", levels(polymod$participants$country)))),
    list("sliderInput", list("inv_gamma1", label = "Length of latent period", 
                             min = 1, max = 14, step = 0.1, value = 1/default_seir_parameters$gamma1)),
    list("sliderInput", list("inv_gamma2", label = "Length of symptoms period", 
                             min = 1, max = 14, step = 0.1, value = 1/default_seir_parameters$gamma2_i1)),
    list("sliderInput", list("p_as", label = "Proportion asymptomatic", 
                             min = 0, max = 1, step = 0.01, value = mean(default_seir_parameters$p_as))))
  
  output$panel1_settings <- renderUI({
    lapply(inputs_to_manipulate, function(x) {
      do.call(x[[1]], x[[2]])
    })
  })
  
  seir_pars_no_i <- reactive({
    convert_settings_to_par(input, default_seir_parameters)
  })
  
  seir_pars_nonpi <- reactive({
    pars <- seir_pars_no_i()
    
    if(!is.null(input$add_npi_toggle)){
      if(input$add_npi_toggle == "basic"){
        if(!is.null(input$add_intervention_prop) && !is.null(input$add_intervention_scaling))
          pars <- add_npi(pars, 
                          prop = input$add_intervention_prop/100, 
                          scaling_const = 1 - input$add_intervention_scaling/100)
      }
      
      if(input$add_npi_toggle == "detailed"){
        ngroups <- length(pars$group_names)
        prop <- vector(length = ngroups); scale <- vector(length = ngroups)
        for(i in 1:ngroups){
          if(!is.null(input[[paste0("add_npi_prop",  i)]]) && 
             !is.null(input[[paste0("add_npi_scale",  i)]])){
            
            prop[i] <- input[[paste0("add_npi_prop",  i)]]
            scale[i] <- input[[paste0("add_npi_scale",  i)]]
          }
        }
        pars <- add_npi(pars, 
                        prop = prop/100, 
                        scaling_const = 1 - scale/100)
      }
    }
    pars
  })
  
  
  
  
  # Starting population -----
  output$set_starting_ui <- renderUI({
    if(input$set_starting_toggle == 0)
      return(selectInput("initial_infected_prop", label = "Initial infected proportion",
                         choices = c("One in million" = 1e-06, "Five per million" = 5e-06,
                                     "One per 100,000" = 1e-05, "One per 10,000" = 1e-04,
                                     "One per 1,000" = 1e-03), selected = 1e-04))
    if(input$set_starting_toggle == 1)
      return(NULL)
  })
  
  
  seir_model <- reactive({
    if(is.null(seir_pars_nonpi()))
      return(NULL)
    
    pars <- seir_pars_nonpi()
    
    # Add pharmaceutical interventions
    # Prophylaxis (immunised at start)
    if(input$add_pi_toggle == "basic_pro"){
      if(is.null(input$add_pro_efficacy) || is.null(input$add_pro_use) || is.null(input$add_pro_length))
        return(NULL)
      Im <- (input$add_pro_use/100)*(input$add_pro_efficacy/100)
      if(length(pars$N) == 9){
        pars$N <- (1-Im)*pars$N
        pars$N[["Im"]] <- Im
        pars$kappa <- rep(1/(input$add_pro_length*7), pars$Ngroups)
      }
    }
    # Vaccination (over time)
    if(input$add_pi_toggle == "vac"){
      if(is.null(input$add_vac_use) || is.null(input$add_vac_rate) || 
         is.null(input$add_vac_length) || is.null(input$add_vac_efficacy))
        return(NULL)
      Im <- (input$add_vac_use/100)*(input$add_vac_efficacy/100)
      if(length(pars$N) == 9){
        pars$N <- (1-Im)*pars$N
        pars$N[["Im"]] <- Im
        pars$kappa <- rep(1/(input$add_vac_length*(365/12)), pars$Ngroups)
        pars$delta <- rep((input$add_vac_rate/100)*(input$add_vac_efficacy/100), pars$Ngroups)
      }
    }

    
    
    # pars$method <- "stan"
    do.call(run_covid_simulation, pars)
  })
  
  output$panel1_plot <- renderPlot({
    if(is.null(seir_model()))
      return(NULL)
    
    y <- seir_model()
    
    scale <- as.numeric(pbc_spread[input$demographics,])
    lt <- " individuals"
    
    if(input$panel1_scaling == "pct"){
      lt <- " (%)"
      if(input$panel1_dnmerge_groups)
        scale <- rep(1, dim(y)[3]) #everything is just treated as 1
      else
        scale <- scale/sum(scale) #normalise the distribution over age groups
    }
    if(input$panel1_scaling == "per100k"){
      lt <- " (per 100,000)"
      if(input$panel1_dnmerge_groups)
        scale <- rep(100000, dim(y)[3]) #everything is just treated as 1
      else
        scale <- 100000*scale/sum(scale) #normalise the distribution over age groups
    }
    
    if(!is.null(input$add_intervention_prop) && input$add_npi_toggle == "basic"){
      pr <- input$add_intervention_prop/100
      scale <- c((1-pr)*scale, (pr)*scale)
    }
    
    y <- rescale_rcs(y, merge = !input$panel1_dnmerge_groups, pop_sizes = scale)
    
    gg <- plot_rcs(y, input$panel1_output_selector, 
             start_date = input$start_date, end_date = input$start_date + input$panel1_xlim, 
             lab_type = lt) 
    
    if(input$panel1_scaling == "pct")
      gg <- gg + scale_y_continuous(label = scales::label_percent())
    else
      gg <- gg + scale_y_continuous(label = scales::label_number_si())
    
    gg
  })
  
  
  
  # UI renders ------
  
  output$add_npi_ui <- renderUI({
    if(!is.null(input$add_npi_toggle)){
      if(input$add_npi_toggle == "basic")
        return(list(
          sliderInput("add_intervention_prop", "To what % of population?", 
                      min = 0, max = 100, value = 50),
          sliderInput("add_intervention_scaling", "How much to decrease contacts? (in %)", 
                      min = 0, max = 100, value = 50)
        ))
      if(input$add_npi_toggle == "detailed"){
        pars <- seir_pars_no_i()
        ll <- list()
        for(i in 1:length(pars$group_names)) {
          ll[[paste0("add_npi_prop", i)]] <- 
            sliderInput(paste0("add_npi_prop", i), 
                        label = paste0("Proportion: ", pars$group_names[i]), min = 0, max = 100, value = 100)
          ll[[paste0("add_npi_scale", i)]] <- 
            sliderInput(paste0("add_npi_scale", i), 
                        label = paste0("Scaling: ", pars$group_names[i]), min = 0, max = 100, value = 100)
        }
        return(ll)
        # return(do.call(flowLayout, ll))
      }
    }
    return(NULL)
    
    
  })
  output$add_pi_ui <- renderUI({
    if(input$add_pi_toggle == "detailed_pro"){
      pars <- seir_pars_nonpi()
      ll <- lapply(as.list(pars$group_names), function(x) {
        sliderInput(paste0("add_pi_", which(pars$group_names == x)), label = x, min = 0, max = 1, value = 0)
      })
      return(do.call(flowLayout, ll))
    }
    
    if(input$add_pi_toggle == "basic_pro")
      return(list(
        sliderInput("add_pro_use", "% of population immunised",
                    min = 0, max = 100, value = 5),
        sliderInput("add_pro_length", "Average length of immunity (weeks)", 
                    min = 0, max = 52, value = 5, step = 1),
        sliderInput("add_pro_efficacy", "Efficacy (% effectively immunised)", 
                    min = 0, max = 100, value = 50)
      ))
    
    if(input$add_pi_toggle == "vac")
      return(list(
        sliderInput("add_vac_use", "% of population initialy immunised",
                    min = 0, max = 100, value = 0),
        sliderInput("add_vac_rate", "Immunisation rate (% population per month)",
                    min = 0, max = 100, value = 0),
        sliderInput("add_vac_length", "Average length of immunity (months)", 
                    min = 0, max = 60, value = 12, step = 1),
        sliderInput("add_vac_efficacy", "Efficacy (% effectively immunised)", 
                    min = 0, max = 100, value = 70)
      ))
    
    if(input$add_pi_toggle == "basic_at")
      return(list(
        sliderInput("add_at_use_key", "% of NPI ('key workers') treated",
                    min = 0, max = 100, value = 5),
        sliderInput("add_at_use_nonkey", "% of non-NPI population treated",
                    min = 0, max = 100, value = 0),
        HTML("<i>If no NPI is used, we assume that all population is 'non-key'</i>"),
        sliderInput("add_at_gamma", "Average duration of illness in treated", 
                    min = 0, max = 52, value = 5, step = 1)
      ))
    
    if(input$add_pi_toggle == "no")
      return(NULL)
  })
  
  output$plot_demographics <- renderPlot({
    N <- pbc_spread[input$demographics,]/1e06
    ggplot(gather(N), aes(x=key, y=value)) + 
      geom_bar(fill = "cornflowerblue", stat = "identity") + 
      labs(x="", y="N (million)")
  })
})
