server <- shinyServer(function(input, output, session) {
  # inputs_to_manipulate will later be generated based on external file config, to make this more 
  # dynamically customisable
  inputs_to_manipulate <- list(
    list("sliderInput", list("r0", label = "r0 (approximate)", 
                             min = 0.1, max = 4, value = 2.4, step = .1)),
    list("selectInput", 
         list("country", label = "Social mixing patterns", 
              choices = c("All countries (recommended)", levels(polymod$participants$country)))),
    list("sliderInput", list("inv_gamma1", label = "Length of incubation period", 
                             min = 1, max = 14, step = 0.1, value = 5.1)),
    list("sliderInput", list("inv_gamma2", label = "Length of infectious period", 
                             min = 1, max = 14, step = 0.1, value = 6.5)))
  
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
          pars <- add_one_int(pars, 
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
        pars <- add_one_int(pars, 
                            prop = prop/100, 
                            scaling_const = 1 - scale/100)
      }
    }
    pars
  })
  
  
  seir_model <- reactive({
    # pars <- default_seir_parameters
    
    # if(!is.null(input$add_pi_toggle))
    # if(input$add_pi_toggle)
    
    
    # Collect parameter settings that the user set:
    # for(nm in names(inputs_to_manipulate))
    # if(!is.null(input[[nm]]))
    # pars[[nm]] <- input[[nm]]
    if(is.null(seir_pars_nonpi()))
      return(NULL)
    
    do.call(run_covid_simulation, seir_pars_nonpi())
  })
  
  output$panel1_plot <- renderPlot({
    if(is.null(seir_model()))
      return(NULL)
    
    plot_rcs(seir_model(), input$panel1_output_selector, 
             start_date = input$start_date, end_date = input$start_date + input$panel1_xlim)
  })
  
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
    if(!is.null(input$add_pi_toggle))
      if(input$add_pi_toggle){
        pars <- seir_pars_nonpi()
        ll <- lapply(as.list(pars$group_names), function(x) {
          sliderInput(paste0("add_pi_", which(pars$group_names == x)), label = x, min = 0, max = 1, value = 0)
        })
        return(do.call(flowLayout, ll))
      }
    return(NULL)
  })
})
