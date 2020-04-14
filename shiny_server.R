server <- shinyServer(function(input, output, session) {
  # inputs_to_manipulate will later be generated based on external file config, to make this more 
  # dynamically customisable
  inputs_to_manipulate <- list(
    list("sliderInput", list("r0", label = "r0 (approximate)", min = 0.1, max = 4, value = 2.4, step = .1)),
    list("selectInput", 
         list("country", label = "Social mixing patterns", 
              choices = c("All countries (recommended)", levels(polymod$participants$country)))),
    list("sliderInput", list("inv_gamma1", label = "Length of incubation period", min = 1, max = 14, step = 0.1, value = 5.1)),
    list("sliderInput", list("inv_gamma2", label = "Length of infectious period", min = 1, max = 14, step = 0.1, value = 6.5)))
  
  output$panel1_settings <- renderUI({
    lapply(inputs_to_manipulate, function(x) {
      do.call(x[[1]], x[[2]])
    })
  })
  
  seir_pars_nonpi <- reactive({
    pars <- convert_settings_to_par(input, default_seir_parameters)
    
    if(!is.null(input$add_intervention_toggle))
      if(input$add_intervention_toggle == "basic")
        pars <- add_one_int(pars, 
                            prop = input$add_intervention_prop/100, 
                            scaling_const = 1 - input$add_intervention_scaling/100)
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
    
    do.call(run_covid_simulation, seir_pars_nonpi())
  })
  
  output$panel1_plot <- renderPlot({
    plot_rcs(seir_model(), input$panel1_output_selector, 
             start_date = input$start_date, end_date = input$start_date + input$panel1_xlim)
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
