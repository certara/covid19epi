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
                             min = 1, max = 14, step = 0.1, 
                             value = 1/mean(default_seir_parameters$gamma1))),
    list("sliderInput", list("inv_gamma2", label = "Length of symptoms period", 
                             min = 1, max = 14, step = 0.1, 
                             value = 1/mean(default_seir_parameters$gamma2_i1))),
    list("sliderInput", list("p_as", label = "Proportion asymptomatic", 
                             min = 0, max = 1, step = 0.01, 
                             value = mean(default_seir_parameters$p_as))))
  
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
    
    # Old way: dividing all age groups into key workers and non-key pop
    if(input$add_npi_prop > 0)
      pars <- add_npi(pars,
                      prop = input$add_npi_prop/100,
                      scaling_const = 1) #we don't scale contacts -- we use the "new way" below
    
    # New way: use contact scaling matrix
    if(input$add_npi_scaling > 0){
      week_to_start <- ceiling(as.numeric(input$add_npi_date - input$start_date + 1)/7)
      tot_weeks <- pars$Nweeks
      if(!is.null(input$add_npi_diff_young) && !is.null(input$add_npi_diff_old)){
        cscale <- 1 - c(rep(input$add_npi_diff_young, 2), 
                        rep(input$add_npi_scaling, 5), 
                        rep(input$add_npi_diff_old, 2))/100
      }else{  
        cscale <- 1 - rep(input$add_npi_scaling/100, 9)
      }
      if(week_to_start > tot_weeks)
        stop("Can't apply NPI after more than", tot_weeks, "weeks since epidemic starts")
      pars$contacts_scaling <- matrix(c(rep(1, (week_to_start-1)*pars$Ngroups), 
                                        rep(cscale, (tot_weeks-week_to_start+1))),
                                      pars$Ngroups, tot_weeks)
      
    }
    
    pars
  })
  
  seir_model <- reactive({
    if(is.null(seir_pars_nonpi()))
      return(NULL)
    
    pars <- seir_pars_nonpi()
    
    # Add pharmaceutical interventions
    # Prophylaxis (immunised at start)
    if(input$add_pi_toggle == "basic_pro"){
      if(is.null(input$add_pro_efficacy) || 
         is.null(input$add_pro_use) || 
         is.null(input$add_pro_length))
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
    if(input$add_pi_toggle == "basic_at"){
      if(is.null(input$add_at_use) || is.null(input$add_at_gamma))
        return(NULL)
        p <- (input$add_at_use/100)
        pars$gamma2_i1 <- (1-p)*pars$gamma2_i1 + p*rep(1/input$add_at_gamma, 9)
    }
    
    do.call(run_covid_simulation, pars)
  })
  
  output$panel1_plot <- renderPlot({
    if(is.null(seir_model()))
      return(NULL)
    
    y <- seir_model()
    
    # Grab real data (if needed)
    if(input$panel1_show_data && !input$panel1_dnmerge_groups) {
      real_data <-
        filter(cases_csv_clean, country == names(which(countries == input$demographics))) %>%
        filter(variable == input$panel1_output_selector) %>%
        select(time, value)
      
    } else {
      real_data <- NULL
    }
    
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
    
    y <- rescale_rcs(y, merge = !input$panel1_dnmerge_groups, pop_sizes = scale)
    
    gg <- plot_rcs(y, input$panel1_output_selector, 
                   start_date = input$start_date, end_date = input$start_date + input$panel1_xlim, 
                   lab_type = lt, overlay_data = real_data) 
    
    if(input$panel1_scaling == "pct")
      gg <- gg + scale_y_continuous(label = scales::label_percent())
    else
      gg <- gg + scale_y_continuous(label = scales::label_number_si())
    
    gg
  })
  
  
  
  # UI renders ------
  output$add_pi_ui <- renderUI({
    if(input$add_pi_toggle == "detailed_pro"){
      pars <- seir_pars_nonpi()
      ll <- lapply(as.list(pars$group_names), function(x) {
        sliderInput(paste0("add_pi_", which(pars$group_names == x)), 
                    label = x, min = 0, max = 1, value = 0)
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
        # sliderInput("add_at_use_key", "% of NPI ('key workers') treated",
                    # min = 0, max = 100, value = 5),
        HTML("<i>In the current version of the app the effect of active treatment is accounted
              for by adjusting the average duration of illness</i>"),
        sliderInput("add_at_use", "% of infected individuals treated",
                    min = 0, max = 100, value = 0),
        sliderInput("add_at_gamma", "Average duration of illness in treated", 
                    min = 0, max = 10, value = 4, step = .1)
      ))
    
    if(input$add_pi_toggle == "no")
      return(NULL)
  })
  
  output$plot_demographics <- renderPlot({
    N <- pbc_spread[input$demographics,]/1e06
    ggplot(gather(N), aes(x=key, y=value)) + 
      geom_bar(fill = "cornflowerblue", stat = "identity") + 
      labs(x="", y="N (million)") +
      theme_minimal(base_size = 10)
  })
  
  
  
  # UI -----
  output$add_npi_diff_ui <- renderUI({
    if(input$add_npi_diff_toggle){
      list(
        sliderInput("add_npi_diff_young", "Contacts decrease (%) in under 20's", 
                    min = 0, max = 100, value = input$add_npi_scaling),
        sliderInput("add_npi_diff_old",  "Contacts decrease (%) in over 70's", 
                    min = 0, max = 100, value = input$add_npi_scaling),
        HTML("Decrease of contacts in the individuals aged 20-70 is controlled by the 'main' decrease
              parameter above.")
      )
    }else{
      return(NULL)
    }
  })
  
  output$add_npi_onoff_ui <- renderUI({
    if(input$add_npi_onoff_toggle){
      HTML("In this version of the app the on and off triggers are not yet enabled.")
    }else{
      return(NULL)
    }
  })
  
  
  # Legends -----
  
  output$panel1_legend <- renderUI({
    if(input$panel1_show_data)
      HTML("Data (black points in the plot) are based on 
           <a href='https://github.com/CSSEGISandData/'>Johns Hopkins-curated dataset</a>",
           "of mortality and recovered cases. You need to select appropriate outcome ('Display...') and 
           uncheck 'show individual age groups' option, as data are only available cumulatively. You can only
           compare with absolute numbers.")
  })
  
  
})




