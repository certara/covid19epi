ui <- shinyUI(
  fluidPage(
    titlePanel("COVID epidemic and intervention model"),
    tabsetPanel(
      id="main_tabset",
      tabPanel("Basic simulation",
               fluidRow(
                 column(
                   h3("Simulation settings"),
                   selectInput("scenario", label = "Choose scenario", 
                               choices = c("Generic" = "generic", 
                                           "United States" = "us", "United Kingdom" = "uk")),
                   uiOutput('panel1_settings'),
                   checkboxInput('set_starting_prop', "Set starting proportions?", value = FALSE),
                   dateInput('start_date', label = "Date of epidemic start", value = "2020-02-01"),
                   width=3),
                 column(width = 8,
                        h3("Simulation output"),
                        flowLayout(
                          selectInput("panel1_output_selector", 
                                      "Display...", c(compartment_names), selected = "I3"),
                          sliderInput("panel1_xlim", label = "Days to display", value=150, min=14, max=300)),
                        plotOutput('panel1_plot'),
                        # NPIs
                        fluidRow(radioButtons("add_intervention_toggle", "Apply intervention?", 
                                              choices = list("No intervention" = "no",
                                                             "Basic NPI (same in all age groups)" = "basic",
                                                             "Detailed definition" = "deta")),
                                 sliderInput("add_intervention_prop", "To what % of population?", 
                                             min = 0, max = 100, value = 50),
                                 sliderInput("add_intervention_scaling", "How much to decrease contacts? (in %)", 
                                             min = 0, max = 100, value = 50)
                        ),
                        # PIs
                        fluidRow(checkboxInput("add_pi_toggle", "Apply prophylaxis?", value = TRUE),
                                 uiOutput("add_pi_ui"))) 
               )
      ),
      tabPanel("Model description",
               includeHTML("doc/model_description.html"))
      # tabPanel("Model parameters")
      #          # includeHTML("doc/"))
    )
  )
)
