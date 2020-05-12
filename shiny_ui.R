ui <- shinyUI(
  fluidPage(
    titlePanel("COVID-19 epidemic and intervention model"),
    tabsetPanel(
      id="main_tabset",
      tabPanel(
        "Basic simulation",
        fluidRow(
          column(
            h3("Simulation settings"),
            selectInput("demographics", label = "Choose country (demographics)", 
                        choices = c(countries), selected = "826"),
            plotOutput("plot_demographics", height = "200px"),
            dateInput('start_date', label = "Date of epidemic start", value = "2020-02-01"),
            selectInput("initial_infected_prop", label = "Initially infected proportion",
                        choices = c("One in million" = 1e-06, "Five per million" = 5e-06,
                                    "One per 100,000" = 1e-05, "One per 10,000" = 1e-04,
                                    "One per 1,000" = 1e-03), selected = 1e-04),
            uiOutput('panel1_settings'),
            width=2),
          column(
            width=2,
            h3("NPI settings"),
            HTML("<i>To disable social distancing, simply set the decrease in contacts to 0.
                    When proportion of individuals who do not socially distance is above 0, 
                    the model will divide each age group into 2 to show (non-)distancing individuals.
                    See </i>Model Description<i> for more details</i>.<br>"),
            dateInput('add_npi_date', label = "NPI starts on", value = "2020-04-01"),
            sliderInput("add_npi_prop", "What % of population doesn't distance?", 
                        min = 0, max = 100, value = 0),
            sliderInput("add_npi_scaling", "How much to decrease contacts? (in %)", 
                        min = 0, max = 100, value = 70),
            checkboxInput("add_npi_diff_toggle", 
                          "Use additional social distancing measures for children and elderly?", value = FALSE),
            uiOutput("add_npi_diff_ui"),
            checkboxInput("add_npi_onoff_toggle", 
                          "Add on and off triggers for social distancing?", value = FALSE),
            uiOutput("add_npi_onoff_ui")
          ),
          column(
            width=2,
            h3("PI settings"),
            radioButtons("add_pi_toggle", "",
                         choices = list("No intervention" = "no",
                                        "Basic prophylaxis" = "basic_pro",
                                        "Basic active treatment" = "basic_at",
                                        "Vaccination" = "vac",
                                        "Detailed prophylaxis definition" = "detailed_pro")),
            uiOutput("add_pi_ui")),
          column(
            width = 5,
            h3("Simulation output"),
            HTML("This model is meant for simulations of hypothetical COVID-19 epidemics to understand impact of therepautics. 
                    While it is possible to adjust demographics to a particular country and check the model outputs against real
                    data, the model is not calibrated to particular situations and should be not used to make predictions for the
                    ongoing epidemic in particular countries, which are affected by many local factors.<br>
                    Details on the model are presented in the <i>Model description</i> tab above.<br>"),
            flowLayout(
              selectInput("panel1_output_selector", 
                          "Display...", c(compartment_names_short), selected = "I1"),
              sliderInput("panel1_xlim", label = "Days to display", value=150, min=14, max=300),
              selectInput("panel1_scaling", "Y axis shows", 
                          c("Absolute numbers" = "absolute", 
                            "Numbers per 100,000" = "per100k",
                            "Percentages" = "pct"), selected = "absolute"),
              checkboxInput("panel1_dnmerge_groups", "Show individual age groups?", value = TRUE),
              checkboxInput("panel1_show_data", "Show real data?", value = FALSE)),
            uiOutput("panel1_legend"),
            plotOutput('panel1_plot')
            
          )
        )
      ),
      tabPanel("Model description",
               includeHTML("doc/model_description.html"))
    )
  )
)
