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
            uiOutput('panel1_settings'),
            checkboxInput('set_starting_toggle', "Set starting proportions?", value = FALSE),
            uiOutput('set_starting_ui'),
            dateInput('start_date', label = "Date of epidemic start", value = "2020-02-01"),
            width=2),
          fluidRow(
            column(
              width=2,
              h3("NPI settings"),
              radioButtons("add_npi_toggle", "", 
                           choices = list("No intervention" = "no",
                                          "Basic NPI (same in all age groups)" = "basic",
                                          "Predefined NPIs" = "predefined",
                                          "Manual NPI definition" = "detailed")),
              uiOutput("add_npi_ui")),
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
        )
      ),
      tabPanel("Model description",
               includeHTML("doc/model_description.html"))
    )
  )
)
