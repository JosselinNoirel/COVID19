library("shiny")

shinyUI(fluidPage(
    titlePanel("Plotting the SARS-Cov-2 (COVID19) Pandemic"),

    p("How many people have been infected or have died as a result of
       the pandemic; how many people have recovered from the disease.
       (Note that testing procedures and policies vary from country to country.
        France, in particular, only recently started to include statistics from
        nursing homes.)"),

    sidebarLayout(
        sidebarPanel(
            selectInput("what", "What are we counting:",
                        c("Confirmed cases"="confirmed",
                          "Deaths"         ="deceased",
                          "Recovered cases"="recovered")),

            p("Filter countries to focus on a specific group using a
               comma-separated list. If the first character of the list
               is a dash, then the countries are excluded from the analysis."),

            textInput("countries", "Filter countries",
                      value="-China,South Korea,Diamond Princess"),

            p("Filter the data to focus on periods of time when counts have
               reached a certain threshold. Increase the value to reduce noise."),

            shinyWidgets::sliderTextInput("min", "Minimal Count",
                                          choices=c(10, 20, 50,
                                                    100, 200, 500,
                                                    1000, 2000, 5000,
                                                    10000, 20000, 50000,
                                                    100000, 200000, 500000),
                                          selected=5000, grid=TRUE),

            p("We generally want to look at those things on a log-scale,
               but you can also deactivate it to feel the full wrath
               of the exponential."),

            checkboxInput("log", "Log-Scale", value=TRUE),

            p("Linear regression for a 10-day trend"),

            checkboxInput("predict", "Prediction", value=FALSE),

            width=5 # Options
        ),

        mainPanel(
            tabsetPanel(type="tabs",
                        # tabPanel("Log", br(), uiOutput("log")),
                        tabPanel("Cumulative counts",
                                 br(),
                                 plotOutput("cumPlot")),
                        tabPanel("Waiting for the peak",
                                 br(),
                                 p("Daily cases/deaths normalised by the population of a country
                                    and smoothed (per thousand people). Note there's a lag when one switches from confirmed
                                    cases to number of deaths."),
                                 plotOutput("peakPlot"),
                                 p("Suggestion (1): copy “Italy,Spain,UK,US,Germany” in the country filter.",
                                  br(),
                                  "Suggestion (2): copy “China,South Korea,Japan” in the country filter."),
                                 p("(1) Normalisation by population flattens the curve for very large countries.
                                    (2) The Log-scale cannot be applied for this plot.")),
                        tabPanel("Statistics",           DT::dataTableOutput("stats"))),
            width=7
        )
    ),

    p("This Shiny application relies on the ",
      a("2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository
         by Johns Hopkins CSSE",
        href="https://github.com/CSSEGISandData/COVID-19"), "."),

    p("Find the code for this app at ",
    a("JosselinNoirel/COVID19", href="https://github.com/JosselinNoirel/COVID19/"),
    " on GitHub.")
    ))
