# Packages ----

library("shiny")
library("shiny.i18n")
library("shinyWidgets") # devtools::install_github("dreamRs/shinyWidgets")
library("ggrepel")      # install.packages("ggrepel")
# library("tidyverse")
library("tidyr")
library("stringr")
library("readr")
library("dplyr")
library("lubridate")
library("DT")

theme_set(theme_bw() + theme(panel.border=element_blank()))

# Possible settings
# Italy,Spain,UK,US,Germany
# China,South Korea,Japan

# Internationalisation ----

translator = Translator$new(translation_json_path="translation.json")
translator$set_translation_language("en")

# Functions ----

ma = function(x, n=5) {
    as.numeric(stats::filter(x, rep(1/n, n), sides=2))
}

preprocess = function (dat) {
    colnames(dat)[1:4] = c("Province", "Country", "Latitude", "Longitude")

    dat$Country[dat$Country == "United Kingdom"] = "UK"
    dat$Country[dat$Country == "Korea, South"] = "South Korea"
    dat$Country[dat$Country == "Taiwan*"] = "Taiwan"
    dat = pivot_longer(dat, 5:last_col(), names_to="Date", values_to="Value")
    dat = dat %>% group_by(Country, Date) %>%
        summarise(Value=sum(Value)) %>%
        ungroup()
    dat$Date = mdy(dat$Date)
    dat
}

# Reading the data from the RDS file ----

populations = read_csv("populations.csv", col_names=FALSE, na="Indeterminate")
colnames(populations) = c("Country", "Population", "Continent")
continents = unique(populations$Continent)
continents = continents[! is.na(continents)]

src = paste(today(), ".rds", sep="")

if (file.exists(src)) {
    dat = read_rds(src)
} else { # Reading the data from GitHub ----
    fn = function (str)
        paste("time_series_covid19_", str, "_global.csv", sep="")

    path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

    file = list()
    file[["deceased"]]  = paste(path, fn("deaths"), sep="/")
    file[["confirmed"]] = paste(path, fn("confirmed"), sep="/")
    file[["recovered"]] = paste(path, fn("recovered"), sep="/")

    data = list()
    data[["deceased"]]  = read_csv(file[["deceased"]])
    data[["confirmed"]] = read_csv(file[["confirmed"]])
    data[["recovered"]] = read_csv(file[["recovered"]])

    dat = list()
    dat[["deceased"]] = preprocess(data[[1]])
    dat[["confirmed"]] = preprocess(data[[2]])
    dat[["recovered"]] = preprocess(data[[3]])

    write_rds(dat, src)
}

country_list = (dat[[2]] %>% filter(Date == dmy("15-03-2020") & Value > 40))$Country
country_list = setdiff(country_list, "Others")

# Server ----

shinyServer(function(input, output) {
    # Internationalisation ----

    i18n = reactive({
        selected = input$selected_language
        if (length(selected) > 0 && selected %in% translator$languages)
            translator$set_translation_language(selected)
        translator
    })

    # Cumulative plot ----

    output$cumPlot = renderPlot({
        if (str_starts(input$countries, "-")) {
            selfn = setdiff
            input_countries = substring(input$countries, 2)
        } else {
            selfn = intersect
            input_countries = input$countries
        }
        cl = strsplit(input_countries, ',')[[1]]
        cl = c(cl, populations$Country[populations$Continent %in% continents[continents %in% cl]])

        if (length(cl) == 0) {
            cl = country_list
        } else {
            cl = selfn(country_list, cl)
        }

        x = dat[[input$what]] %>% filter(Country %in% cl &
                                             Value > input$min &
                                             Date >= input$daterange[1] &
                                             Date <= input$daterange[2])

        p = ggplot(x,
                   aes(Date, Value, colour=Country)) +
            geom_point(size=1, alpha=(if (input$predict) 0.5 else 1)) +
            geom_line(size=1, alpha=(if (input$predict) 0.5 else 1)) +
            coord_cartesian(clip="off") +
            (if (input$log) scale_y_log10() else NULL) +
            (if (input$predict)
                geom_smooth(data=subset(x, Date > today() - 10),
                            method="lm", se=FALSE) else NULL) +
            geom_text_repel(data=subset(x, Date == max(x$Date)),
                      aes(label=Country, colour=Country, x=Date, y=Value),
                      xlim=c(min(x$Date), max(x$Date) + 14),
                      segment.size=0.1,
                      direction="y",
                      nudge_x = 1,
                      hjust = 0) +
            ylab(NULL) + xlab(NULL) +
            theme(plot.margin=unit(c(0.1,1,0.1,0.1), "in"),
                  legend.position="none")

        p
    })

    # Peak ----

    output$peakPlot = renderPlot({
        if (str_starts(input$countries, "-")) {
            selfn = setdiff
            input_countries = substring(input$countries, 2)
        } else {
            selfn = intersect
            input_countries = input$countries
        }
        cl = strsplit(input_countries, ',')[[1]]
        cl = c(cl, populations$Country[populations$Continent %in% continents[continents %in% cl]])

        if (length(cl) == 0) {
            cl = country_list
        } else {
            cl = selfn(country_list, cl)
        }

        x = dat[[input$what]] %>%
            filter(Country %in% cl &
                       Date >= input$daterange[1] &
                       Date <= input$daterange[2] &
                       Value > input$min) %>%
            group_by(Country) %>%
            mutate(n=n()) %>%
            ungroup() %>%
            filter(n > 10)

        x = dat[[input$what]] %>% filter(Country %in% x$Country &
                                             Date >= input$daterange[1] &
                                             Date <= input$daterange[2])

        x = x %>% left_join(populations, by="Country")

        TODAY = max(x$Date)
        y = x %>% mutate(diff=as.integer(TODAY - Date)) %>%
            arrange(Date) %>%
            group_by(Country) %>%
            mutate(n=n(), Difference=c(0, diff(Value))) %>%
            ungroup() %>%
            mutate(NormalisedDifference=1000 * Difference/Population,
                   Smoothed=ma(NormalisedDifference, n=4))

        p = ggplot(y,
               aes(Date, NormalisedDifference, colour=Country)) +
            (if (input$scatter) geom_point(alpha=0.3) else NULL) +
            geom_smooth(method="loess", formula=y~x,
                        span=0.5, se=FALSE) +
            geom_text_repel(data=subset(y, Date == max(x$Date)),
                            aes(label=Country, colour=Country,
                                x=Date, y=NormalisedDifference),
                            xlim=c(min(x$Date), max(x$Date) + 14),
                            segment.size=0.1,
                            direction="y",
                            nudge_x = 1,
                            hjust = 0) +
            ylab(NULL) + xlab(NULL) +
            coord_cartesian(clip="off") +
            theme(plot.margin=unit(c(0.1,1,0.1,0.1), "in"),
                  legend.position="none")

        p
    })

    # Table ----

    output$stats = renderDataTable({
        if (str_starts(input$countries, "-")) {
            selfn = setdiff
            input_countries = substring(input$countries, 2)
        } else {
            selfn = intersect
            input_countries = input$countries
        }
        cl = strsplit(input_countries, ',')[[1]]
        cl = c(cl, populations$Country[populations$Continent %in% continents[continents %in% cl]])

        if (length(cl) == 0) {
            cl = country_list
        } else {
            cl = selfn(country_list, cl)
        }

        datatable(
            dat[[input$what]] %>%
                filter(Country %in% cl & Value > input$min) %>%
                arrange(Country, Date),
            extensions='Buttons',
            options = list(
                dom = 'Bft',
                buttons = c('copy', 'csv', 'excel'))
        )
    })

    # UI ----

    output$content = renderUI({
        TODAY = max(dat[[1]]$Date)
        tagList(
            # setBackgroundColor(""),
            tags$head(tags$style('h1, h2, h3, .control-label {color: #857761;}')),
            setSliderColor(rep("#c1002a", 5), 1:5),
            chooseSliderSkin(skin="Flat"),
            selectInput('selected_language',
                        i18n()$t("Language"),
                        choices=c("en", "fr"),
                        selected=input$selected_language),
            titlePanel(i18n()$t("Title")),
            p(i18n()$t("Intro_paragraph")),

            sidebarLayout(
                sidebarPanel(
                    selectInput("what", i18n()$t("What"),
                                c("confirmed", "deceased", "recovered") %>%
                                    stats::setNames(c(i18n()$t("Confirmed cases"),
                                                    i18n()$t("Deaths"),
                                                    i18n()$t("Recovered")))),

                    p(i18n()$t("Country_paragraph")),

                    textInput("countries", i18n()$t("Filter countries"),
                              value="Italy,France,Germany,Spain,UK,US"),

                    p(i18n()$t("Threshold_paragraph")),

                    shinyWidgets::sliderTextInput("min", i18n()$t("Minimal count"),
                                                  choices=c(10, 20, 50,
                                                            100, 200, 500,
                                                            1000, 2000, 5000,
                                                            10000, 20000, 50000,
                                                            100000, 200000, 500000),
                                                  selected=10000, grid=TRUE),

                    sliderInput("daterange", "Dates",
                                min=as.Date("2020-02-01", "%Y-%m-%d"),
                                max=TODAY,
                                value=c(as.Date("2020-02-01", "%Y-%m-%d"), TODAY),
                                timeFormat="%d-%m-%Y"),

                    width=5 # Options
                ),

                mainPanel(
                    tabsetPanel(type="tabs",
                                tabPanel(i18n()$t("Cumulative counts"),
                                         br(),
                                         p(i18n()$t("Cumplot_explanation")),
                                         plotOutput("cumPlot"),
                                         p(i18n()$t("Log_paragraph")),
                                         checkboxInput("log", i18n()$t("Log-scale"), value=TRUE),
                                         p(i18n()$t("Regression_paragraph")),
                                         checkboxInput("predict", i18n()$t("Slope"), value=FALSE)),
                                tabPanel(i18n()$t("Waiting for the peak"),
                                         br(),
                                         p(i18n()$t("Peakplot_explanation")),
                                         plotOutput("peakPlot"),
                                         p(i18n()$t("Scatter_paragraph")),
                                         checkboxInput("scatter", i18n()$t("Scatter"), value=TRUE)
                                ),
                                tabPanel(i18n()$t("Table"),
                                         br(),
                                         p(i18n()$t("Table_explanation")),
                                         DT::dataTableOutput("stats"))),
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
        )
    })
})
