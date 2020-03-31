#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Data: https://github.com/CSSEGISandData/COVID-19

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

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

preprocess = function (dat) {
    colnames(dat)[1:4] = c("Province", "Country", "Latitude", "Longitude")
    dat = pivot_longer(dat, 5:last_col(), names_to="Date", values_to="Value")
    dat = dat %>% group_by(Country, Date) %>% summarise(Value=sum(Value))
    dat$Date = mdy(dat$Date)
    dat
}

dat = list()
dat[["deceased"]] = preprocess(data[[1]])
dat[["confirmed"]] = preprocess(data[[2]])
dat[["recovered"]] = preprocess(data[[3]])

country_list = (dat[[2]] %>% filter(Date == dmy("15-03-2020") & Value > 40))$Country
country_list = setdiff(country_list, "Others")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')

        cl = strsplit(input$countries, ',')[[1]]

        if (length(cl) == 0) {
            cl = country_list
        } else {
            cl = intersect(country_list, cl)
        }

        x = dat[[input$what]] %>% filter(Country %in% cl & Value > input$min)

        ggplot(x,
               aes(Date, Value, col=Country)) +
            geom_point() + (if (input$log) scale_y_log10() else NULL) +
            geom_line() +
            (if (input$predict) geom_smooth(data=subset(x, Date > today() - 10),
                                            method="lm") else NULL)
    })
})
