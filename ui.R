#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("CoVid19 quasi-exponential growth"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("what", "Counting", c(Confirmed="confirmed",
                                            Deaths="deceased",
                                           Recovered="recovered")),
            textInput("countries", "Country Filter", value=""),
            sliderInput("min", "Minimal Count", min=15, max=100000, value=50),
            checkboxInput("log", "Log-Scale", value=TRUE),
            checkboxInput("predict", "Prediction", value=FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
