
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Bayesian Parameter Estimation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Data Parameters"),
      sliderInput("n",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 100),
      sliderInput("prob",
                  "Success Probability:",
                  min = 0,
                  max = 1,
                  value = .5),
      h3("Beta Distribution Parameters"),
      sliderInput("alpha",
                  "Alpha:",
                  min = 0,
                  max = 100,
                  value = .5),
      sliderInput("beta",
                  "Beta:",
                  min = 0,
                  max = 100,
                  value = .5)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("triPlot")
    )
  )
))
