
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  withMathJax(),

  # Application title
  titlePanel("Bayesian Parameter Estimation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Data Parameters"),
      numericInput("n", "Number of samples:", min = 1, max = 1000, value = 10),
      actionButton("add", "Add"),
      actionButton("remove", "Remove"),
      actionButton("reset", "Reset"),
      sliderInput("prob",
                  "Success Probability:",
                  min = 0,
                  max = 1,
                  step=.01,
                  value = .5),
      h3("Prior Parameters"),
      sliderInput("alpha",
                  "Alpha:",
                  min = 0,
                  max = 20,
                  step=.05,
                  value = 1),
      sliderInput("beta",
                  "Beta:",
                  min = 0,
                  max = 20,
                  step=.05,
                  value = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(2, plotOutput("samplePlot")),
               column(10, plotOutput("triPlot"))),
      
      h2("Parameterization"),
      fluidRow(
        column(4, 
               h3("Prior"), 
               withMathJax(uiOutput("priorDistFormula"))),
        column(4, 
               h3("Likelihood"), 
               withMathJax(uiOutput("likelihoodFormula"))),
        column(4, 
               h3("Posterior"), 
               withMathJax(uiOutput("posteriorFormula")))
      ),
      h2("Point Estimates"),
      fluidRow(
        column(4, 
               h3("Prior"),
               tableOutput("pointEst_Prior")),
        column(4, 
               h3("Likelihood"),
               tableOutput("pointEst_Likelihood")),
        column(4, 
               h3("Posterior"),
               tableOutput("pointEst_Posterior")))
    )
  )
))
