
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
      h3("Likelihood Parameters"),
      h3(withMathJax("$$X\\sim Bin(n, \\theta), with$$")),
      fluidRow(
        column(4,
               numericInput("n", withMathJax("$$\\textbf{Number}\\ \\textbf{of}\\ \\textbf{samples}\\ \\textbf{n}$$"), 
                            min = 1, max = 1000, value = 10)),
        column(4,
               numericInput("prob", withMathJax("$$\\textbf{Success}\\ \\textbf{Probability}\\ \\theta$$"), 
                            min = 0, max = 1, step=.1, value = .5))),
      h3("Prior Parameters"),
      h3(withMathJax("$$p(\\theta)=Beta(\\theta|\\alpha, \\beta), with$$")),
      fluidRow(
        column(3, 
               numericInput("alpha", withMathJax("$$\\alpha$$"), 
                            min = 0, max = 100, step=.05, value = 1)),
        column(3, 
               numericInput("beta", withMathJax("$$\\beta$$"), 
                            min = 0, max = 100, step=.05, value = 1))
        ),
      h3("Sampling"),
      actionButton("add", "Add"),
      actionButton("remove", "Remove"),
      actionButton("reset", "Reset")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(2, 
                      plotOutput("samplePlot")),
               column(10, 
                      plotOutput("triPlot"))),
      h2("Parametrization"),
      wellPanel(
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
        )
      ),
      h2("Point Estimates"),
      wellPanel(
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
  )
))
