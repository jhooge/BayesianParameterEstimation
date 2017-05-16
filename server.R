
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(reshape2)
library(ggplot2)

shinyServer(function(input, output) {

  output$triPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    n <- input$n
    alpha <- input$alpha
    beta <- input$beta
    prob <- input$prob ## success probability
    theta <- seq(0,1, length.out = n)
    
    ## Data
    x <- sum(rbinom(n=n, size = 1, prob=prob)) ## number of successes
    
    ## Likelihood p(x|theta) with x ~ Bin(theta, alpha, beta)
    likelihood <- dbinom(x, n, theta)
    
    ## Prior p(theta) based on Beta(theta, alpha, beta)
    prior <- dbeta(theta, alpha, beta)
    
    ## Posterior Distribution p(theta|x)
    posterior <- dbeta(theta, alpha+x, beta+n-x)
    
    data <- data.frame(Theta=theta, Prior=prior, Likelihood=likelihood, Posterior=posterior)
    data.molten <- melt(data, id.vars = "Theta")
    colnames(data.molten) <- c("Theta", "Function", "Density")
    
    ggplot(data.molten, aes(x=Theta, y=Density)) +
      geom_line(aes(colour=Function))
  })
  

})
