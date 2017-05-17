
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(reshape2)
library(ggplot2)

betaMode <- function(alpha, beta) {
  return((alpha - 1)/(alpha+beta-2))
}

betaMean <- function(alpha, beta) {
  return((alpha)/(alpha+beta))
}

betaStd <- function(alpha, beta) {
  return(sqrt((alpha * beta)/(((alpha + beta)**2) * (alpha + beta + 1))))
}

shinyServer(function(input, output) {
  
  x <- reactive({
    ## Number of successes
    n <- input$n
    prob <- input$prob ## success probability
    data <- sum(rbinom(n, size = 1, prob))
    return(data)
  })

  output$triPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    n <- input$n
    alpha <- input$alpha
    beta <- input$beta
    theta <- seq(0,1, length.out = n)
    
    ## Data
    x <- x() ## number of successes
    
    ## Likelihood p(x|theta) with x ~ Bin(theta, alpha, beta)
    likelihood <- dbinom(x, n, theta)
    
    ## Prior p(theta) based on Beta(theta, alpha, beta)
    prior <- dbeta(theta, alpha, beta)
    
    ## Posterior Distribution p(theta|x)
    posterior <- dbeta(theta, alpha+x, beta+n-x)
    
    data <- data.frame(Theta=theta, Posterior=posterior, Prior=prior, Likelihood=likelihood)
    data.molten <- melt(data, id.vars = "Theta")
    colnames(data.molten) <- c("Theta", "Function", "Density")
    
    ggplot(data.molten, aes(x=Theta, y=Density)) +
      geom_line(aes(colour=Function, linetype=Function), size=2) +
      xlab("Probability of Success") +
      scale_x_continuous(breaks = seq(0, 1.1, by=.1)) +
      theme_bw() +
      theme(plot.title   = element_text(size=15),
            axis.text.x  = element_text(size=20),
            axis.title.x = element_text(size=25),
            axis.text.y  = element_text(size=20),
            axis.title.y = element_text(size=25),
            legend.title = element_blank(),
            legend.text  = element_text(size=15))
  })
  
  output$pointEst_Prior <- renderTable({
    alpha <- input$alpha
    beta  <- input$beta
    
    beta_mode <- betaMode(alpha, beta)
    beta_mean <- betaMean(alpha, beta)
    beta_std  <- betaStd(alpha, beta)
    
    pE <- data.frame(Type=c("Mode", "Mean", "Std"), 
                     PointEstimate=c(beta_mode, beta_mean, beta_std))
    return(pE)
  })
  
  output$pointEst_Likelihood <- renderTable({
    x <- x() ## number of successes
    n <- input$n ## number of tries
    alpha <- x + 1
    beta <- n - x + 1
    
    beta_mode <- betaMode(alpha, beta)
    beta_mean <- betaMean(alpha, beta)
    beta_std  <- betaStd(alpha, beta)
    
    pE <- data.frame(Type=c("Mode", "Mean", "Std"), 
                     PointEstimate=c(beta_mode, beta_mean, beta_std))
    return(pE)
  })
  
  output$pointEst_Posterior <- renderTable({
    x <- x() ## number of successes
    n <- input$n ## number of tries
    alpha <- input$alpha + 1
    beta <- input$beta + n - x
    
    beta_mode <- betaMode(alpha, beta)
    beta_mean <- betaMean(alpha, beta)
    beta_std  <- betaStd(alpha, beta)
    
    pE <- data.frame(Type=c("Mode", "Mean", "Std"), 
                     PointEstimate=c(beta_mode, beta_mean, beta_std))
    return(pE)
    
  })
})

