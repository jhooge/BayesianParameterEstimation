
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
  
  values <- reactiveValues(x=NULL)
  
  observeEvent(input$add, {
    n <- sum(input$n, length(values$x))
    prob <- input$prob ## success probability
    values$x <- c(values$x, rbinom(input$n, size = 1, prob))
    
  })
  
  observeEvent(input$remove, {
    n <- input$n
    values$x <- head(values$x, -n)
    
  })
  
  observeEvent(input$reset, {
    values$x <- NULL
  })
  
  
  output$priorDistFormula <- renderUI({
    alpha <- input$alpha
    beta <- input$beta
    uiElement <- withMathJax(helpText(sprintf('$$\\begin{align}
                                      p(\\theta)&=\\frac{\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}}{B(\\alpha, \\beta)}
                                            \\\\&=Beta(\\theta|\\alpha, \\beta)
                                            \\\\&=Beta(\\theta|\\textbf{%.2f}, \\textbf{%.2f})
                                      \\end{align}$$', alpha, beta, alpha, beta, alpha, beta)))
    return(uiElement)
  })
  
  output$likelihoodFormula <- renderUI({
    n <- length(values$x)
    x <- sum(values$x)
    alpha <- input$alpha
    beta <- input$beta
    uiElement <- list(withMathJax(helpText(sprintf('$$X\\sim Bin(n, \\theta) = Bin(\\textbf{%i}, \\theta)$$', n))),
                      withMathJax(helpText(sprintf('$$\\begin{align}
                                      p(x|\\theta)&={n\\choose{x}}\\theta^{x}(1-\\theta)^{n-x}
                                              \\\\&={\\textbf{%i}\\choose{\\textbf{%i}}}\\theta^{\\textbf{%i}}(1-\\theta)^{\\textbf{%i}}
                                      \\end{align}$$', n, x, x, n-x))))
    return(uiElement)
  })
  
  output$posteriorFormula <- renderUI({
    n <- length(values$x)
    x <- sum(values$x)
    alpha <- input$alpha
    beta <- input$beta
    uiElement <- withMathJax(helpText(sprintf('$$\\begin{align}
                                               p(\\theta|x)&=p(x|\\theta)p(\\theta)
                                                       \\\\&=\\theta^{x}(1-\\theta)^{n-x}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}
                                                       \\\\&=\\theta^{(\\alpha+x)-1}(1-\\theta)^{(\\beta+n+x)-1}
                                                       \\\\&=Beta(\\theta|\\alpha+x, \\beta+n-x)
                                                       \\\\&=Beta(\\theta|\\textbf{%.2f}, \\textbf{%.2f})
                                                   \\end{align}$$', sum(alpha, x), sum(beta, n, x))))
    return(uiElement)
})
  
  
  output$samplePlot <- renderPlot({
    validate(
      need(!(length(values$x)==0), "")
    )
    
    successes <- sum(values$x)
    fails <- length(values$x) - successes
    
    draws <- data.frame(Draw=as.factor(c("Successes", "Fails")),
                        Count=c(successes, fails),
                        Sample=c("Sample", "Sample"))
    
    
    fig <- ggplot(draws, aes(x=Sample, y=Count, colour=Draw, fill=Draw)) + 
      geom_bar(stat = "identity", width=.2) +
      geom_text(aes(label = Count), colour="black", size = 8, hjust = .5, vjust = 4, position = "stack") +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="left",
            legend.title = element_blank(),
            legend.text  = element_text(size=15),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    
    return(fig)
  })
  
  output$triPlot <- renderPlot({
    validate(
      need(!(length(values$x)==0), "Please start drawing samples using the 'Add' button!")
    )
    
    # generate bins based on input$bins from ui.R
    n <- length(values$x)
    alpha <- input$alpha
    beta <- input$beta
    theta <- seq(0, 1, length.out = 100)
    
    ## Data
    x <- sum(values$x) ## number of successes
    
    norm <- 
    
    ## Likelihood p(x|theta) with x ~ Bin(theta, alpha, beta)
    likelihood <- dbinom(x, n, theta)
    likelihood <- likelihood/sum(likelihood)
    
    ## Prior p(theta) based on Beta(theta, alpha, beta)
    prior <- dbeta(theta, alpha, beta)
    prior <- prior/sum(prior)
    
    ## Posterior Distribution p(theta|x)
    posterior <- dbeta(theta, alpha+x, beta+n-x)
    posterior <- posterior/sum(posterior)
    
    print(sprintf("Prior Density: %.5f", prior))
    print(sprintf("Likelihood Density: %.5f", likelihood))
    print(sprintf("Posterior Density: %.5f", posterior))
    
    data <- data.frame(Theta=theta, Posterior=posterior, Prior=prior, Likelihood=likelihood)
    data.molten <- melt(data, id.vars = "Theta")
    colnames(data.molten) <- c("Theta", "Function", "Density")
    
    ggplot(data.molten, aes(x=Theta, y=Density)) +
      geom_line(aes(colour=Function, linetype=Function), size=1.5) +
      geom_text(x = Inf, y = Inf, label = paste0("n=", n), hjust = 1.2, vjust = 1.2, size=10) + 
      xlab(expression(theta)) +
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
    validate(
      need(!(length(values$x)==0), NULL)
    )
    
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
    validate(
      need(!(length(values$x)==0), NULL)
    )
    
    x <- sum(values$x) ## number of successes
    n <- length(values$x) ## number of tries
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
    validate(
      need(!(length(values$x)==0), NULL)
    )
    
    x <- sum(values$x) ## number of successes
    n <- length(values$x) ## number of tries
    alpha <- input$alpha + x
    beta <- input$beta + n - x
    
    beta_mode <- betaMode(alpha, beta)
    beta_mean <- betaMean(alpha, beta)
    beta_std  <- betaStd(alpha, beta)
    
    pE <- data.frame(Type=c("Mode", "Mean", "Std"), 
                     PointEstimate=c(beta_mode, beta_mean, beta_std))
    return(pE)
    
  })
})