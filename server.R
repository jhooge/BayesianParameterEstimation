
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    
    x <- 1:input$n
    theta <- input$theta

    # draw the histogram with the specified number of bins
    plot(density(dbinom(x, 100, input$theta)))
  })
  
  output$posteriorPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    n <- input$n
    alpha <- input$alpha
    beta <- input$beta
    theta <- input$theta
    x <- rbinom(n, 1, theta)
    
    print(alpha)
    print(beta)
    
    # draw the histogram with the specified number of bins
    plot(density(dbeta(x, alpha+x, beta+n-x)))
  })

})
