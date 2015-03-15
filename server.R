library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  cfunc <- function(x, n, mu, sigma) sqrt(n) * (mean(x) - mu) / sigma
  
  dataInput <- reactive({switch(input$dist,
     "Die rolling" =      {
                            mu    <- 3.5
                            sigma <- sqrt(35 / 12)  # aprox. 1.707
                            tbl   <- matrix(sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                            ncol=as.numeric(input$nEvt))
                            temp  <- as.data.frame(apply(tbl, 1, cfunc, as.numeric(input$nEvt), mu, sigma))
                            colnames(temp) <- c("x")
                            temp
                          },
     "Two dice rolling" = {
                            mu    <- 7
                            sigma <- sqrt(35 /  6)  # aprox. 2.415
                            tbl   <- matrix(sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE) +
                                            sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                            ncol=as.numeric(input$nEvt))
                            temp  <- as.data.frame(apply(tbl, 1, cfunc, as.numeric(input$nEvt), mu, sigma))
                            colnames(temp) <- c("x")
                            temp
                          },
     "Flip coin" =        {
                            mu    <- as.numeric(input$p)
                            sigma <- sqrt(as.numeric(input$p) * (1 - as.numeric(input$p)))
                            tbl   <- matrix(sample(0:1, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                            ncol=as.numeric(input$nEvt))
                            temp  <- as.data.frame(apply(tbl, 1, cfunc, as.numeric(input$nEvt), mu, sigma))
                            colnames(temp) <- c("x")
                            temp
                          },
     "Exponential" =      {
                            mu    <- 1 / as.numeric(input$lambda_exp)
                            sigma <- 1 / as.numeric(input$lambda_exp)
                            tbl   <- matrix(rexp(as.numeric(input$nEvt) * as.numeric(input$nSim), rate=as.numeric(input$lambda_exp)),
                                            ncol=as.numeric(input$nEvt))
                            temp  <- as.data.frame(apply(tbl, 1, cfunc, as.numeric(input$nEvt), mu, sigma))
                            colnames(temp) <- c("x")
                            temp
                          },
     "Poisson" =          {
                            mu    <- as.numeric(input$lambda_pois)
                            sigma <- sqrt(as.numeric(input$lambda_pois))
                            tbl   <- matrix(rpois(as.numeric(input$nEvt) * as.numeric(input$nSim), lambda=as.numeric(input$lambda_pois)),
                                            ncol=as.numeric(input$nEvt))
                            temp  <- as.data.frame(apply(tbl, 1, cfunc, as.numeric(input$nEvt), mu, sigma))
                            colnames(temp) <- c("x")
                            temp
                          }
  )})

  output$plot  <- renderPlot({
    g <- ggplot(dataInput(), aes(x = x)) +
      geom_histogram(alpha=0.20, binwidth=0.3, colour="black", aes(y = ..density..))
    if(input$show_dist == TRUE) {
      g <- g + stat_function(fun = dnorm, size = 1)
    }
    g
  })

#   output$dplot <- renderPlot({
#     tbl <- as.data.frame(apply(tbl, 1, mean))
#     colnames(tbl) <- c("c")
#     g <- ggplot(tbl, aes(x = x)) +
#       geom_histogram(alpha=0.20, binwidth=0.3, colour="black", aes(y = ..density..))
#     if(input$show_dist == TRUE) {
#       g <- g + stat_function(fun = density, size = 1)
#     }
#     g
#   })

  output$table <- renderTable({dataInput()})
  
})
