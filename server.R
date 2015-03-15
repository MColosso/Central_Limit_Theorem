library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  cfunc <- function(x, n, mu, sigma) sqrt(n) * (mean(x) - mu) / sigma

#
# Calculate thoretical mean and standard deviation
#
  mu    <- reactive({switch(input$dist,
                  "Die rolling"      = {3.5},
                  "Two dice rolling" = {7},
                  "Flip coin"        = {as.numeric(input$p)},
                  "Exponential"      = {1 / as.numeric(input$lambda_exp)},
                  "Poisson"          = {as.numeric(input$lambda_pois)}
  )})

  sigma <- reactive({switch(input$dist,
                  "Die rolling"      = {sqrt(35 / 12)},  # aprox. 1.707
                  "Two dice rolling" = {sqrt(35 /  6)},  # aprox. 2.415
                  "Flip coin"        = {sqrt(as.numeric(input$p) * (1 - as.numeric(input$p)))},
                  "Exponential"      = {1 / as.numeric(input$lambda_exp)},
                  "Poisson"          = {sqrt(as.numeric(input$lambda_pois))}
  )})

#
# Get a sample of selected distribution accordingly to the parameters indicated
#
  dataTable <- reactive({switch(input$dist,
     "Die rolling" =      {
                            matrix(sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                   ncol=as.numeric(input$nEvt))
                          },
     "Two dice rolling" = {
                            matrix(sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE) +
                                   sample(1:6, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                   ncol=as.numeric(input$nEvt))
                          },
     "Flip coin" =        {
                            matrix(sample(0:1, as.numeric(input$nEvt) * as.numeric(input$nSim), replace=TRUE),
                                   ncol=as.numeric(input$nEvt))
                          },
     "Exponential" =      {
                            matrix(rexp(as.numeric(input$nEvt) * as.numeric(input$nSim), rate=as.numeric(input$lambda_exp)),
                                   ncol=as.numeric(input$nEvt))
                          },
     "Poisson" =          {
                            matrix(rpois(as.numeric(input$nEvt) * as.numeric(input$nSim), lambda=as.numeric(input$lambda_pois)),
                                   ncol=as.numeric(input$nEvt))
                          }
  )})

#
# Center the dataTable accordingly to the theoretical parameters
#
  dataInput <- reactive({switch(input$dist,
                                "Die rolling" =      {
                                  temp  <- as.data.frame(apply({dataTable()}, 1, cfunc, as.numeric(input$nEvt), {mu()}, {sigma()}))
                                  colnames(temp) <- c("x")
                                  temp
                                },
                                "Two dice rolling" = {
                                  temp  <- as.data.frame(apply({dataTable()}, 1, cfunc, as.numeric(input$nEvt), {mu()}, {sigma()}))
                                  colnames(temp) <- c("x")
                                  temp
                                },
                                "Flip coin" =        {
                                  temp  <- as.data.frame(apply({dataTable()}, 1, cfunc, as.numeric(input$nEvt), {mu()}, {sigma()}))
                                  colnames(temp) <- c("x")
                                  temp
                                },
                                "Exponential" =      {
                                  temp  <- as.data.frame(apply({dataTable()}, 1, cfunc, as.numeric(input$nEvt), {mu()}, {sigma()}))
                                  colnames(temp) <- c("x")
                                  temp
                                },
                                "Poisson" =          {
                                  temp  <- as.data.frame(apply({dataTable()}, 1, cfunc, as.numeric(input$nEvt), {mu()}, {sigma()}))
                                  colnames(temp) <- c("x")
                                  temp
                                }
  )})

#
# Display results: "Mean distrib." tab
#
  output$plot  <- renderPlot({
    g <- ggplot(dataInput(), aes(x = x)) +
      geom_histogram(alpha=0.20, binwidth=0.3, colour="black", aes(y = ..density..))
    if(input$show_dist == TRUE) {
      g <- g + stat_function(fun = dnorm, size = 1)
    }
    g
  })

  output$tInput <- renderText("Theoretical values: mean = 0, standard deviation = 1")
  output$sInput <- renderText(paste("Sample values: mean = ",   round(mean(as.matrix({dataInput()})), 3),
                                     ", standard deviation = ", round(sd(as.matrix({dataInput()})),   3)))

#
# Display results: "Sample distrib." tab
#
  output$dplot <- renderPlot({
    tbl <- as.data.frame(matrix({dataTable()}, ncol=1))
    colnames(tbl) <- c("x")
    g <- ggplot(tbl, aes(x = x)) +
      geom_histogram(alpha=0.20, binwidth=0.3, colour="black", aes(y = ..density..))
    if(input$show_dist == TRUE) {
      g <- g + geom_density()
    }
    g
  })

  output$tValues <- renderText(paste("Theoretical values: mean = ", round({mu()},              3),
                                     ", standard deviation = ",     round({sigma()},           3)))
  output$sValues <- renderText(paste("Sample values: mean = ",      round(mean({dataTable()}), 3),
                                     ", standard deviation = ",     round(sd({dataTable()}),   3)))

#
# Display results: "Table" tab
#
  output$table <- renderTable({dataInput()})
  
})
