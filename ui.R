library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Central Limit Theorem"),
  
  navlistPanel(
    widths=c(2, 9),
    "Introduction",
    tabPanel("Definition",
      sidebarLayout(
        sidebarPanel(width=0,
          tags$script(src = 'http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
        ),
        mainPanel(
          includeMarkdown("inc_Definition.Rmd"),
          br(),
          h5(em(strong("References:"), "Wikipedia (", 
                a("[1]", href="http://en.wikipedia.org/wiki/Central_limit_theorem"), ",",
                a("[2]", href="http://en.wikipedia.org/wiki/Bernoulli_distribution"), ",",
                a("[3]", href="http://en.wikipedia.org/wiki/Exponential_distribution"), "and",
                a("[4]", href="http://en.wikipedia.org/wiki/Poisson_distribution"), "),",
             "Brian Caffo's little inference book ", a("Statistical inference for data science", href="https://leanpub.com/LittleInferenceBook/read#leanpub-auto-the-central-limit-theorem"),
             "and Charles M. Grinstead & J. Laurie Snell's ", a("Introduction to Probability", href="http://www.dartmouth.edu/~chance/teaching_aids/books_articles/probability_book/book.html"))
          )
        ),
        fluid = FALSE
      )
    ),
    tabPanel("Definition (cont.)",
      sidebarLayout(
        sidebarPanel(width=0),
        mainPanel(
          includeMarkdown("inc_Definition_cont.Rmd")
        )
      )
    ),
    tabPanel("Some distributions",
             sidebarLayout(
               sidebarPanel("", width=0),
               mainPanel(
                 includeMarkdown("inc_Examples.Rmd")
               )
             )
    ),
    "Examples",
    tabPanel("Help",
      sidebarLayout(
        sidebarPanel("", width=0),
        mainPanel(
          includeMarkdown("inc_Help.Rmd")
        )
      )
    ),
    tabPanel("Samples",
      sidebarLayout(
        sidebarPanel(
          selectInput("dist", "Distribution type:",
                      c("Die rolling", "Two dice rolling", "Flip coin",
                        "Exponential", "Poisson")
                      ),
          conditionalPanel(
            condition = "input.dist == 'Flip coin'",
            sliderInput("p", "Probability (p):",
                        value = 0.5,
                        min   = 0,
                        max   = 1)),
          conditionalPanel(
            condition = "input.dist == 'Exponential'",
            sliderInput("lambda_exp", "Lambda:",
                        value =   1,
                        min   =   1,
                        max   =  10)),
          conditionalPanel(
            condition = "input.dist == 'Poisson'",
            sliderInput("lambda_pois", "Lambda:",
                        value =   1,
                        min   =   1,
                        max   = 100)),
          br(),
          sliderInput("nSim", "Number of simulations:", 
                      value =  500,
                      min   =    1, 
                      max   = 1000),
          sliderInput("nEvt", "Number of events in each simulation:", 
                      value =   10,
                      min   =    1, 
                      max   =  100),
          checkboxInput("show_dist", "Show distribution curve", TRUE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Mean distrib.",
                    plotOutput("plot"),
                    textOutput("tInput"),
                    textOutput("sInput")), 
           tabPanel("Sample distrib.",
                    plotOutput("dplot"),
                    textOutput("tValues"),
                    textOutput("sValues")), 
            tabPanel("Table",
                    tableOutput("table"))
          )
        )
      )
    )
  )
))
