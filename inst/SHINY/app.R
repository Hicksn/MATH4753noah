library(shiny)
library(ggplot2)
library(stats4)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation for Univariate Distributions"),

  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Uniform", "Gamma")),
      numericInput("n", "Sample Size:", 50, min = 10, max = 1000),
      actionButton("simulate", "Simulate New Sample")
    ),

    mainPanel(
      verbatimTextOutput("mle_out"),
      plotOutput("dataPlot"),
      plotOutput("likelihoodPlot")
    )
  )
)

server <- function(input, output) {
  data <- reactiveVal()

  observeEvent(input$simulate, {
    n <- input$n
    dist <- input$dist

    set.seed(Sys.time() |> as.integer())

    # Simulate data based on chosen distribution
    x <- switch(dist,
                "Normal" = rnorm(n, mean = 5, sd = 2),
                "Exponential" = rexp(n, rate = 1.5),
                "Poisson" = rpois(n, lambda = 3),
                "Uniform" = runif(n, min = 2, max = 8),
                "Gamma" = rgamma(n, shape = 2, rate = 1))
    data(x)
  })

  output$dataPlot <- renderPlot({
    req(data())
    ggplot(data.frame(x = data()), aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black") +
      labs(title = paste("Sample from", input$dist), x = "x", y = "Density") +
      theme_minimal()
  })

  output$mle_out <- renderPrint({
    req(data())
    x <- data()
    dist <- input$dist

    # Define log-likelihood functions and compute MLEs
    mle_fit <- switch(dist,
                      "Normal" = mle(function(mean, sd) -sum(dnorm(x, mean, sd, log = TRUE)),
                                     start = list(mean = mean(x), sd = sd(x))),
                      "Exponential" = mle(function(rate) -sum(dexp(x, rate, log = TRUE)),
                                          start = list(rate = 1 / mean(x))),
                      "Poisson" = mle(function(lambda) -sum(dpois(x, lambda, log = TRUE)),
                                      start = list(lambda = mean(x))),
                      "Uniform" = mle(function(min, max) -sum(dunif(x, min, max, log = TRUE)),
                                      start = list(min = min(x), max = max(x))),
                      "Gamma" = mle(function(shape, rate) -sum(dgamma(x, shape, rate, log = TRUE)),
                                    start = list(shape = 1, rate = 1))
    )

    summary(mle_fit)
  })

  output$likelihoodPlot <- renderPlot({
    req(data())
    x <- data()
    dist <- input$dist

    # Create grid for one-parameter likelihood visualization
    if (dist == "Exponential") {
      rates <- seq(0.1, 3, length.out = 200)
      ll <- sapply(rates, function(r) sum(dexp(x, rate = r, log = TRUE)))
      df <- data.frame(param = rates, logLik = ll)
      ggplot(df, aes(param, logLik)) +
        geom_line(color = "blue") +
        labs(title = "Log-Likelihood (Exponential)", x = "Rate", y = "Log-Likelihood") +
        theme_minimal()
    } else if (dist == "Poisson") {
      lambdas <- seq(0.1, 6, length.out = 200)
      ll <- sapply(lambdas, function(l) sum(dpois(x, lambda = l, log = TRUE)))
      df <- data.frame(param = lambdas, logLik = ll)
      ggplot(df, aes(param, logLik)) +
        geom_line(color = "blue") +
        labs(title = "Log-Likelihood (Poisson)", x = "Lambda", y = "Log-Likelihood") +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Likelihood visualization available for 1-parameter models only.",
                 size = 5, color = "gray40") +
        theme_void()
    }
  })
}

shinyApp(ui, server)
