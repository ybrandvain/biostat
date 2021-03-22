#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(infer)
library(cowplot)
# Define UI for application that draws a histogram

ui <- fluidPage(

    # Give the page a title
    titlePanel("How big must n be for the sampling distribution to be normal?"),

    # Generate a row with a sidebar
    sidebarLayout(

        # Define the sidebar with one input
        sidebarPanel(
            selectInput("n", "Sample size:", choices=c("1","2","5","10","25","50","100")),
            selectInput('dist', 'Distribution', choices=c("uniform","binomial, p = .1","all iris petal lengths", "mammal body weights")),
            hr(),
            helpText("We take 100 samples each of your specified size and calculate the mean of each.")
        ),

        # Create a spot for the barplot
        mainPanel(
            plotOutput("distPlot")
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        if(input$dist == "uniform"){ population_dist <- tibble(x = runif(1500)) }
        if(input$dist == "binomial, p = .1"){ population_dist <- tibble(x = rbinom(n = 1500, size = 1, prob = .1)) }
        if(input$dist == "all iris petal lengths"){ population_dist <- tibble(x = iris$Petal.Length) }
        if(input$dist == "mammal body weights"){ population_dist <- tibble(x = msleep$bodywt) }

        n_reps <- 100
        sampling_dist <- population_dist  %>%
            rep_sample_n(size = as.numeric(input$n), replace = TRUE, reps = n_reps) %>%
            summarise(mean_x = mean(x))

        if(input$n == "1") sampling_dist  <- mutate(population_dist , mean_x = x)


        a <- ggplot(sampling_dist, aes(x = mean_x)) + geom_histogram(bins = 20)+labs(title = "Histogram", x = ifelse(as.numeric(input$n_samples)==1,"individual observations","sample mean")) + geom_vline(xintercept = mean(population_dist$x), color = "red",lty = 2)
        b <- ggplot(sampling_dist , aes(x = mean_x)) + geom_density(fill = "grey")+labs(title = "Density plot", x = ifelse(as.numeric(input$n_samples)==1,"individual observations","sample mean"))+ geom_vline(xintercept = mean(population_dist$x), color = "red",lty = 2)
        c <- ggplot(sampling_dist , aes(sample = mean_x)) + geom_qq() + geom_qq_line()+labs(title = "quantile-quantile plot")
        d <- ggplot(sampling_dist , aes(x = mean_x)) + stat_ecdf()+labs(title = "Cumulative distribution", x = ifelse(as.numeric(input$n_samples)==1,"individual observations","sample mean"))+ geom_vline(xintercept = mean(population_dist$x), color = "red",lty = 2)

        title <- ggdraw() +
            draw_label(
                ifelse(input$n != 1,
                       sprintf("Distribution of sample means (Each w. n = %s) from the %s distribution",input$n,input$dist),
                       sprintf("Actual values from the %s distribution",input$dist)),
                fontface = 'bold',
                x = 0,
                hjust = 0
            ) +
            theme(
                # add margin on the left of the drawing canvas,
                # so title is aligned with left edge of first plot
                plot.margin = margin(0, 0, 0, 1)
            )

        bottom <- ggdraw() +
            draw_label(
                " -  -  -  Actual mean", color = "red",
                x = 0,
                hjust = 0
            ) +
            theme(
                # add margin on the left of the drawing canvas,
                # so title is aligned with left edge of first plot
                plot.margin = margin(0, 0, 0, 1)
            )
        plot_grid(title ,plot_grid(a,b,c, d, ncol = 2),bottom, ncol = 1, rel_heights = c(1,10,1))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
