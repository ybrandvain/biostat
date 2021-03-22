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
library(cowplot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Getting a feel for normal distributions"),
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("do", "Generate a sample from the normal distribution"),
    # Application title
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Sample Size:",
                        min = 10,
                        max = 100,
                        value = 40)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("normPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$do ,{

    output$normPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        n           <- input$n
        my_dat <- tibble(x = rnorm(n = n, mean = 0, sd = 1))

        a <- ggplot(my_dat , aes(x = x)) + geom_histogram(bins = 20)+labs(title = "Histogram")
        b <- ggplot(my_dat , aes(x = x)) + geom_density(fill = "grey")+labs(title = "Density plot")
        c <- ggplot(my_dat , aes(sample = x)) + geom_qq() + geom_qq_line()+labs(title = "quantile-quantile plot")
        d <- ggplot(my_dat , aes(x = x)) + stat_ecdf()+labs(title = "Cumulative distribution")

        title <- ggdraw() +
            draw_label(
                sprintf("A sample of size %s from the standard normal distribution",n),
                fontface = 'bold',
                x = 0,
                hjust = 0
            ) +
            theme(
                # add margin on the left of the drawing canvas,
                # so title is aligned with left edge of first plot
                plot.margin = margin(0, 0, 0, 1)
            )

        plot_grid(title ,plot_grid(a,b,c, d, ncol = 2), ncol = 1, rel_heights = c(1,10))

    })})
}

# Run the application
shinyApp(ui = ui, server = server)
