#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that plots binomial and Poisson distributions.
ui <- fluidPage(
   
   # Application title
   titlePanel("Binomial to Poisson Distribution"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("size",
                     "Number of trials:",
                     min = 10,
                     max = 10000,
                     value = 100),
         sliderInput('lambda',
                     'Lambda:',
                     min = 0.1,
                     max = 100,
                     value = 10,
                     step = 0.1),
         sliderInput('shape','Shape', min = 1, max = 25, value = 16),
         sliderInput('cex','Shape and line size',min = 1, max = 7, value = 1.5, step=0.1)
      ),
         
      # Show a plot of the generated distribution
      mainPanel(
         h3('Plot'),
         plotOutput("plot"),
         textOutput(outputId = 'description')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     # Plot binomial and Poisson distributions
     par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
     p = input$lambda/input$size
     plot(pbinom(0:100,input$size,p),pch=input$shape, cex=input$cex, cex.lab=1.5, xlab='Quantile',ylab='Probability')
     lines(ppois(0:100,input$lambda),type = 'l',col='red',lwd = input$cex)
     legend('topright', inset=c(-0.35,0), legend=c('Binomial','Poisson'),pch=c(input$shape,NA),lty=c(NA,1),col=c('black','red'),title='Distribution')
   })
   
   output$description <- renderText({
     paste0("The plot above shows size N = ", input$size, " and p = ", round(input$lambda/input$size,3), ".")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
