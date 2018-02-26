# This app seeks to show that the binomial distribution converges to Poisson when 
# N the trial size is large and p small.  p = lambda/N.



library(shiny)
my_css <- "
#downloadPlot {
 /* Change the background color of the download button to orange. */
 background: orange;
}
"

# Define UI for application that plots binomial and Poisson distributions.
ui <- fluidPage(
   tags$style(my_css),  
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
         textOutput(outputId = 'description'),
         downloadButton(outputId = 'downloadPlot', label = 'Download Plot')
      )
   )
)

# Define server logic required to draw the plot.
server <- function(input, output) {
   plot1 <- function(){
     par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
     p = input$lambda/input$size
     plot(pbinom(0:100,input$size,p),pch=input$shape, cex=input$cex, cex.lab=1.5, xlab='x',ylab='Probability')
     lines(ppois(0:100,input$lambda),type = 'l',col='red',lwd = input$cex)
     legend('topright', inset=c(-0.35,0), legend=c('Binomial','Poisson'),pch=c(input$shape,NA),lty=c(NA,1),col=c('black','red'),title='Distribution')
   }
  
   output$plot <- renderPlot({
     plot1()
     #print(p)
   })
   
   output$description <- renderText({
     paste0("The plot above shows size N = ", input$size, " and p = ", round(input$lambda/input$size,3), ".")
   })
   
   output$downloadPlot <- downloadHandler(
     filename = 'BinomialPoissonPlot.pdf',
     content = function(file){
       pdf(file)
       plot1()
       dev.off()
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

