library(shiny)

# Define UI for application that plot maximum and minimum of two identical distributions.
ui <- fluidPage(
  
  # Application title
  titlePanel("Maximum of Two Distributions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('choice','Minimum or maximum',c('minimum','maximum'),selected = 'maximum'),
      sliderInput('N','Number of samples:',
                  min = 10,
                  max = 10000,
                  value = 100),
      textOutput(outputId = 'description')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('uniform',plotOutput('Uniform')),
        tabPanel('exponential',plotOutput('Exponential'))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Uniform <- renderPlot({
    N = input$N
    X = runif(N)
    Y = runif(N)
    if (input$choice == 'maximum'){
    ind = which(X < Y)
    z = X 
    z[ind] = Y[ind] 
    
    plot(ecdf(z),pch=1,main='Max of 2 Uniform Distributions', xlab ='x',ylab ='Probability')
    x = seq(0,1,length.out = N)
    y = x^2
    lines(x,y,type='l',col='red',lwd = 2)
    legend('topleft', legend=c("ECDF", "Truth"),
           col=c("black", "red"), lty=c(NA,1),pch=c(1,NA), cex=0.8, text.font=4, bg='lightblue')
    }
    else {
      ind = which(X > Y)
      w = X
      w[ind] = Y[ind] 
      plot(ecdf(w),pch=1,main='Min of 2 Uniform Distributions', xlab ='x',ylab ='Probability')
      x = seq(0,1,length.out = N)
      y = 1 - (1 - x)^2
      lines(x,y,type='l',col='red',lwd = 2)
      #legend('topleft',c('ECDF','Truth'),pch=c(1,NA),lty=c(NA,1),col=c('black','red'))
      legend('topleft', legend=c("ECDF", "Truth"),
             col=c("black", "red"), lty=c(NA,1),pch=c(1,NA), cex=0.8, text.font=4, bg='lightblue')
    }
  })
  
  output$Exponential <- renderPlot({
    N = input$N
    X = rexp(N)
    Y = rexp(N)
    
    if (input$choice == 'maximum'){
    ind = which(X < Y)
    z = X 
    z[ind] = Y[ind] 
    
    #par(mfrow=c(2,2))
    plot(ecdf(z),pch=1,main='Max of 2 Exponential Distributions', xlab ='x',ylab ='Probability')
    x = seq(0,10,length.out = N)
    y = (1 - exp(-x))^2
    lines(x,y,type='l',col='red',lwd = 2)
    legend('topleft', legend=c("ECDF", "Truth"),
           col=c("black", "red"), lty=c(NA,1),pch=c(1,NA), cex=0.8, text.font=4, bg='lightblue')
    }
    else {
      ind = which(X > Y)
      w = X
      w[ind] = Y[ind] 
      plot(ecdf(w),pch=1,main='Min of 2 Exponenital Distributions', xlab ='x',ylab ='Probability')
      x = seq(0,10,length.out = N)
      y = 1 - exp(-x)^2
      lines(x,y,type='l',col='red',lwd = 2)
      #legend('topleft',c('ECDF','Truth'),pch=c(1,NA),lty=c(NA,1),col=c('black','red'))
      legend('topleft', legend=c("ECDF", "Truth"),
             col=c("black", "red"), lty=c(NA,1),pch=c(1,NA), cex=0.8, text.font=4, bg='lightblue')
      
    }
  })
  
  output$description <- renderText({
    paste0('The plots show maximum of two distributions of two different random variables.')
    #paste0("The plot above shows size N = ", input$size, " and p = ", round(input$lambda/input$size,3), ".")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
