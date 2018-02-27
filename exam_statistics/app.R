# The app is intend to obtain box plots of different discussion sections 
# to see exam statistics. 

# Work in progress app.


library(shiny)
library(plotly)
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
  titlePanel("Exam Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'file','Import a csv file'),
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3('Boxplots'),
      conditionalPanel(
        condition = T,
      plotlyOutput("plot")),
      downloadButton(outputId = 'downloadPlot', label = 'Download Plot'),
      width = 9
      
      
    )
  )
)

# Define server logic required to draw the plot.
server <- function(input, output) {
  plot1 <- reactive({
  #dat = read.csv(input$file$datapath,header=T,stringsAsFactors = T)
  main_dat = dat[2:nrow(dat)-1,] %>% select(Section,Exam.1..86938.)
  colnames(main_dat) = c('section','score')
  
  main_dat %>% group_by(section) %>% na.omit() %>% filter(row_number() >=2) %>% ggplot(aes(x=section,y=score))+ geom_boxplot()+
  scale_x_discrete(labels=c("Sec 2","Sec 3","Sec 4","Sec 5", "Sec 6", "Sec 7", "Sec 8", "Sec 9"))+theme_bw()
  })
  output$plot <- renderPlotly({
    ggplotly(plot1())
    #print(p)
  })
  
  input_file <- reactive({
    if (is.null(input$file)){
      return("")
    }
    dat = read.csv(input$file$datapath,header=T,stringsAsFactors = T)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = 'BoxPlot.pdf',
    content = function(file){
      pdf(file)
      ggplotly(plot1())
      dev.off()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

