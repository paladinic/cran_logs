library(shiny)
library(shinyjs)
library(shinythemes)
library(cranlogs)
library(plotly)
library(DT)

TRY = function (x, verbose = FALSE){
  tryCatch(x, error = function(e) {
    if (verbose) {
      message(e)
    }
    return(NULL)
  })
}

ui <- fluidPage(
  theme = shinytheme('journal'),
  useShinyjs(),
  h2('CRAN LOGS'),
  tags$a(href='https://www.paladinic.com',em('by paladinic')),
  hr(),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = 'lib',
        label = 'library',
        placeholder = 'R library (e.g. linea)'
      ),
      selectInput(
        inputId = 'when',
        label = 'when',
        choices = c('last-day', 'last-week', 'last-month')
      ),
      actionButton(inputId = 'go', label = 'go',width = '100%'),
      shinyjs::hidden(downloadButton(outputId = "download", "download data",style='width:100%;margin-top:7px;')),
      hr(),
      textOutput(outputId = 'total')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',
                 plotlyOutput(outputId = 'plt')),
        tabPanel('Table',
                 DT::dataTableOutput(outputId = 'tb'))
      )
    )
  )
)

server <- function(input, output, session) {
  get_data = reactiveVal(NA)
  
  observeEvent(input$go, {
    df = cranlogs::cran_downloads(packages = input$lib,when = input$when) %>% 
      TRY()
    if(is.null(df)){
      #show notif
    }
    else{
      get_data(df)
    }
    
  })
  
  output$tb = renderDataTable({
    if(is.data.frame(get_data())){
      get_data()
    }
  })
    
  output$plt = renderPlotly({
    df = get_data()
    
    if (is.data.frame(df)) {
      plot_ly(data = df) %>%
        add_lines(x = ~date,y = ~count)
    }
    else{
    }
  })
  
  output$total = renderText({
    if(is.data.frame(get_data())){
      paste0('Total: ',sum(get_data()$count))
    }else{
      'Total: '
    }
  })
 
  observeEvent(get_data(), {
    if (!is.data.frame(get_data())) {
      shinyjs::hide("download")
    }
    else{
      shinyjs::show("download")
    }
  })
  
  output$download = downloadHandler(
    filename = function() {
      "cran_logs.csv"
    },
    content = function(file) {
      write.csv(get_data(), file, row.names = FALSE)
    }
  )
   
}

shinyApp(ui, server)