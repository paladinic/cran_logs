library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(cranlogs)
library(plotly)
library(DT)

options(spinner.color="#000000")

TRY = function (x, verbose = FALSE){
  tryCatch(x, error = function(e) {
    if (verbose) {
      message(e)
    }
    return(NULL)
  })
}

ui <- fluidPage(
  tags$head(
    HTML('<link rel="shortcut icon" href="img/icon.png"/>')
  ),
  theme = shinytheme('journal'),
  useShinyjs(),
  h2('CRAN LOGS'),
  tags$a(href='https://www.paladinic.com',em('by paladinic')),
  hr(),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = 'lib',
        label = 'Package',
        placeholder = 'R Package(s) (e.g. linea,dplyr,shiny)'
      ),
      dateRangeInput(inputId = 'date',label = 'dates',start = Sys.Date()-90,end=(Sys.Date()-1),max = (Sys.Date()-1)),
      actionButton(inputId = 'go', label = 'Go',width = '100%'),
      shinyjs::hidden(downloadButton(outputId = "download", "Download Data",style='width:100%;margin-top:7px;')),
      hr(),
      textOutput(outputId = 'total')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',
                 withSpinner(plotlyOutput(outputId = 'plt'))),
        tabPanel('Table',
                 withSpinner(DT::dataTableOutput(outputId = 'tb')))
      )
    )
  )
)

server <- function(input, output, session) {
  get_data = reactiveVal(NA)
  
  observeEvent(input$go, {
    df = cranlogs::cran_downloads(packages = input$lib,
                                  from = input$date[1],
                                  to = input$date[2]) %>% 
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
        add_lines(x = ~date,y = ~count,color=~package)
    }
    else{
    }
  })
  
  output$total = renderText({
    if(is.data.frame(get_data())){
      paste0('Period Total: ',sum(get_data()$count))
    }else{
      'Period Total: '
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

options(shiny.host = '0.0.0.0')
options(shiny.port = 8881)

shinyApp(ui, server)