library(shiny)
library(dplyr)
library(prophet)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinyjs)

# UI ------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  shinyjs::useShinyjs(),
  titlePanel("Prophet Forecasting"),
  
# Sidebar
  fluidPage(sidebarPanel(width=3,
                         tabsetPanel(
                           ## Tab prophet parameters
                           tabPanel(HTML("Parameters"),
                                    
                                    ### paramter: growth
                                    radioButtons("growth","",
                                                 c('linear'), inline = TRUE),
                                    
                                    ### parameter: yearly.seasonality
                                    checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                    
                                    ### parameter: weekly.seasonality 
                                    checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                    ### parameter: n.changepoints
                                    numericInput("n.changepoints","n.changepoints", value = 25),
                                    
                                    ### parameter: seasonality.prior.scale
                                    numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                    
                                    ### parameter: changepoint.prior.scale
                                    numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
                                    
                                    ### parameter: holidays.prior.scale
                                    numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                    
                                    ### parameter: mcmc.samples
                                    numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                    
                                    ### parameter: interval.width
                                    numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                    ### parameter: uncertainty.samples
                                    numericInput("uncertainty.samples","uncertainty.samples", value = 1000)
                           ),
                           
                           ## Tab predict parameters -------------------------
                           tabPanel(HTML("Prediction <br> Setting"),
                                    
                                    ## make_future_dataframe() parameters ------------------
                                    ### paramater: periods
                                    numericInput("periods","periods",value=10),
                                    
                                    ### parameter: freq
                                    selectInput("freq","freq",
                                                choices = c('day', 'week', 'month', 'quarter','year'))
                           ))
                         
  ),
  
  # Main panel -------------------------
  mainPanel(
    fluidRow(
      ## upload file -----------------
      column(width = 6,
             fileInput("ts_file","Upload File (*.csv)",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")))),
    fluidRow( 
      ## plot button -----------------
      column(width = 6,
             shinyjs::disabled(actionButton("plot_btn2", "Fit & Plot",
                                            style = "width:80%; margin-top: 25px;")))
    ),
    
    fluidRow(column(width = 12,
                    uiOutput("msg"))),
    
    fluidRow(column(width = 12,
                    uiOutput("msg2"))),
    
    fluidRow(column(width = 12)
    ),
    
    ## plot/results tabs --------------------------------
    fluidRow(column(width=12,
                    tabsetPanel(
                      tabPanel("Forecast Plot",
                               conditionalPanel("input.plot_btn2",
                                                div(id = "output-container1",
                                                    tags$img(src = "spinner.gif",
                                                             id = "loading-spinner"),
                                                    plotOutput("ts_plot")
                                                )
                               )
                      ),
                      tabPanel("Prophet Plot Components",
                               conditionalPanel("input.plot_btn2",
                                                div(id = "output-container2",
                                                    tags$img(src = "spinner.gif",
                                                             id = "loading-spinner"),
                                                    plotOutput("prophet_comp_plot")
                                                )
                               )
                      )
                      
                      # tabPanel("Forecast Results",
                      #          conditionalPanel("output.data",
                      #                           uiOutput("dw_button")
                      #          ),
                      #          conditionalPanel("input.plot_btn2",
                      #                           div(id = "output-container3",
                      #                               tags$img(src = "spinner.gif",
                      #                                        id = "loading-spinner"),
                      #                               dataTableOutput("data")))
                      # )
                    )
    )
    ),
    
    ## test output --------
    verbatimTextOutput("test")
  )
  )
           )

# Server
server <- function(input, output, session) {
  
  ## function: duplicatedRecative values
  duplicatedRecative <- function(signal){
    values <- reactiveValues(val="")
    
    observe({
      values$val <- signal()
    })
    
    reactive(values$val)
  }
  
  ## read csv file data
  dat <- reactive({
    req(input$ts_file)
    
    file_in <- input$ts_file
    
    # read csv
    read.csv(file_in$datapath, header = T) 
  })
  
  ## Toggle submit button state according to data
  observe({
    if(!(c("ds","y") %in% names(dat()) %>% mean ==1))
      shinyjs::disable("plot_btn2")
    else if(c("ds","y") %in% names(dat()) %>% mean ==1)
      shinyjs::enable("plot_btn2")
  })
  
  ## logistic_check -------------------
  # logistic_check <- eventReactive(input$plot_btn2, {
  #   # req(dat())
  #   if( (input$growth == "logistic") & !("cap" %in% names(dat())) )
  #   {
  #     return("error")
  #   }
  #   else 
  #     return("no_error")
  # })
  
  ## create prophet model -----------
  prophet_model <- eventReactive(input$plot_btn2,{
    req(dat(),
        input$n.changepoints,
        input$seasonality_scale, input$changepoint_scale,
        input$holidays_scale, input$mcmc.samples,
        input$mcmc.samples, input$interval.width,
        input$uncertainty.samples)
    
    # if(input$growth == "logistic"){
    #   validate(
    #     need(try("cap" %in% names(dat())),
    #          "Error: for logistic 'growth', the input dataframe must have a column 'cap' that specifies the capacity at each 'ds'."))
    # }

    # mutate dataframe
    datx <- dat() %>% 
      mutate(y = log(y))
    
    kk <- prophet(datx,
                  growth = input$growth,
                  changepoints = NULL,
                  n.changepoints = input$n.changepoints,
                  yearly.seasonality = input$yearly,
                  weekly.seasonality = input$monthly,
                  seasonality.prior.scale = input$seasonality_scale,
                  changepoint.prior.scale = input$changepoint_scale,
                  holidays.prior.scale = input$holidays_scale,
                  mcmc.samples = input$mcmc.samples,
                  interval.width = input$interval.width,
                  uncertainty.samples = input$uncertainty.samples,
                  fit = T)
    return(kk)
  })
  
  ## dup reactive
  p_model <- duplicatedRecative(prophet_model)
  
  ## Make dataframe with future dates for forecasting
  future <- eventReactive(input$plot_btn2,{
    req(p_model(),input$periods, input$freq)
    make_future_dataframe(p_model(),
                          periods = input$periods,
                          freq = input$freq,
                          include_history = T
                          )
  })
  
  ## dup reactive --------------
  p_future <- duplicatedRecative(future)
  
  ## predict future values -----------------------
  forecast <- reactive({
    req(prophet_model(),p_future())
    predict(prophet_model(),p_future())
  })
  
  ## dup reactive --------------
  #p_forecast <- duplicatedRecative(forecast)
  
  ## plot forecast -------------
  output$ts_plot <- renderPlot({
    g <- plot(p_model(), forecast())
    g+theme_classic()
  })
  
  ## plot prophet components --------------
  output$prophet_comp_plot <- renderPlot({
    prophet_plot_components(p_model(),forecast())
  })
  
  ## create datatable from forecast dataframe --------------------
  # output$data <- renderDataTable({
  #   # req(logistic_check()!="error")
  #   datatable(forecast()) %>% 
  #     formatRound(columns=2:17,digits=4)
  # })
  
  ## error msg ------------------------
  output$msg <- renderUI({
    if(c("ds","y") %in% names(dat()) %>% mean !=1)
      "Invalid Input: dataframe should have at least two columns named (ds & y)"
  })
  
  }

# Run the application ---------------------------
shinyApp(ui = ui, server = server)