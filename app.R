library(shiny)
library(gmwm)
library(imudata)
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)

data("navchip")

imu6 = imu(imu6, gyros = 1:3, accels = 4:6, axis = 
             c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100)
data("imar.gyro")
data("ln200.gyro")

ui <- shinyUI(fluidPage(
  
  title = "Diamonds Explorer",
  tabsetPanel(id = "tabs",
              tabPanel("IMU", plotOutput(outputId = "plot", height = "500px")), 
              tabPanel("Select Sensor", plotOutput(outputId = "plot2", height = "500px")),
              tabPanel("Summary", verbatimTextOutput(outputId = "summ", 
                                                     placeholder = TRUE))
  ),
  
  hr(),
  
  fluidRow(
    column(4,
           h3("Data"),
           br(),
           
           selectInput("imu_obj", "Select IMU object:",
                       c("MTi-G" = "imu6",
                         "Navchip" = "navchip",
                         "LN-200" = "ln200.gyro",
                         "IMAR" = "imar.gyro"), selected = 1),
           selectInput("sensors", "Select sensor", c("1"="1","2"="2", selected = 1)),
           fluidRow(
             column(7, radioButtons("robust", "Select estimator:", choices = c("Classic WV" = "classic", "Robust WV" = "robust"))),
             column(5, 
                    br(),
                    actionButton("fit1", label = "Plot WV"))
           ),
           uiOutput("choose_columns")
    ),
    
    column(4,
           h3("GMWM Modelling"),
           br(),
           checkboxGroupInput("model", "Select Model",
                              c("Quantization Noise" = "QN",
                                "White Noise" = "WN",
                                "Gauss-Markov" = "GM",
                                "Random Walk" = "RW",
                                "Drift" = "DR"), 
                              selected = "WN"),
           conditionalPanel(
             condition = "input.model.indexOf('GM')>-1", 
             sliderInput("gm_nb", "Number of Gauss-Markov Processes", 1, 5, 2)
           ),
           
           fluidRow(
             column(6, actionButton("fit2", label = "Reduce Model")),
             column(6, actionButton("fit3", label = "Fit Model"))
           )
    ),
    
    column(4,
           h3("Options"),
           br(),
           
           
           checkboxGroupInput("option_plot", label = "Plot options:", 
                              c("Process Decomp." = "process_decomp",
                                "Include CI" = "ci"),
                              selected = c("process_decomp","ci")),
           checkboxGroupInput("summary_plot", label = "Summary options:", 
                              c("Display summary" = "sum",
                                "Include CI" = "ci"),
                              selected = c("sum")),
           checkboxInput("edit_intern", label = "Edit Optimization Parameters", value = FALSE),
           
           conditionalPanel(
             condition = "input.edit_intern == 1", 
             numericInput("num", label = "Number of Simu. for Starting Values", value = 10^5),
             numericInput("seed", label = "Simulation seed", value = 1982)
           )
    )
  )
))

server <- function(input, output, session) {
  
  v <- reactiveValues(plot = FALSE, fit = FALSE, gmwm = NULL, 
                      form = NULL, freq = 100, first_gmwm = NULL,
                      n = NULL)
  
  observeEvent(input$fit1, {
    v$plot = TRUE
    v$fit = FALSE
    my_data = get(input$imu_obj)
    Xt = my_data[, input$sensors]
    v$n = length(Xt)
    v$form = wvar(as.numeric(Xt), robust = (input$robust=="robust"))
    v$freq = attr(my_data, 'freq')
    updateNavbarPage(session, "tabs", selected = "Select Sensor")
  })
  
  observeEvent(input$fit3, {
    
    if (is.null(v$first_gmwm)){
      v$first_gmwm = TRUE
    }
    v$fit = TRUE
    v$plot = FALSE
    my_data = get(input$imu_obj)
    sensor_axis = paste(input$sensor, input$axis)
    
    Xt = my_data[, input$sensors]
    v$n = length(Xt)
    first = TRUE
    counter_model_size = 0
    
    if ("QN" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = QN()
        first = FALSE
      }else{
        model = model + QN()
      }
    }
    
    if ("WN" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = WN()
        first = FALSE
      }else{
        model = model + WN()
      }
    }  
    
    if ("GM" %in% input$model){
      for (i in 1:input$gm_nb){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = GM()
          first = FALSE
        }else{
          model = model + GM()
        }
      }
    }
    
    if ("RW" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = RW()
        first = FALSE
      }else{
        model = model + RW()
      }
    }    
    
    if ("DR" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = DR()
        first = FALSE
      }else{
        model = model + DR()
      }
    }
    
    if (is.null(model)){
      model = 3*GM()
    }
    
    #if (v$first_gmwm == TRUE || counter_model_size == 1){
    
    if (is.null(input$seed)){
      input$seed = 1982
    }
    
    if (is.null(input$num)){
      input$num = 10^5
    }
    v$gmwm = gmwm_imu(model, Xt, G = input$num, seed = input$seed, robust = (input$robust=="robust")) 
    v$form = v$gmwm
    v$first_gmwm = FALSE
    #}else{
    #  v$form = update(v$gmwm, model) 
    #}
    
    updateNavbarPage(session, "tabs", selected = "Select Sensor")
    
  })
  
  
  observeEvent(input$fit2, {
    
    if (is.null(v$first_gmwm)){
      v$first_gmwm = TRUE
    }
    v$fit = TRUE
    v$plot = FALSE
    my_data = get(input$imu_obj)
    sensor_axis = paste(input$sensor, input$axis)
    
    Xt = my_data[, input$sensors]
    v$n = length(Xt)
    first = TRUE
    counter_model_size = 0
    
    if ("QN" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = QN()
        first = FALSE
      }else{
        model = model + QN()
      }
    }
    
    if ("WN" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = WN()
        first = FALSE
      }else{
        model = model + WN()
      }
    }  
    
    if ("GM" %in% input$model){
      for (i in 1:input$gm_nb){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = GM()
          first = FALSE
        }else{
          model = model + GM()
        }
      }
    }
    
    if ("RW" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = RW()
        first = FALSE
      }else{
        model = model + RW()
      }
    }    
    
    if ("DR" %in% input$model){
      counter_model_size = counter_model_size + 1
      if (first == TRUE){
        model = DR()
        first = FALSE
      }else{
        model = model + DR()
      }
    }
    
    if (is.null(model)){
      model = 3*GM()
    }
    
    a = auto_imu(Xt, model = model)
    v$form = a[[1]][[2]]
    
    updateNavbarPage(session, "tabs", selected = "Select Sensor")
  })
  
  
  
  
  dsnames <- c()
  
  data_set <- reactive({
    inFile <- input$imu_obj
    
    if (is.null(inFile))
      return(imu6)
    
    data_set <- get(input$imu_obj)
  })
  
  observe({
    dsnames <- colnames(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    
    updateSelectInput(session, "sensors",
                      label = "Select sensor",
                      choices = cb_options,
                      selected = "")
    
  })
  
  output$plot2 <- renderPlot({
    
    if (v$fit || v$plot){
      a = v$form
      freq = v$freq
      a$scales = a$scales/freq
      duration = v$n/(freq*60*60)
      title = paste("Haar Wavelet Variance of Dataset: ", input$imu_obj, " (", input$sensors,
                    ") - Duration: ", round(duration,1), "(h) @", freq, "(Hz)", sep = "")
      if (v$plot){
        plot(a, axis.x.label = expression(paste("Scale ", tau, " [s]")), title = title)
      }else{
        plot(a, axis.x.label = expression(paste("Scale ", tau, " [s]")), 
             process.decomp = "process_decomp" %in% input$option_plot, 
             CI = "ci" %in% input$option_plot, title = title)
      }
    }else{
      plot(NA)
    }
    
  })
  
  output$plot <- renderPlot({
    
    plot(wvar(get(input$imu_obj)))
    
  })
  
  output$summ <- renderPrint({ 
    if (v$fit && "sum" %in% input$summary_plot){
      summary(v$form, inference = "ci" %in% input$summary_plot)
    }
  })
  
}

shinyApp(ui, server)