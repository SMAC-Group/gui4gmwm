library(gui4gmwm)

const.FIGURE_PLOT_HEIGHT = "600px"
const.FIGURE_PLOT_HEIGHT_REDUCED = "333px"

const.nb_of_digits = 7

# convert degrees-per-second to radians-per-second
const.degps_2_radps = 1/360 * 2*pi

# constant default frequency for custom data
const.DEFAULT_FREQ = 250 # [Hz]

# constants for the custom datasheet
const.DEFAULT_WN = 5.304377e-07
const.DEFAULT_QN = 1.681227e-06
const.DEFAULT_SIGMA2_GM = 7.348293e-09
const.DEFAULT_BETA_GM = 3.526037e-01
const.DEFAULT_RW = 1.268314e-12
const.DEFAULT_DR = 3.913529e-09
const.DEFAULT_BI = const.DEFAULT_WN


# source: https://www.xsens.com/wp-content/uploads/2013/11/MTi-G_User_Manual_and_Technical_Documentation.pdf
# https://www.xsens.com/tags/accelerometers/
# https://www.xsens.com/tags/gyroscopes/
# the frequency here is 100, because the dataset was acquired at this rate
const.MTIG.GYRO_WN = (0.05 * const.degps_2_radps * sqrt(100))^2 
const.MTIG.GYRO_BI = (20 / 3600 * const.degps_2_radps )^2
const.MTIG.ACC_WN = (0.002 * sqrt(100))^2
const.MTIG.ACC_BI = (30 * 1e-6 * 10)^2

# source: http://cdn-docs.av-iq.com/dataSheet//NavChip_Product_Brief.pdf, 
# the frequency here is 250, because the dataset was acquired at this rate
const.NAVCHIP.GYRO_WN = (0.003 * const.degps_2_radps * sqrt(250))^2 # [(rad/s)^2]
const.NAVCHIP.GYRO_BI = (10 / 3600 * const.degps_2_radps)^2
const.NAVCHIP.ACC_WN = (50 * 1e-6 * 10 * sqrt(250))^2 # [(m/s^2)^2]
const.NAVCHIP.ACC_BI = (0.05 * 1e-3 * 10)^2

# source: http://www.northropgrumman.com/Capabilities/LN200FOG/Documents/ln200.pdf
# the frequency here is 400, because the dataset was acquired at this rate
const.LN200.GYRO_WN = (0.05 / 60 * sqrt(400) * const.degps_2_radps)^2
const.LN200.GYRO_BI = NA
const.LN200.ACC_WN = const.DEFAULT_WN
const.LN200.ACC_BI = NA

# source: http://www.imar-navigation.de/downloads/IMU_FSAS.pdf
# the frequency here is 400, because the dataset was acquired at this rate
const.IMAR.GYRO_WN = (0.15 / 60 * sqrt(400) * const.degps_2_radps)^2
const.IMAR.GYRO_BI = (0.1 /3600 * const.degps_2_radps)^2
const.IMAR.ACC_WN = const.DEFAULT_WN
const.IMAR.ACC_BI = NA

# loading the four internal datasets
data("navchip") # NAVCHIP
imu6 = imu(imu6, gyros = 1:3, accels = 4:6, axis = c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100) #MTIG
data("imar.gyro") #IMAR
data("ln200.gyro") #LN200

# increses file limit from default-5MB to 100MB
options(shiny.maxRequestSize=100*1024^2) 

ui <- shinyUI(fluidPage(
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
  
  title = "GMWM GUI",
  tabsetPanel(id = "tabs",
              tabPanel("Model Data", plotOutput(outputId = "plot", height = const.FIGURE_PLOT_HEIGHT)),
              tabPanel("Selected Sensor", plotOutput(outputId = "plot2", height = const.FIGURE_PLOT_HEIGHT)),
              tabPanel("Summary", verbatimTextOutput(outputId = "summ", placeholder = TRUE))
  ),
  
  hr(),
  
  fluidRow(
    column(4,
           #h4("Data" ),
           
           radioButtons("data_input_choice", "Select data input:", choices = c("from library" = "library", "custom" = "custom")),
           
           conditionalPanel(
             condition = "input.data_input_choice == 'library'",
             
             selectInput("imu_obj", "Select IMU file:",
                         c("MTi-G" = "imu6",
                           "Navchip" = "navchip",
                           "LN-200" = "ln200.gyro",
                           "IMAR" = "imar.gyro"),
                         selected = 1),
             
             selectInput("sensors", "Select sensor", c("1"="1","2"="2", selected = 1))
           ),
           
           conditionalPanel(
             condition = "input.data_input_choice == 'custom'",
             
             fileInput("user_defined_txt_file", "Select INPUT file (max 100MB):",
                       accept = c(
                         "text/txt",
                         "text/comma-separated-values,text/plain",
                         ".txt",
                         ".imu",
                         placeholder = "No file selected")
             ),
             sliderInput("user_defined_txt_file_column", "Select column number:",
                         min=1, max=6, value=1),
             numericInput("user_defined_txt_frequency", label = "Set frequency of dataset", value = const.DEFAULT_FREQ) # frequency defined by the user
             
           ),
           
           column(7, radioButtons("robust", "Select estimator:", choices = c("Classic WV" = "classic", "Robust WV" = "robust"))),
           
           br(),
           
           actionButton("fit1", label = "Plot WV"),
           
           br(),
           
           uiOutput("choose_columns")
    ),
    
    
    column(4,
           # h4("GMWM Modelling"),
           
           checkboxGroupInput("model", "Select Model",
                              c("Quantization Noise" = "QN",
                                "White Noise" = "WN",
                                "Random Walk" = "RW",
                                "Drift" = "DR",
                                "Gauss-Markov" = "GM"),
                              selected = "WN"),
           conditionalPanel(
             condition = "input.model.indexOf('GM')>-1",
             sliderInput("gm_nb", "Number of Gauss-Markov Processes", 1, 5, 2)
           ),
           
           actionButton("fit3", label = "Fit Model"),
           
           br(),
           br(),
           br(),
           br(),
           br(),
           
           actionButton("fit2", label = "Reduce Model Automatically")
           
    ),
    
    column(4,
           # h4("Options"),
           
           checkboxGroupInput("option_plot", label = "Plot options:",
                              c("Process Decomp." = "process_decomp",
                                "Add Datasheet WV" = "datasheet"
                                #"Show CI of empirical WV" = "ci"
                              ),
                              selected = c("datasheet")),
           
           checkboxInput("overlay_datasheet", label = "Show Datasheet Specifications", value = FALSE),
           
           conditionalPanel(
             condition = "input.overlay_datasheet",
             numericInput("dsv_wn", label = " WN", value = format(const.DEFAULT_WN, digits = const.nb_of_digits)),
             numericInput("dsv_bi", label = " BIAS INSTABILITY", format(const.DEFAULT_BI, digits = const.nb_of_digits)),
             
             
             conditionalPanel(
               condition = "input.data_input_choice == 'custom'",
               numericInput("dsv_qn", label = " QN", value = format(const.DEFAULT_QN, digits = const.nb_of_digits)),
               
               column(6,
                      numericInput("dsv_sigma2_gm", label = " sigma2 GM", value = format(const.DEFAULT_SIGMA2_GM, digits = const.nb_of_digits))
               ),
               column(5,
                      numericInput("dsv_beta_gm", label = " beta GM", value = format(const.DEFAULT_BETA_GM, digits = const.nb_of_digits))
               ),
               
               
               numericInput("dsv_rw", label = " RW", value = format(const.DEFAULT_RW, digits = const.nb_of_digits)),
               numericInput("dsv_dr", label = " DR", value = format(const.DEFAULT_DR, digits = const.nb_of_digits)),
               column(2),
               column(10,
                      actionButton("button_reset_noise_params", label = "Reset Noise Params")
               )
             )
           ), 
           br(),
           
           checkboxGroupInput("summary_plot", label = "Summary options:",
                              c("Show CI of parameters" = "ci"),
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
  # library or custom dataset
  v <- reactiveValues(plot = FALSE,
                      fit = FALSE,
                      gmwm = NULL,
                      form = NULL,
                      freq = 100,
                      first_gmwm = NULL,
                      n = NULL,
                      sensor_name = NULL,
                      sensor_column = NULL,
                      overlap_datasheet = FALSE,
                      
                      actual_datasheet_WN_parameter = const.DEFAULT_WN, 
                      actual_datasheet_BI_parameter = NA,
                      actual_datasheet_QN_parameter = NA,
                      actual_datasheet_SIGMA2_GM_parameter = NA,
                      actual_datasheet_BETA_GM_parameter = NA,
                      actual_datasheet_RW_parameter = NA,
                      actual_datasheet_DR_parameter = NA,
                      
                      
                      first_time_plotting_6_pack = TRUE,
                      
                      custom_data = FALSE,
                      custom_data_name = NULL,
                      custom_data_type = NULL,
                      custom_data_size = NULL,
                      custom_data_tot_colums = NULL,
                      datasheet_noise_model = NULL,
                      datasheet_values_make_sense = FALSE)
  
  # PUSHING ON BUTTON "Plot WV"
  observeEvent(input$fit1, {
    
    withProgress(message = 'Calculating empirical WV...', value = 0, {
      
      v$plot = TRUE
      v$fit = FALSE
      v$overlap_datasheet = "datasheet" %in% input$option_plot
      
      if ("library" %in% input$data_input_choice){ #using library data
        my_data = get(input$imu_obj)
        Xt = my_data[, input$sensors]
        
        v$sensor_name = input$imu_obj
        v$sensor_column = input$sensors
        v$freq = attr(my_data, 'freq')
        v$custom_data = FALSE
        
        if( v$sensor_column == "Gyro. X" || v$sensor_column == "Gyro. Y" || v$sensor_column == "Gyro. Z"){
          if (v$sensor_name == "navchip"){
            v$actual_datasheet_WN_parameter = const.NAVCHIP.GYRO_WN
            v$actual_datasheet_BI_parameter = const.NAVCHIP.GYRO_BI
          } else {
            if(v$sensor_name == "imu6"){
              v$actual_datasheet_WN_parameter = const.MTIG.GYRO_WN
              v$actual_datasheet_BI_parameter = const.MTIG.GYRO_BI
            } else {
              if(v$sensor_name == "imar.gyro"){
                v$actual_datasheet_WN_parameter = const.IMAR.GYRO_WN
                v$actual_datasheet_BI_parameter = const.IMAR.GYRO_BI
              } else{
                if(v$sensor_name == "ln200.gyro"){
                  v$actual_datasheet_WN_parameter = const.LN200.GYRO_WN
                  v$actual_datasheet_BI_parameter = const.LN200.GYRO_BI
                } else{
                  v$actual_datasheet_WN_parameter = const.DEFAULT_WN
                  v$actual_datasheet_BI_parameter = NA
                }
              }
            }
          } 
        }
        
        if( v$sensor_column == "Accel. X" || v$sensor_column == "Accel. Y" || v$sensor_column == "Accel. Z"){
          if (v$sensor_name == "navchip"){
            v$actual_datasheet_WN_parameter = const.NAVCHIP.ACC_WN
            v$actual_datasheet_BI_parameter = const.NAVCHIP.ACC_BI
          } else {
            if(v$sensor_name == "imu6"){
              v$actual_datasheet_WN_parameter = const.MTIG.ACC_WN
              v$actual_datasheet_BI_parameter = const.MTIG.ACC_BI
            } else {
              v$actual_datasheet_WN_parameter = const.DEFAULT_WN
              v$actual_datasheet_BI_parameter = NA
            }
          } 
        }
        
        # reset other noise values in case custom data was loaded previosuly
        v$actual_datasheet_QN_parameter = const.DEFAULT_QN
        v$actual_datasheet_SIGMA2_GM_parameter = const.DEFAULT_SIGMA2_GM
        v$actual_datasheet_BETA_GM_parameter = const.DEFAULT_BETA_GM
        v$actual_datasheet_RW_parameter = const.DEFAULT_RW
        v$actual_datasheet_DR_parameter = const.DEFAULT_DR
        
        # reset other variables in case custom data was loaded previously
        v$custom_data_name = NULL
        v$custom_data_type = NULL
        v$custom_data_size = NULL
        v$custom_data_tot_colums = NULL
        
        
        
      } else{ #using custom data
        inFile <- input$user_defined_txt_file
        if (is.null(inFile))
          return(NULL)
        
        # if (inFile$type == '.txt'){
        my_data = read.csv(inFile$datapath, header = FALSE, sep = ",")
        # } else{
        # my_data = read.csv(inFile$datapathas, header = FALSE, sep = ",")
        # }
        
        
        
        if(input$user_defined_txt_file_column > ncol(my_data)){
          updateSliderInput(session, "user_defined_txt_file_column", value = ncol(my_data))
          the_column_number = ncol(my_data)
        } else{
          the_column_number = input$user_defined_txt_file_column
        }
        
        # update the slider number with the current number of colums
        updateSliderInput(session, "user_defined_txt_file_column", max = ncol(my_data))
        
        v$sensor_column = the_column_number
        Xt = my_data[, the_column_number]
        v$freq = input$user_defined_txt_frequency
        v$custom_data = TRUE
        v$custom_data_name = inFile$name
        v$custom_data_type = inFile$type
        v$custom_data_size = inFile$size
        v$custom_data_tot_colums = ncol(my_data)
        
        v$actual_datasheet_WN_parameter = input$dsv_wn
        v$actual_datasheet_BI_parameter = input$dsv_bi
        
        v$actual_datasheet_QN_parameter = input$dsv_qn
        v$actual_datasheet_SIGMA2_GM_parameter = input$dsv_sigma2_gm
        v$actual_datasheet_BETA_GM_parameter = input$dsv_beta_gm
        v$actual_datasheet_RW_parameter = input$dsv_rw
        v$actual_datasheet_DR_parameter = input$dsv_dr
        
        
      }
      
      updateNumericInput(session, "dsv_wn", value = format(v$actual_datasheet_WN_parameter, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_bi", value = format(v$actual_datasheet_BI_parameter, digits = const.nb_of_digits))
      
      updateNumericInput(session, "dsv_qn", value = format(v$actual_datasheet_QN_parameter, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_sigma2_gm", value = format(v$actual_datasheet_SIGMA2_GM_parameter, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_beta_gm", value = format(v$actual_datasheet_BETA_GM_parameter, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_rw", value = format(v$actual_datasheet_RW_parameter, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_dr", value = format(v$actual_datasheet_DR_parameter, digits = const.nb_of_digits))
      
      v$n = length(Xt)
      v$form = wvar(as.numeric(Xt), robust = (input$robust=="robust") )
      
      if (v$overlap_datasheet == TRUE){
        v$datasheet_values_make_sense = TRUE
        
        if ("library" %in% input$data_input_choice){ #using library data there is only WN, the BI is plotted separately directly in the plot
          v$datasheet_noise_model = wn_to_wv(sigma2 = v$actual_datasheet_WN_parameter, tau = v$form$scales)
          
        } else{
          intermediate = gm_to_ar1(theta = c(v$actual_datasheet_BETA_GM_parameter,
                                             v$actual_datasheet_SIGMA2_GM_parameter),
                                   freq = v$freq)
          
          v$datasheet_noise_model = wn_to_wv(sigma2 = v$actual_datasheet_WN_parameter, tau = v$form$scales) +
            qn_to_wv(q2 = v$actual_datasheet_QN_parameter, tau = v$form$scales) +
            rw_to_wv(gamma2 = v$actual_datasheet_RW_parameter, tau = v$form$scales) +
            dr_to_wv(omega = v$actual_datasheet_DR_parameter,  tau = v$form$scales) +
            ar1_to_wv(phi = intermediate[1], sigma2 = intermediate[2], tau = v$form$scales)
          
        }
      } else{
        v$datasheet_values_make_sense = FALSE
      }
      
      updateNavbarPage(session, "tabs", selected = "Selected Sensor")
    })
  })
  
  #PUSHED THE BUTTON "FIT MODEL"
  observeEvent(input$fit3, {
    
    withProgress(message = 'Fitting desired model...', value = 0, {
      
      if (is.null(v$first_gmwm)){
        v$first_gmwm = TRUE
      }
      v$fit = TRUE
      v$plot = FALSE
      
      if ("library" %in% input$data_input_choice){ #using library data
        my_data = get(input$imu_obj)
        Xt = my_data[, input$sensors]
        v$freq = attr(my_data, 'freq')
        v$custom_data = FALSE
      } else{ #using custom data
        inFile <- input$user_defined_txt_file
        if (is.null(inFile))
          return(NULL)
        
        my_data = read.csv(inFile$datapath, header = FALSE, sep = ",")
        Xt = my_data[, input$user_defined_txt_file_column]
        v$freq = input$user_defined_txt_frequency
        v$custom_data = TRUE
      }
      
      v$n = length(Xt)
      first = TRUE
      counter_model_size = 0
      
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
      
      if ("WN" %in% input$model){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = WN()
          first = FALSE
        }else{
          model = model + WN()
        }
      }
      
      if ("QN" %in% input$model){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = QN()
          first = FALSE
        }else{
          model = model + QN()
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
      
      if (is.null(input$seed)){
        input$seed = 1982
      }
      
      if (is.null(input$num)){
        input$num = 10^5
      }
      v$gmwm = gmwm_imu(model, Xt, G = input$num, seed = input$seed, robust = (input$robust=="robust"), freq = v$freq)
      v$form = v$gmwm
      v$first_gmwm = FALSE
      
      updateNavbarPage(session, "tabs", selected = "Selected Sensor")
      
    })
    
  })
  
  # BUTTON REDUCE MODEL WHICH WILL USE THE AUTOIMU FUNCTION
  observeEvent(input$fit2, {
    
    withProgress(message = 'Reducing model automatically...', value = 0, {
      
      if (is.null(v$first_gmwm)){
        v$first_gmwm = TRUE
      }
      v$fit = TRUE
      v$plot = FALSE
      
      if ("library" %in% input$data_input_choice){ #using library data
        my_data = get(input$imu_obj)
        Xt = my_data[, input$sensors]
        v$freq = attr(my_data, 'freq')
        v$custom_data = FALSE
      } else{ #using custom data
        inFile <- input$user_defined_txt_file
        if (is.null(inFile))
          return(NULL)
        
        my_data = read.csv(inFile$datapath, header = FALSE, sep = ",")
        Xt = my_data[, input$user_defined_txt_file_column]
        Xt = imu(data = Xt, freq = v$freq, gyros = 1)
        v$freq = input$user_defined_txt_frequency
        v$custom_data = TRUE
      }
      
      v$n = length(Xt)
      first = TRUE
      counter_model_size = 0
      
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
      
      if ("WN" %in% input$model){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = WN()
          first = FALSE
        }else{
          model = model + WN()
        }
      }
      
      if ("QN" %in% input$model){
        counter_model_size = counter_model_size + 1
        if (first == TRUE){
          model = QN()
          first = FALSE
        }else{
          model = model + QN()
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
      
      updateNavbarPage(session, "tabs", selected = "Selected Sensor")
    })
  })
  
  
  # BUTTON RESET-NOISE-PARAMS
  observeEvent(input$button_reset_noise_params, {
    
    withProgress(message = 'Resetting to default noise parameters...', value = 0, {
      
      updateNumericInput(session, "dsv_wn", value = format(const.DEFAULT_WN, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_bi", value = format(const.DEFAULT_BI, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_qn", value = format(const.DEFAULT_QN, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_sigma2_gm", value = format(const.DEFAULT_SIGMA2_GM, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_beta_gm", value = format(const.DEFAULT_BETA_GM, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_rw", value = format(const.DEFAULT_RW, digits = const.nb_of_digits))
      updateNumericInput(session, "dsv_dr", value = format(const.DEFAULT_DR, digits = const.nb_of_digits))
      
    })
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
                      label = "Selected sensor",
                      choices = cb_options,
                      selected = "")
    
  })
  
  # calc a specific VW and plot it in the tab "Selected Sensor"
  output$plot2 <- renderPlot({
    
    if (v$fit || v$plot){
      # for the real data
      a = v$form
      freq_a = v$freq
      a$scales = a$scales/freq_a
      duration_a = v$n/(freq_a*60*60)
      
      if (v$plot){ # should i plot just the real data?
        if (v$custom_data){ # is it custom data from a txt file?
          title = paste("Haar Wavelet Variance of TXT-FILE: ", v$custom_data_name, " (column # ", input$user_defined_txt_file_column, " / ", v$custom_data_tot_colums,
                        ") - Filesize: ", round(v$custom_data_size/1024/1024,2), " [MB] - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }else{ # it is NOT custom data
          title = paste("Haar Wavelet Variance of DATASET: ", input$imu_obj, " (", input$sensors,
                        ") - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }
        
        
        if ("datasheet" %in% input$option_plot){
          plot_wv_and_datasheet(a,
                                v$datasheet_noise_model, 
                                v$actual_datasheet_BI_parameter,
                                expression(paste("Scale ", tau, " [s]")),
                                prov_title = title)
        } else {
          plot(a,
               axis.x.label = expression(paste("Scale ", tau, " [s]")),
               title = title,
               CI = T, #"ci" %in% input$option_plot,
               title.size = 22, 
               axis.label.size = 20, 
               axis.tick.size = 17, 
               legend.title.size = 19, 
               legend.text.size = 19) + theme(legend.position = c(0, 0))
        }
        
      }else{ # when doing the "gmwm modeling" plot
        
        if (v$custom_data){ # is it custom data from a txt file?
          title = paste("Haar Wavelet Variance of TXT-FILE-DATA: ", v$custom_data_name, " (column number ", input$user_defined_txt_file_column,
                        ") - Filesize: ", round(v$custom_data_size/1024/1024,2), " [MB] - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }else{ # it is NOT custom data
          title = paste("Haar Wavelet Variance of DATASET: ", input$imu_obj, " (", input$sensors,
                        ") - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }
        
        if ("datasheet" %in% input$option_plot & !"process_decomp" %in% input$option_plot){
          plot_gmwm_and_datasheet(object = a, 
                                  datasheet = v$datasheet_noise_model, 
                                  v$actual_datasheet_BI_parameter,
                                  axis.x.label = expression(paste("Scale ", tau, " [s]")),
                                  prov_title = title)
        }else{
          plot(a,
               axis.x.label = expression(paste("Scale ", tau, " [s]")),
               process.decomp = "process_decomp" %in% input$option_plot,
               CI = T, #"ci" %in% input$option_plot,
               title = title,
               title.size = 22, 
               axis.label.size = 20, 
               axis.tick.size = 17, 
               legend.title.size = 19, 
               legend.text.size = 19) #+ theme(legend.position = c(0.1, 0.1))
          
        }
      }
    }else{
      plot(NA)
    }
  })
  
  # calc the 6 WV from the dataset and plot it in the tab "Model Data"
  output$plot <- renderImage({
    
    width  <- session$clientData$output_plot2_width
    height <- session$clientData$output_plot2_height
    
    if(input$data_input_choice == 'library'){
      if(input$imu_obj == 'imu6'){
        filename <- normalizePath(file.path('./initial_6_pack_plots', paste('imu6', '.png', sep='')))
        model_data_height = const.FIGURE_PLOT_HEIGHT
      } else{
        if(input$imu_obj == 'navchip'){
          filename <- normalizePath(file.path('./initial_6_pack_plots', paste('navchip', '.png', sep='')))
          model_data_height = const.FIGURE_PLOT_HEIGHT
        } else{
          if(input$imu_obj == 'ln200.gyro'){
            filename <- normalizePath(file.path('./initial_6_pack_plots', paste('ln200_gyro', '.png', sep='')))
            model_data_height = const.FIGURE_PLOT_HEIGHT_REDUCED
          } else{
            if(input$imu_obj == 'imar.gyro'){
              filename <- normalizePath(file.path('./initial_6_pack_plots', paste('imar_gyro', '.png', sep='')))
              model_data_height = const.FIGURE_PLOT_HEIGHT_REDUCED
            }
          }
        }
      }
    } else{
      filename <- normalizePath(file.path('./initial_6_pack_plots', paste('custom_dataset', '.png', sep='')))
      model_data_height = const.FIGURE_PLOT_HEIGHT_REDUCED
    }
    
    list(src = filename, height = model_data_height)
  }, deleteFile = FALSE)
  
  # print the summary in the summary-tab
  output$summ <- renderPrint({
    if (v$fit){
      
      if("ci" %in% input$summary_plot){
        summary_message = 'Generating summary with Confidence Intervals...'
      } else {
        summary_message = 'Generating summary...'
      }
      
      withProgress(message = summary_message, value = 0, {
        summary(v$form, inference = "ci" %in% input$summary_plot)
      })
    }
  })
}

shinyApp(ui, server)
