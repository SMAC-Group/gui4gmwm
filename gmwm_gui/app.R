library(shiny)
library(gmwm)
library(imudata)
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)

source("../R/plot_wv_and_datasheet.R")
# source("../R/load_internal_datasets.R")

const.degps_2_radps = 1/360 * 2*pi

const.DEFAULT_WN = 1

const.NAVCHIP.GYRO_WN = (0.003 * const.degps_2_radps * sqrt(250))^2 # [(rad/s)^2]
const.NAVCHIP.ACC_WN = (50 * 1e-6 * 10 * sqrt(250))^2 # [(m/s^2)^2]

# source: https://www.xsens.com/wp-content/uploads/2013/11/MTi-G_User_Manual_and_Technical_Documentation.pdf
const.MTIG.GYRO_WN = (0.05 * const.degps_2_radps * sqrt(100))^2 
const.MTIG.ACC_WN = (0.002 * sqrt(100))^2

#source: http://www.imar-navigation.de/downloads/IMU_FSAS.pdf
const.IMAR.GYRO_WN = (0.15 / 60 * sqrt(400) * const.degps_2_radps)^2
const.IMAR.ACC_WN = const.DEFAULT_WN

#source: http://www.northropgrumman.com/Capabilities/LN200FOG/Documents/ln200.pdf
const.LN200.GYRO_WN = (0.05 / 60 * sqrt(400) * const.degps_2_radps)^2


data("navchip")

imu6 = imu(imu6, gyros = 1:3, accels = 4:6, axis =
             c('X', 'Y', 'Z', 'X', 'Y', 'Z'), freq = 100)

data("imar.gyro")
data("ln200.gyro")

options(shiny.maxRequestSize=100*1024^2) # increses file limit from default-5MB to 100MB

ui <- shinyUI(fluidPage(

  title = "GMWM GUI",
  tabsetPanel(id = "tabs",
              # tabPanel("Datasheet", plotOutput(outputId = "plot_datasheet", height = "500px")),
              tabPanel("Model Data", plotOutput(outputId = "plot", height = "500px")),
              tabPanel("Selected Sensor", plotOutput(outputId = "plot2", height = "500px")),
              tabPanel("Summary", verbatimTextOutput(outputId = "summ", placeholder = TRUE))
  ),

  hr(),

  fluidRow(
    column(5,
           h3("Data" ),
           br(),

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

             fileInput("user_defined_txt_file", "Select TXT file:",
                       accept = c(
                         "text/txt",
                         "text/comma-separated-values,text/plain",
                         ".txt",
                         placeholder = "No file selected")
             ),
             sliderInput("user_defined_txt_file_column", "Select Column number:",
                         min=1, max=6, value=1),
             numericInput("user_defined_txt_frequency", label = "Set Frequency of dataset", value = 250) # frequency defined by the user

           ),

           column(7, radioButtons("robust", "Select estimator:", choices = c("Classic WV" = "classic", "Robust WV" = "robust"))),

                      br(),
           actionButton("fit1", label = "Plot / Update WV"),

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

           actionButton("fit2", label = "Reduce Model"),

           br(),

           actionButton("fit3", label = "Fit Model")

    ),

    column(3,
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
           ),
           
           checkboxInput("overlay_datasheet", label = "Overlay Datasheet Specifications", value = FALSE), 
           
           conditionalPanel(
             condition = "input.overlay_datasheet",
             numericInput("dsv_wn", label = "WN from Datasheet", value = const.DEFAULT_WN)
           )
           
    )
  )
))

server <- function(input, output, session) {

  # data created by the datasheet
  w <- reactiveValues(plot = FALSE,
                      fit = FALSE,
                      gmwm = NULL,
                      form = NULL,
                      freq = 100,
                      first_gmwm = NULL,
                      n = NULL)

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
                      
                      custom_data = FALSE,
                      custom_data_name = NULL,
                      custom_data_type = NULL,
                      custom_data_size = NULL,
                      custom_data_tot_colums = NULL,
                      datasheet_noise_model = NULL,
                      datasheet_values_make_sense = FALSE)


  ###3# PUSHING ON BUTTON "Update Datasheet WV plot"
  # observeEvent(input$fit0, {
  #   w$plot = TRUE
  #   w$fit = FALSE
  #
  #   if ("WN" %in% input$model_from_datasheet){
  #     if ("QN" %in% input$model_from_datasheet){
  #       if ("GM" %in% input$model_from_datasheet){
  #         m = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + WN(sigma2 = (input$dsv_wn)) + QN(q2 = (input$dsv_qn))
  #       } else{
  #         m = WN(sigma2 = (input$dsv_wn)) + QN(q2 = (input$dsv_qn))
  #       }
  #     }else{
  #       if ("GM" %in% input$model_from_datasheet){
  #         m = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + WN(sigma2 = (input$dsv_wn))
  #       } else {
  #         m = WN(sigma2 = (input$dsv_wn))
  #       }
  #     }
  #   }else{
  #     if ("QN" %in% input$model_from_datasheet){
  #       if ("GM" %in% input$model_from_datasheet){
  #         m = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + QN(q2 = (input$dsv_qn))
  #       } else {
  #         m = QN(q2 = (input$dsv_qn))
  #       }
  #     } else{
  #       if ("GM" %in% input$model_from_datasheet){
  #         m = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta)
  #       } else {
  #         m = WN(sigma2 = 0)
  #         w$plot = FALSE
  #       }
  #     }
  #   }
  #
  #   # Generate Data
  #   w$freq = input$dsv_frequency # the frequence defined by the user
  #   w$n = input$no_of_samples # numbers of samples
  #   Xt = gen_gts(n = w$n, model = m, freq =  w$freq) # generated data
  #
  #   w$form = wvar(as.numeric(Xt), robust = FALSE) # OR USE HERE DIRECTLY THE FORMULAS FROM THE HOMEPAGE, I WAS NOT ABLE TO FIND THEM THOUGH
  #
  #   updateNavbarPage(session, "tabs", selected = "Datasheet")
  ###### })

  # PUSHING ON BUTTON "Plot/update WV"
  observeEvent(input$fit1, {
    v$plot = TRUE
    v$fit = FALSE
    v$overlap_datasheet = input$overlay_datasheet
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
        } else {
          if(v$sensor_name == "imu6"){
            v$actual_datasheet_WN_parameter = const.MTIG.GYRO_WN
          } else {
            if(v$sensor_name == "imar.gyro"){
              v$actual_datasheet_WN_parameter = const.IMAR.GYRO_WN
            } else{
              if(v$sensor_name == "ln200.gyro"){
                v$actual_datasheet_WN_parameter = const.LN200.GYRO_WN
              } else{
                v$actual_datasheet_WN_parameter = const.DEFAULT_WN
              }
            }
          }
        } 
      }
      

      if( v$sensor_column == "Accel. X" || v$sensor_column == "Accel. Y" || v$sensor_column == "Accel. Z"){
        if (v$sensor_name == "navchip"){
          v$actual_datasheet_WN_parameter = const.NAVCHIP.ACC_WN
        } else {
          if(v$sensor_name == "imu6"){
            v$actual_datasheet_WN_parameter = const.MTIG.ACC_WN
          } else {
            v$actual_datasheet_WN_parameter = const.DEFAULT_WN
          }
        } 
      }

    } else{ #using custom data
      inFile <- input$user_defined_txt_file
      if (is.null(inFile))
        return(NULL)

      my_data = read.csv(inFile$datapath, header = FALSE, sep = ",")

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

    }
    
    updateNumericInput(session, "dsv_wn", value = v$actual_datasheet_WN_parameter)

    v$n = length(Xt)
    v$form = wvar(as.numeric(Xt), robust = (input$robust=="robust") )
    
    if (v$overlap_datasheet == TRUE){
      v$datasheet_values_make_sense = TRUE
      
      v$datasheet_noise_model = wn_to_wv(sigma2 = v$actual_datasheet_WN_parameter, tau = v$form$scales) 
      
    } else{
      v$datasheet_values_make_sense = FALSE
    }
    

    # if the user checked the "overlay datasheet checkbox
    # if (v$overlap_datasheet == TRUE){
    #   # condition is set to true for plottin ghte datasheet
    #   v$datasheet_values_make_sense = TRUE
    #   # the datasheet noise model is assembled
    #   if ("WN" %in% input$model_from_datasheet){
    #         if ("QN" %in% input$model_from_datasheet){
    #           if ("GM" %in% input$model_from_datasheet){
    #             v$datasheet_noise_model = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + WN(sigma2 = (input$dsv_wn)) + QN(q2 = (input$dsv_qn))
    #           } else{
    #             v$datasheet_noise_model = WN(sigma2 = (input$dsv_wn)) + QN(q2 = (input$dsv_qn))
    #           }
    #         }else{
    #           if ("GM" %in% input$model_from_datasheet){
    #             v$datasheet_noise_model = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + WN(sigma2 = (input$dsv_wn))
    #           } else {
    #             v$datasheet_noise_model = WN(sigma2 = (input$dsv_wn))
    #           }
    #         }
    #       }else{
    #         if ("QN" %in% input$model_from_datasheet){
    #           if ("GM" %in% input$model_from_datasheet){
    #             v$datasheet_noise_model = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta) + QN(q2 = (input$dsv_qn))
    #           } else {
    #             v$datasheet_noise_model = QN(q2 = (input$dsv_qn))
    #           }
    #         } else{
    #           if ("GM" %in% input$model_from_datasheet){
    #             v$datasheet_noise_model = GM(sigma2_gm = input$dsv_gm , beta = input$dsv_gm_beta)
    #           } else {
    #             # no model is selected, thus the datasheet noise model is set to empty
    #             v$datasheet_noise_model = NULL
    #             # in this case, the values make NO sense, maybe this flag is not required later on, to be discussed
    #             v$datasheet_values_make_sense = FALSE
    #           }
    #         }
    #       }
    #   # here would be the code, which convert the datasheet noise model "v$datasheet_noise_model" into a vector, if the condition is given
    #   if (v$datasheet_values_make_sense){
    #     # bla
    #     # bla
    #     # bla
    #     # bla
    #   }
    # }

    updateNavbarPage(session, "tabs", selected = "Selected Sensor")
  })

  #PUSHED THE BUTTON "FIT MODEL"
  observeEvent(input$fit3, {

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
    v$gmwm = gmwm_imu(model, Xt, G = input$num, seed = input$seed, robust = (input$robust=="robust"))
    v$form = v$gmwm
    v$first_gmwm = FALSE

    updateNavbarPage(session, "tabs", selected = "Selected Sensor")

  })

  # BUTTON REDUCE MODEL WHICH WILL USE THE AUTOIMU FUNCTION
  observeEvent(input$fit2, {

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


  output$plot_datasheet <- renderPlot({

    if (w$plot){
      b = w$form
      freq = w$freq
      b$scales = b$scales/freq
      duration = w$n/(freq*60*60)
      title = paste("Haar Wavelet Variance of DATASHEET, Duration: ", round(duration,1), "(h) @", freq, "(Hz)", sep = "")
      plot(b, axis.x.label = expression(paste("Scale ", tau, " [s]")), title = title, CI = FALSE)
    }else{
      plot(NA)
    }

  })

  # calc a specific VW and plot it in the tab "Selected Sensor"
  output$plot2 <- renderPlot({

    if (v$fit || v$plot){
      # for the real data
      a = v$form
      freq_a = v$freq
      a$scales = a$scales/freq_a
      duration_a = v$n/(freq_a*60*60)

      # for the simualted data
      b = w$form
      freq_b = w$freq
      b$scales = b$scales/freq_b
      duration_b = w$n/(freq_b*60*60)

      if (v$plot){ # should i plot just the real data?
        # if (w$plot && v$overlap_datasheet) { #should i plot the emulated data AND is the overlap-checkbox activated
        # compare_wvar(a, b, split = FALSE, CI = FALSE, legend.label = c('dataset','datasheet'), auto.label.wvar = FALSE)
        # } else{
          if (v$custom_data){ # is it custom data from a txt file?
            title = paste("Haar Wavelet Variance of TXT-FILE-DATA: ", v$custom_data_name, " (column number ", input$user_defined_txt_file_column, " out of ", v$custom_data_tot_colums,
                          ") - Filesize: ", round(v$custom_data_size/1024/1024,2), " [MB] - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
          }else{ # it is NOT custom data
            title = paste("Haar Wavelet Variance of DATASET: ", input$imu_obj, " (", input$sensors,
                          ") - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
          }
        
        
        if (v$overlap_datasheet){
          # plot_wv_and_datasheet(v$form, v$datasheet_noise_model)
          plot_wv_and_datasheet(a, v$datasheet_noise_model)
        } else {
          plot(a, axis.x.label = expression(paste("Scale ", tau, " [s]")), title = title)
        }
        

          # if(v$datasheet_values_make_sense){
          #    v$datasheet_values_make_sense = v$datasheet_values_make_sense
          # }

        # }
      }else{ # when doing the "gmwm modeling" plot

        if (v$custom_data){ # is it custom data from a txt file?
          title = paste("Haar Wavelet Variance of TXT-FILE-DATA: ", v$custom_data_name, " (column number ", input$user_defined_txt_file_column,
                        ") - Filesize: ", round(v$custom_data_size/1024/1024,2), " [MB] - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }else{ # it is NOT custom data
          title = paste("Haar Wavelet Variance of DATASET: ", input$imu_obj, " (", input$sensors,
                        ") - Duration: ", round(duration_a,1), "(h) @", freq_a, "(Hz)", sep = "")
        }


        plot(a, axis.x.label = expression(paste("Scale ", tau, " [s]")),
             process.decomp = "process_decomp" %in% input$option_plot,
             CI = "ci" %in% input$option_plot, title = title)

        # if(v$datasheet_values_make_sense){
        #   v$datasheet_values_make_sense = v$datasheet_values_make_sense
        # }
      }
    }else{
      plot(NA)
    }

  })

  # calc the 6 WV from the dataset and plot it in the tab "Model Data"
  output$plot <- renderPlot({

    plot(wvar(get(input$imu_obj)))

  })

  # print the summary in the summary-tab
  output$summ <- renderPrint({
    if (v$fit && "sum" %in% input$summary_plot){
      summary(v$form, inference = "ci" %in% input$summary_plot)
    }
  })

}

shinyApp(ui, server)
