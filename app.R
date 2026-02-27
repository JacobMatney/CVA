options(shiny.maxRequestSize = 30*1024^2, scipen = 999, shiny.launch.browser = .rs.invokeShinyWindowExternal)


source("Static.R")
source("R-R Detection.R")


packages = c(
  "shiny",
  "shinydashboard",
  "shinythemes",
  "shinyBS",
  "tidyverse",
  "ggplot2",
  "lme4",
  "readxl",
  "spiro",
  "signal",
  "zoo",
  "writexl",
  "MESS",
  "dplyr",
  "bslib", 
  "plotly",
  "shinyjs",
  "pracma")

package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


ui = page_sidebar(
  useShinyjs(),
  includeCSS("www/custom.css"),
  theme = bs_theme(preset = "cosmo", 
                   primary = "#cc2a36"),
  
  title = "Cerebral Vasomotor Analyzer",
  
  sidebar = sidebar(
    width = 300,
    
    p(
      card(card_header(strong("File Settings")),
           fileInput("file", label = "Upload File") |>
             tooltip("Select file for analysis"),
           
           radioButtons("condition", label = tags$u("Select Condition:"), 
                        choices = list("Condition A" = 1, "Condition B" = 2), selected = 1) |> 
             tooltip("Select which condition you wish to analyze; CondA = A, CondB = B"),
           
           numericInput("Fs", "Sample Frequency (Hz)", value= 1000) |>
             tooltip("Input the sampling frequency of your raw data (Hz)")),
      
      card(card_header(strong("CrCP & RAP Analyzer")), 
           numericInput("HtCorrect", label = "Height Correction (cm)", value = 0) |>
             tooltip("Input the distance that the probe is above the heart (cm); 0 = supine", options = list(container = "body")),
           
           
           numericInput("Base_Av_Interval", label = "Baseline Steady State (s)", value = 20) |>
             tooltip("Select the number of seconds from the end of baseline to average for steady state values"),
           
           numericInput("Stim_Av_Interval", label = "Stimulus Steady State (s)", value = 20) |>
             tooltip("Select the number of seconds from the end of stimulus to average for steady state values")),
      
      card(card_header(strong("Subcomponent Analyzer")), 
           numericInput("Av_Interval", label = "Initial Value Average (s)", value = 20) |>
             tooltip(HTML(paste0("Select the number of seconds from the end of baseline to represent V",tags$sub("0"), ", MAP", tags$sub("0"), ", C", tags$sub("0"), ", and R", tags$sub("0")))),
           
           numericInput("Av_Interval_2", label = "Stimulus Steady State (s)", value = 20)|>
             tooltip(HTML(paste0("Select the number of seconds from the end of stimulus to represent steady state V",tags$sub("MCAv"), ", V", tags$sub("CrCP"), ", V", tags$sub("RAP"), ", and V", tags$sub("MAP"))))),
      
      card(card_header(strong("Export")),
           textInput("text", label = "File Name:", value = "Enter file name...") |> 
             tooltip("Name file for export"),
           uiOutput("ExportButton")),
      
      
      tags$div(
        HTML("<p>Created by Jacob Matney (2025)</p><br>")),
    )
  ),
  
  
  navset_bar(
    
    
    nav_panel(
      "ECG Signal",
      
      h2("Beat Detection"), 
      fluidRow(
        column(width = 12, 
                           card(card_header("Baseline"),
                                full_screen = T, 
                                plotlyOutput("Plot"))),
                 
                 column(width = 12, 
                          card(card_header("Stimulus"),
                            full_screen = T, 
                                plotlyOutput("Plot2"))),
               
      )
    ),
  

  
  nav_panel(
    "Absolute Values",
    
    h2("Absolute Calculations"), 
    fluidRow(
      column(width = 6, 
             uiOutput("Baseline_Results"),
             navset_card_tab(
               nav_panel("MCAv",
                         card(full_screen = T, 
                              plotlyOutput("Base_MCAv_Plot"))),
               nav_panel("CrCP",
                         card(full_screen = T, 
                              plotlyOutput("Base_CrCP_Plot"))),
               nav_panel("RAP",
                         card(full_screen = T, 
                              plotlyOutput("Base_RAP_Plot"))),
               nav_panel("CPP",
                         card(full_screen = T, 
                              plotlyOutput("Base_CPP_Plot"))),
               nav_panel("CVCi",
                         card(full_screen = T, 
                              plotlyOutput("Base_CVCi_Plot"))),
               nav_panel("CPI",
                         card(full_screen = T, 
                              plotlyOutput("Base_CPI_Plot"))),
               nav_panel("MAP", 
                         card(full_screen = T,
                              plotlyOutput("Base_MAP_Plot"))), 
               nav_panel("PETCO2", 
                         card(full_screen = T,
                              plotlyOutput("Base_PETCO2_Plot"))))),
      
      column(width = 6, 
             uiOutput("Stim_Results"),
             navset_card_tab(
               nav_panel("MCAv",
                         card(full_screen = T, 
                              plotlyOutput("Stim_MCAv_Plot"))),
               nav_panel("CrCP",
                         card(full_screen = T, 
                              plotlyOutput("Stim_CrCP_Plot"))),
               nav_panel("RAP",
                         card(full_screen = T, 
                              plotlyOutput("Stim_RAP_Plot"))),
               nav_panel("CPP",
                         card(full_screen = T, 
                              plotlyOutput("Stim_CPP_Plot"))),
               nav_panel("CVCi",
                         card(full_screen = T, 
                              plotlyOutput("Stim_CVCi_Plot"))),
               nav_panel("CPI",
                         card(full_screen = T, 
                              plotlyOutput("Stim_CPI_Plot"))),
               nav_panel("MAP", 
                         card(full_screen = T,
                              plotlyOutput("Stim_MAP_Plot"))), 
               nav_panel("PETCO2", 
                         card(full_screen = T,
                              plotlyOutput("Stim_PETCO2_Plot"))))),
      
      column(width = 12, navset_card_tab(
        nav_panel("Baseline Dataframe",
                  card(full_screen = T, 
                       dataTableOutput("BaselineDF"))),
        nav_panel("Stimulus Dataframe", 
                  card(full_screen = T,
                       dataTableOutput("StimDF"))))),
      
      br(),
      
      
      h2("Model Fit"),
      column(width = 6,uiOutput("BaseMSNE_Results"),
             card(full_screen = T,
                  plotlyOutput("Abs_Check"))),
      
      column(width = 6, uiOutput("StimMSNE_Results"),
             card(full_screen = T,
                  plotlyOutput("Stim_Check"))),
      
      
      
      
      
      
    ),
    
    
  ),
  
  
  
  nav_panel("Subcomponent Analysis",
            h2("Dynamic Subcomponent Response"),
            
            fluidRow(
              column(width = 12, uiOutput("SubComp_Results")),
              card(full_screen = T, plotlyOutput("Data")),
              column(width = 3,card(card_header("AUC MCAv (Δ%∙s)"), 
                                    textOutput("AUC_MCAv")),
                     card(
                       full_screen = TRUE,
                       card_header("Δ% MCAv"),
                       card_body(plotlyOutput("Sub_MCAv_Plot"))
                     )),
              column(width = 3, card(card_header("AUC CrCP (Δ%∙s)"), 
                                     textOutput("AUC_CrCP")),
                     card(
                       full_screen = TRUE,
                       card_header("V CrCP"),
                       card_body(plotlyOutput("Sub_CrCP_plot"))
                     )),
              column(width = 3, card(card_header("AUC RAP (Δ%∙s)"), 
                                     textOutput("AUC_RAP")),
                     card(
                       full_screen = TRUE,
                       card_header("V RAP"),
                       card_body(plotlyOutput("Sub_RAP_Plot"))
                     )),
              column(width = 3, card(card_header("AUC MAP (Δ%∙s)"), 
                                     textOutput("AUC_MAP")),
                     card(
                       full_screen = TRUE,
                       card_header("V MAP"),
                       card_body(plotlyOutput("Sub_MAP_Plot"))
                     )),
              
              br(),
              h2("Model Fit"), 
              
              column(width = 6, uiOutput("SubMSNE_Results"),
                     card(full_screen = T,
                          plotlyOutput("SubComp_Check")))
              
              
              
            )
  )
)
)



   


server = function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  B2B_1 = reactive({
    
    req(input$file$datapath, input$condition, input$Fs)
    
    R_Detect(input$file$datapath, input$condition, input$Fs)
    
  })
  
  
  Work2BDone = reactive({
    
    req(input$file$datapath, input$Av_Interval, input$Base_Av_Interval, input$Stim_Av_Interval, input$Av_Interval_2, input$HtCorrect, input$condition, input$Fs)
    
    Static_Func(input$file$datapath, input$Av_Interval, input$Base_Av_Interval, input$Stim_Av_Interval, input$Av_Interval_2, 
                    input$HtCorrect, B2B_1()$Baseline.Data, B2B_1()$Stim.Data, input$Fs)
    
    
  })
 
  output$Plot = renderPlotly({
    req(B2B_1)
    B2B_1()$Base.Plot
  }) 
  
  
  output$Plot2 = renderPlotly({
    req(B2B_1)
    B2B_1()$Stim.Plot
  }) 
  
  
  output$Abs_Check = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Abs_Check_Plot 
    #ggplot_obj <<- Work2BDone()$Abs_Check_Plot 
    
  }) 
  
  output$Data = renderPlotly({
    req(Work2BDone)
    Work2BDone()$SubComp_Master_Plot
  }) 
  
  output$AUC_MCAV = renderText({
    req(Work2BDone)
    Work2BDone()$Subcomponent_AUC$AUC_MCAv
  }) 
  
  output$Sub_CrCP_plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Sub_CrCP_Plot
  }) 
  
  output$Sub_RAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Sub_RAP_Plot
  }) 
  
  output$Sub_MAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Sub_MAP_Plot
  }) 
  
  output$Sub_MCAv_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Sub_MCAv_Plot
  }) 
  
  output$Abs_Baseline = renderTable({
    req(Work2BDone)
    Work2BDone()$Abs_Baseline
  }) 
  
  output$Abs_Stim = renderTable({
    req(Work2BDone)
    Work2BDone()$Abs_Stim
  }) 
  
  output$BaselineDF = renderDataTable({
    req(Work2BDone)
    Work2BDone()$Abs_Baseline_Values
  }) 
  
  
  output$StimDF = renderDataTable({
    req(Work2BDone)
    Work2BDone()$Abs_Stress_Values
  }) 
  
  output$Stim_Check = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Abs_Stim_Check
  }) 
  
  output$BaseNMSE = renderText({
    req(Work2BDone)
    Work2BDone()$Base_MSNE
    
  }) 
  
  output$StimNMSE = renderText({
    req(Work2BDone)
    Work2BDone()$Stim_MSNE
  }) 
  
  output$AUC_MCAv = renderText({
    req(Work2BDone)
    Work2BDone()$AUC_MCAv
  }) 
  
  output$AUC_CrCP = renderText({
    req(Work2BDone)
    Work2BDone()$AUC_CrCP
  })
  
  output$AUC_RAP = renderText({
    req(Work2BDone)
    Work2BDone()$AUC_RAP
  })
  
  output$AUC_MAP = renderText({
    req(Work2BDone)
    Work2BDone()$AUC_MAP
  })
  
  output$Base_MCAv_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_MCAv_Plot
  }) 
  
  output$Stim_MCAv_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_MCAv_Plot
  }) 
  
  output$Base_CrCP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_CrCP_Plot
  }) 
  
  output$Stim_CrCP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_CrCP_Plot
  }) 
  
  output$Base_RAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_RAP_Plot
  }) 
  
  output$Stim_RAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_RAP_Plot
  }) 
  
  output$Base_MAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_MAP_Plot
  }) 
  
  output$Stim_MAP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_MAP_Plot
  }) 
  
  
  output$SubComp_MSNE = renderText({
    req(Work2BDone)
    Work2BDone()$SubComp_MSNE
  }) 
  
  
  output$SubComp_Check = renderPlotly({
    req(Work2BDone)
    Work2BDone()$SubComp_Check
  }) 
  
  output$SubCompAverage = renderTable({
    req(Work2BDone)
    Work2BDone()$SubCompAve
  }) 
  
  output$Base_CPP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_CPP_Plot
  }) 
  
  output$Base_CVCi_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_CVCi_Plot
  }) 
  
  output$Base_CPI_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_CPI_Plot
  }) 
  
  output$Stim_CPP_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_CPP_Plot
  }) 
  
  output$Stim_CVCi_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_CVCi_Plot
  }) 
  
  output$Stim_CPI_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_CPI_Plot
  }) 
  
  output$Stim_PETCO2_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Stim_PETCO2_Plot
  }) 
  
  output$Base_PETCO2_Plot = renderPlotly({
    req(Work2BDone)
    Work2BDone()$Base_PETCO2_Plot
  }) 
  
  output$Export_excel = downloadHandler(
    
    req(input$text),
    
    filename = function() {
      paste0(input$text, ".xlsx", sep = "")
    },
    
    content = function(file) {
      writexl::write_xlsx(list(Baseline_Dataframe = Work2BDone()$Abs_Baseline_Values, 
                               Stimulus_Dataframe = Work2BDone()$Abs_Stress_Values,
                               Base_Abs_Results = Work2BDone()$Abs_Baseline, 
                               Stim_Abs_Results = Work2BDone()$Abs_Stim,
                               Subcomponent_Results = Work2BDone()$SubCompAve, 
                               Subcomponent_AUC_Results = Work2BDone()$AUC_Data,
                               NMSE_Results = Work2BDone()$MNSEdf),
                          path = file)
      
    }
  )
  
  
  output$Baseline_Results = renderUI({
    if(Work2BDone()$Base_Fit < 0.1){
      card(
        card_header("Baseline Averages", class = "Good_Fit_Color"),
        tableOutput("Abs_Baseline"))
    } else {
      card(
        card_header("Baseline Averages", class = "Bad_Fit_Color"),
        tableOutput("Abs_Baseline"))
    }
  }
  )
  
  output$Stim_Results = renderUI({
    if(Work2BDone()$Stim_Fit < 0.1){
      card(
        card_header("Stimulus Averages", class = "Good_Fit_Color" ),
        tableOutput("Abs_Stim"))
    } else {
      card(
        card_header("Stimulus Averages", class = "Bad_Fit_Color"),
        tableOutput("Abs_Stim"))
    }
  }
  )
  
  
  output$SubComp_Results = renderUI({
    if(Work2BDone()$SubComp_Fit < 1.5){
      card(
        card_header("Average Subcomponent Response (Δ%)", class = "Good_Fit_Color"),
        tableOutput("SubCompAverage"))
    } else {
      card(
        card_header("Average Subcomponent Response (Δ%)", class = "Bad_Fit_Color"),
        tableOutput("SubCompAverage"))
    }
  }
  )
  
  
  
  output$BaseMSNE_Results = renderUI({
    if(Work2BDone()$Base_Fit < .1){
      card(card_header("Baseline NMSE", class = "Good_Fit_Color"), 
           textOutput("BaseNMSE"))
    } else {
      card(card_header("Baseline NMSE", class = "Bad_Fit_Color"), 
           textOutput("BaseNMSE"))
    }
  }
  )
  
  
  output$StimMSNE_Results = renderUI({
    if(Work2BDone()$Stim_Fit < .1){
      card(card_header("Stimulus NMSE", class = "Good_Fit_Color"), 
           textOutput("StimNMSE"))
    } else {
      card(card_header("Stimulus NMSE", class = "Bad_Fit_Color"), 
           textOutput("StimNMSE"))
    }
  }
  )
  
  
  output$SubMSNE_Results = renderUI({
    if(Work2BDone()$SubComp_Fit < 1.5){
      card(card_header("Subcomponent NMSE", class = "Good_Fit_Color"), 
           textOutput("SubComp_MSNE"))
    } else {
      card(card_header("Subcomponent NMSE", class = "Bad_Fit_Color"), 
           textOutput("SubComp_MSNE"))
    }
  }
  )
  
  output$ExportButton = renderUI({
    if(input$text == "Enter file name..."){
      
    } else if (input$text == ""){
      
    } else {
      downloadButton("Export_excel", label = "Export Report", icon = shiny::icon('circle-down')) |>
        tooltip("Download Excel document containing results from analyses")
    }
  }
  )
  
 
  
}

shinyApp(ui, server)