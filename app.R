## app.R ##
library(shiny)
library(DT)
library(shinydashboard)
library(caret)
library(rbokeh)
library(rCharts)
library(fBasics)
library(shinyBS)
library(plotly)

source("visualizations.R")

options(shiny.maxRequestSize = 9*1024^2)

ui <- dashboardPage(
  dashboardHeader(title="Basic Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      fileInput('featureMatrix', 'Upload Feature Matrix',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv')),
      fileInput('responseVector', 'Upload Response Vector',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv')),
      selectInput("problemType", "Problem Type:", choices = c("Classification", "Regression"), selected = "Classification"),
      uiOutput("singleSelect"),
      uiOutput("multiSelect"),
      uiOutput("labelSelect"),
      menuItem("Dataset Content", tabName = "datatable", icon = icon("table")),
      menuItem("Summary Statistics", tabName = "summaryStatsItem", icon = icon("table")),
      menuItem("Overview", tabName = "overview", icon = icon("bar-chart")),
      menuItem("Correlation", tabName = "correlation", icon = icon("bar-chart")),
      # menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")),
      menuItem("Dimensionality Reduction", tabName = "dimRed", icon = icon("chevron-down")),
      menuItem("Documentation", tabName = "doc", icon = icon("book"))
      # menuItem("Preprocessing", tabName = "preProc", icon = icon("list"))
    )
  ),
  
  dashboardBody(
    bsAlert("alert"),
    tabItems(
      # First tab content
      tabItem(tabName = "datatable",
              fluidRow(DT::dataTableOutput("dataTable"))
      ),
      tabItem(tabName = "summaryStatsItem",
              fluidRow(
                box(title="Dataset Feature Types", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    chartOutput("featureTypePlot", "nvd3")),
                box(title="Response Labels", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    chartOutput("responseClassesPlot", "nvd3"))
              ),
              fluidRow(
                box(title="Feature Distribution", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("violinPlot"))
              ),
              h2("Summary Statistics"),
              fluidRow(DT::dataTableOutput("summaryStats"))
      ),
      tabItem(tabName = "overview",
              fluidRow(
                box(title="QQ Plot", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("qqPlot")),
                box(title="Skewness", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("skewnessPlot"))
              ),
              fluidRow(
                box(title="Feature Histogram", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    sliderInput("bins",
                                "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30),
                    plotOutput("featureDistPlot"))
              )
      ),
      tabItem(tabName = "correlation",
              fluidRow(
                box(title="Pearson Correlation Overview", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("pairsPlot")),
                box(title="Pairwise Pearson Correlation", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    chartOutput("corMatPlot", "polycharts"))
              )
      ),
      tabItem(tabName = "dimRed",
              fluidRow(
                box(title="Dimension Reduction Method", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    selectInput("dimRedSelect", NULL, 
                                choices=c("PCA", "ICA"), selected=c("PCA")),
                    actionButton("computeDimRedBtn", "Compute"))),
              fluidRow(box(title="3D Projection", 
                           status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotlyOutput("dimRed3DPlot")),
                       # box(title="2D Projection",
                       #     status = "primary", solidHeader = TRUE,
                       #     plotlyOutput("dimRed2DPlot"))
                       box(title="2D Projection",
                           status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           height=10,
                           rbokehOutput("dimRed2DPlot"))
              ),
              fluidRow(box(title="Screeplot",
                           status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotlyOutput("screePlot"))
              )
      ),
      tabItem(tabName = "doc",
              fluidRow(
                    box(title="About this Dashboard", 
                        status = "primary", solidHeader = TRUE,
                        width=12,
                        includeMarkdown("doc/motivation.md"))
              ),
              fluidRow(
                box(title="Data Upload", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/dataUpload.md")),
                box(title="Dynamic Selectors", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/dynamicSelectors.md"))
              ),
              fluidRow(
                box(title="Dataset Content", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/datasetContent.md")),
                box(title="Summary Statistics", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/summaryStats.md"))
              ),
              fluidRow(
                box(title="Overview", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/overview.md")),
                box(title="Dimensionality Reduction", 
                    status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    includeMarkdown("doc/dimReduct.md")
                    )
              ),
              fluidRow(
                box(title="Author Information", 
                    status = "primary", solidHeader = TRUE,
                    width=12,
                    includeMarkdown("doc/author.md"))
              )
      )
      
      # tabItem(tabName = "preProc",
      #         fluidRow(
      #           box(
      #             selectizeInput(
      #               'preProcessing', 'Preprocessing Options',
      #               choices = c("BoxCox", "YeoJohnson",
      #                           "expoTrans", "center",
      #                           "scale", "spatialSign",
      #                           "range"),
      #               selected = c("center", "scale"),
      #               multiple = TRUE),
      #             actionButton("preProcessBtn", "PreProcess")
      #           ),
      #           fluidRow(
      #             verbatimTextOutput("preProcMsg")
      #           )
      #         )
      # )
    )
  )
)
      

server <- function(input, output, session) {
  
  featureMatrix <- reactive({
    inFile <- input$featureMatrix
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE, 
             sep = ",", 
             stringsAsFactors = T)
  })
  
  responseVector <- reactive({
    inFile <- input$responseVector
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE, 
             sep = ",", 
             stringsAsFactors = T)
  })
  
  inputData <- reactive({
    response <- responseVector()
    features <- featureMatrix()
    
    if (is.null(response))
      return(NULL)

    if (is.null(features))
      return(NULL)
    
    if (ncol(response) != 1) {
      createAlert(session, "alert", "inputAlert", title = "Warning",
                  content = "Response should be a column vector.",
                  style = "warning",
                  append = FALSE)
    }
    if (nrow(features) != nrow(response)) {
      createAlert(session, "alert", "inputAlert", title = "Warning",
                  content = "Dimensions of feature matrix and response Vector don't match.",
                  style = "warning",
                  append = FALSE)
    }
    
    data <- cbind(features, response)
    data
  })
  
  selectedSingleFeature <- reactive({
    input$singleFeature
  })
  
  selectedLabel <- reactive({
    input$label
  })
  
  selectedMultiFeatures <- reactive({
    input$multiFeature
  })
  
  output$featureTypePlot <- renderChart({
    featureTypePlot(inputData())
  })
  
  output$responseClassesPlot <- renderChart({
    responseClassesPlot(responseVector())
  })
  
  output$violinPlot <- renderPlot({
    selected   <- selectedMultiFeatures()
    label      <- selectedLabel()
    
    if (is.null(selected))
      return(NULL)
    
    if (is.null(label))
      return(NULL)
    
    data <- inputData()[, c(selected, label)]
    plotViolins(data, label)
  })
  
  output$featureDistPlot <- renderPlot({
    selected <- selectedSingleFeature()
    
    if (is.null(selected))
      return(NULL)
    
    data <- featureMatrix()
    x    <- data[, selected]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', 
         main = paste("Histogram of", selected))
  })
  
  output$skewnessPlot <- renderPlot({
    selected <- selectedMultiFeatures()
    
    if (is.null(selected))
      return(NULL)
    
    data <- inputData()[, selected]
    print(plotSkewness(data))
  })
  
  output$pairsPlot <- renderPlot({
    selected <- selectedMultiFeatures()
    condition_1 <- (length(selected) < 2)
    condition_2 <- ("factor" %in% sapply(inputData()[, selected], class))
    
    if (condition_1) {
      createAlert(session, "alert", "correlationAlert", title = "Warning",
                  content = "At least two features have to be selected.", 
                  style = "warning",
                  append = TRUE)
    }
    
    if (condition_2) {
      createAlert(session, "alert", "correlationAlert_2", title = "Warning",
                  content = "Selected features should be numeric.", 
                  style = "warning",
                  append = TRUE)
    }

    if (is.null(selected) || condition_1 || condition_2)
      return(NULL)
    
    data <- featureMatrix()[, selected]
    plotPairs(data)
  })
  
  output$corMatPlot <- renderChart({
    selected <- selectedMultiFeatures()
    
    if (length(selected) < 2) {
      createAlert(session, "alert", "correlationAlert_2", title = "Warning",
                  content = "At least two features have to be selected.", 
                  append = FALSE, style = "warning")
    }
    
    if (is.null(selected) || length(selected) < 2)
      return(NULL)
    
    data <- inputData()[, selected]
    data <- data[complete.cases(data), ]
    fig <- plotCorMat(data)
    fig$addParams(dom = 'corMatPlot')
    fig
  })
  
  output$qqPlot <- renderPlot({
    selected <- selectedSingleFeature()
    data <- inputData()[, selected]
    
    if (class(data) == "factor") {
      createAlert(session, "alert", "qqPlotAlert", title = "Warning",
                  content = "The selected feature should be numeric", 
                  append = FALSE, style = "warning")
    }
    
    print(qqPlot(data))
  })
  
  # output$dimRed2DPlot <- renderPlotly({
  #   input$computeDimRedBtn
  #   selected   <- isolate(selectedMultiFeatures())
  #   label      <- isolate(selectedLabel())
  #   
  #   if (length(selected) < 3) {
  #     createAlert(session, "alert", "dimRedAlert", title = "Warning",
  #                 content = "At least three features have to be selected.", 
  #                 append = FALSE, style = "warning")
  #   }
  #   
  #   if (is.null(selected) || length(selected) < 3)
  #     return(NULL)
  #     
  #   data <- inputData()[, c(selected, label)]
  # 
  #   fig <- NULL
  #   
  #   if (input$dimRedSelect == "PCA") {
  #     fig <- pca2dPlotly(data, label)
  #   }
  # 
  #   if (input$dimRedSelect == "ICA") {
  #     fig <- ica2dPlotly(data, label)
  #   }
  #   
  #   return(print(fig))
  # })

  output$dimRed2DPlot <- renderRbokeh({
    input$computeDimRedBtn
    selected   <- isolate(selectedMultiFeatures())
    label      <- isolate(selectedLabel())
    
    if (length(selected) < 3) {
      createAlert(session, "alert", "dimRedAlert", title = "Warning",
                  content = "At least three features have to be selected.", 
                  append = FALSE, style = "warning")
    }
    
    if (is.null(selected) || length(selected) < 3)
      return(NULL)
    
    data <- inputData()[, c(selected, label)]
    
    fig <- NULL
    
    if (input$dimRedSelect == "PCA") {
      fig <- pca2dPlot(data, label)
    }
    
    if (input$dimRedSelect == "ICA") {
      fig <- ica2dPlot(data, label)
    }
    
    return(print(fig))
  })
  
  output$dimRed3DPlot <- renderPlotly({
    input$computeDimRedBtn
    selected   <- isolate(selectedMultiFeatures())
    label      <- isolate(selectedLabel())
    
    if (length(selected) < 3) {
      createAlert(session, "alert", "dimRed3DAlert", title = "Warning",
                  content = "At least three features have to be selected.", 
                  append = FALSE, style = "warning")
    }
    
    if (is.null(selected) || length(selected) < 3)
      return(NULL)
    
    data <- inputData()[, c(selected, label)]
    
    fig <- NULL
    
    if (input$dimRedSelect == "PCA") {
      fig <- pca3dPlot(data, label)
    }
    
    if (input$dimRedSelect == "ICA") {
      fig <- ica3dPlot(data, label)
    }
    return(print(fig))
  })
  
  output$screePlot <-  renderPlotly({
    input$computeDimRedBtn
    selected   <- isolate(selectedMultiFeatures())
    
    data <- inputData()[, selected]
    
    fig <- NULL

    if (input$dimRedSelect == "PCA") {
      fig <- screePlotPCA(data)
    }
    return(print(fig))
  })
  
  output$dataTable <- DT::renderDataTable({
    selected   <- selectedMultiFeatures()
    
    if (is.null(selected))
      return(NULL)
    
    data <- inputData()[, selected]
    data
  }, options = list(pageLength = 25))
  
  output$summaryStats <- DT::renderDataTable({
    selected   <- selectedMultiFeatures()
    
    if (is.null(selected))
      return(NULL)
    
    data <- inputData()[, selected]
    basicStats(data)
  }, options = list(pageLength = 16))
  
  output$singleSelect <- renderUI({
    data <- inputData()
    
    selectInput("singleFeature", "Select Univariate Feature", 
                choices = colnames(data))
  })
  
  output$multiSelect <- renderUI({
    data <- inputData()
    
    selectizeInput(
      'multiFeature', 'Select Multivariate Features',
      choices = colnames(data),
      selected = colnames(data)[1:3], ## Fix the NULL issue
      multiple = TRUE)
  })
  
  output$labelSelect <- renderUI({
    data <- inputData()
    
    selectInput("label", "Select Label", 
                choices = colnames(data))
  })
  
  # preProcessEventMsg <- eventReactive(input$preProcessBtn, {
  #   preProc       <- preProcess(inputData(), method=input$preProcessing)
  #   procInputData <- reactive({predict(preProc, inputData())})
  #   msg <- "Data has been transformed!"
  # })
  # 
  # output$preProcMsg <- renderText({
  #   preProcessEventMsg()
  # })
}

shinyApp(ui, server)