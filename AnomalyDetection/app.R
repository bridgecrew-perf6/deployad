#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(caret)
library(xgboost)
library(reactable)
library(ids)
library(scales)

test_data <- readRDS("data/test.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("INDUSTRIAL MACHINE FAILURE DETECTION",
             tabPanel("About",fluidRow(
               column(2),
               column(7, 
                      p(h4("Detecting Anomalies can be a difficult task and especially in the case of labeled datasets 
                        due to some level of human bias introduced while labeling the final product as anomalous or good. 
                        These giant manufacturing systems need to be monitored every 10 milliseconds to capture their behavior
                        which brings in lots of information and what we call the Industrial IoT (IIOT). Also, hardly a manufacturer
                        wants to create an anomalous product. Hence, the anomalies are like a needle in a haystack which renders 
                        the dataset that is significantly Imbalanced. This tool is based on Kaggle dataset from ", a("Detecting Anomalies in Wafer Manufacturing", href="https://www.kaggle.com/arbazkhan971/anomaly-detection", target="_blank"),
                           "Attribute Description:
                        Feature1 - Feature1558 - Represents the various attributes that were collected from the manufacturing machine
                          Class - (0 or 1) - Represents Good/Anomalous class labels for the products"))),column(2)),
               
               hr(),
               p(h3("Developer")),
               p(a("Mohamed Sabri", href="https://www.linkedin.com/in/mohamed-sabri/", target="_blank"),style = "font-size:25px"),
               hr(),
               fluidRow(
                 column(3),
                 column(5, imageOutput("intro")),
                 column(3))
             ),
             tabPanel("Raw Data",DTOutput('raw')),
             tabPanel("Anomaly Detection",fluidPage(theme = shinytheme("flatly")),
                      tags$head(
                        tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                      pageWithSidebar(
                        headerPanel('Parameter'),
                        sidebarPanel(width = 3,
                                     selectInput('model', 'Select a model:',c('Model 1')),
                                     sliderInput("thres", "Threshold:",
                                                 min = 50, max = 100,
                                                 value = c(50,100)),
                                     checkboxGroupInput(inputId = "show",
                                                        label = 'Show status:', choices = c("Show status" = "status")),
                                     actionButton("run","Run the simulation")
                        ),
                        mainPanel(
                          column(4,dataTableOutput('table')),
                          column(5, imageOutput("factory"))
                        )
                      ))

             
             
             
  )
    )

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    model_trained <- readRDS("data/anomalydetect.rds")
    
    img_dec <- list("www/alert1.jpg","www/alert2.jpg","www/alert3.jpg","www/alert4.jpg")
    
    output$raw <- DT::renderDT({
        colnames(test_data)[51:60] <- c("CP1489", "SensorTemp23","CP5632", "SensorBar78","AH1198", "SensorPres88","AH9923", "SensorBar11","Equip12", "Class")
        
        datatable(test_data[,51:60]) 
    })
    
    output$intro <- renderImage({
      
      list(
        src = "www/base_image.jpg",
        filetype = "image/jpg",
        width = 1000,
        height = 600,
        alt = "This is a factory"
      )
      
    }, deleteFile = FALSE)
    
    predictions <- predict(object=model_trained, test_data[,0:59], type='prob')
    
    ID <- ids::random_id(length(predictions$anomaly), 4)
    
    result <- cbind(data.frame(ID),data.frame(predictions$anomaly))
    
    result$Status <- ifelse(as.numeric(result$predictions.anomaly) >=0.25, "Alert", "Normal")
    
    names(result)[names(result) == "predictions.anomaly"] <- "Probability"
    
    result$Probability <- round(result$Probability,2)
    
    result$Probability <- label_percent()(result$Probability)
    
    reval_omega <- reactiveVal(result[140,])
    
    reval_start <- reactiveVal(140)
    
    reval_end <- reactiveVal(140)
    
    reval_img <- reactiveVal("www/base_image.jpg")
    
    observeEvent(input$run, {
      
      observe({
        invalidateLater(500, session) 
        isolate({
          reval_start(reval_end()) 
          reval_end(reval_start() + 1) 
          omega_new <- result[reval_end(),] 
          reval_omega(rbind(omega_new, reval_omega())) 
          if (omega_new$Status=='Alert'){
            img_val <- sample(1:4, 1)
            url <- img_dec[[img_val]]
            reval_img(url)
          }
        })
      })
      
        output$table <- renderDataTable(datatable(reval_omega())%>%
                                          formatStyle(
                                            'Status',
                                            backgroundColor =  styleEqual(
                                              unique(result$Status), c('green', 'red')
                                            )
                                          )
        )
      
        output$factory <- renderImage({
          
          list(
            src = reval_img(),
            filetype = "image/jpg",
            width = 1000,
            height = 700,
            alt = "This is a factory"
          )
  
        }, deleteFile = FALSE)
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
