#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


server <- function(input, output){
  
  # Confidence Level
  cl <- reactive({
    if (is.na(input$id)){
      return(NA)
    }else{
      # Plant the random number seed
      set.seed(input$id)
      cl <- sample(88:96, 1)
      return(cl)
    }
  })
  
  # Set the mean for numerical data
  mu <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      mu <- 12
    }})
  
  # Creating categorical data
  catData <- reactive({
    if (is.na(input$id)){
      userdata <- data.frame(Item=NA, Answers=NA)
      return(userdata)
    }else{
      # Plant the random number seed
      set.seed(input$id) 
      cl <- sample(88:96, 1)
      n <- sample(60:70, 1)
      
      A_one <- 1:n
      group_one <- rep(c('Correct'), times = 80)
      group_two <- rep(c('Incorrect'), times = 20)
      population <- c(group_one, group_two)
      A_two <- sample(population, n, replace = TRUE)
      
      userdata <- data.frame(A_one, A_two)
      colnames(userdata) <- c('Item', 'Answers')
      return(userdata)
    }
  }) # end reactive
  
  # Creating numerical data
  numData <- reactive({
    # Plant the random number seed
    if (is.na(input$id)){
      userdata <- data.frame(Student=NA, Scores=NA, Notes=NA)
      return(userdata)
    }else{
      set.seed(input$id) 
      n <- 60
      
      A_one <- 1:n
      A_two <- rnorm(n, 12, 0.05)
      A_three <- A_one %% 2
      
      userdata <- data.frame(A_one, A_two, A_three)
      col_headings <- c('Student', 'Scores', 'Notes')
      colnames(userdata) <- col_headings
      return(userdata)
    }
  }) # end reactive
  
  output$downloadnum <- downloadHandler(
    filename = function() {
      paste('Num', input$id, '.csv', sep='')
    },
    content = function(file) {
      write.csv(data.frame(numData()[,1:2]), file, row.names = FALSE)
    }
  )
  
  output$downloadcat <- downloadHandler(
    filename = function() {
      paste('Cat', input$id, '.csv', sep='')
    },
    content = function(file) {
      write.csv(data.frame(catData()), file, row.names = FALSE)
    }
  )
  
  # Output: Textblock 1 ----
  output$textblocknum <- renderText({
    out <- paste("<ul>",
                 "<li>Each row represents the volume of one can (fl oz)</li>",
                 "</ul>")
    out
  })
  
  output$textblockcat <- renderText({
    out <- paste("<ul>",
                 "<li>Each row represents one detection.</li>",
                 "<li>The second column shows the detection results.</li>",
                 "<li><q>Correct</q> means a medical detection dog accurately identified the cancer urine sample.</li>",
                 "<li><q>Incorrect</q> means a medical detection dog failed to identify the cancer urine sample.</li>",
                 "</ul>")
    out
  })
  
  
  # Output: Sample data table ----
  output$catTable <- DT::renderDT({
    datatable(catData(), rownames = FALSE, options = list(
      pageLength = 200,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))%>%
      formatStyle("Answers", backgroundColor=styleEqual(c("Correct","Incorrect"),c('orange','gray')))
  })
  
  # Output: Sample data table ----
  output$numTable <- DT::renderDT({
    datatable(numData(), rownames = FALSE, options = list(
      pageLength = 100,
      dom = "t",
      columnDefs = list(list(className ='dt-center', targets = 0:1), 
                        list(visible=FALSE, targets=2))))%>%
      formatStyle("Notes", backgroundColor=styleEqual(c(1,0),c('orange','gray')), target = "row")%>%
      formatRound(columns=c('Scores'), digits=2)
  })
  
} #end server
