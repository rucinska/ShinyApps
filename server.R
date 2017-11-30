
library(shiny)
library(shinydashboard)




shinyServer(function(input, output, session) {
  values <- reactiveValues(
    plots = list()
  ) 
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, sep = ",", stringsAsFactors = FALSE)
    df[is.na(df)] <- 0
    
    return(df)
  })
  
  exp_con <- 
    
    filtereddata <- eventReactive({
      #validate(need(input$dataset != "","Please select a data set in csv format only!!!"))#
      input$update
      data()
    },  {
      req(data())
      if(is.null(input$select) || input$select == "")
        data() else if (is.null(input$exp_con) || input$exp_con == "")
          data()[, colnames(data()) %in% input$select]
      else {
        data()[, colnames(data()) %in%  exp_con_sel()]
      }
      
    })
  
  exp_con_sel <- reactive({
    names_col <- c("class", "length", "DB")
    gs <- c("early", "mid", "late", "stat")
    t <- c("13","20","30","4")
    ifelse(input$exp_con == "gs", names_col <-append(names_col, grep(paste(gs_3,collapse="|"), colnames(data()), value=TRUE)),
           ifelse(input$exp_con == "temp",names_col <-append(names_col,grep(paste(t,collapse="|"),colnames(data()), value=TRUE)),
                  ifelse(input$exp_con == "salt",names_col <-append(names_col,grep("Na",colnames(data()), value=TRUE)),
                         ifelse(input$exp_con == "met",names_col <-append(names_col,grep("Met",colnames(data()), value=TRUE)),
                                ifelse(input$exp_con == "tri",names_col <-append(names_col,grep("TX",colnames(data()), value=TRUE)),
                                       colnames(data()))))))
    return(names_col)
  })
  
  
  observeEvent(
    data(), {
      updateSelectInput(session, "select", choices=colnames(data()), selected = colnames(data()))
      
    })
  observeEvent(
    input$update, {
      updateSelectInput(session,"select", choices=colnames(filtereddata()), selected = colnames(filtereddata()))
      
    })
  output$contents <- renderDataTable(filtereddata() ) 
  
})
