library(shiny)
library(ggplot2)
library(shinydashboard)


inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

shinyUI(fluidPage(
  
  dashboardPage(
    #HEADER
    dashboardHeader(title = "Lipidomics Application"),
    #SLIDER
    dashboardSidebar(
      sidebarMenu(
        
        id = "tabs",
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
        menuItem("Export", tabName = "export", icon = icon("export", lib = "glyphicon"))
      )
    ),
    #BODY
    dashboardBody(
      tabItems(
        tabItem(  tabName = "data",    
                  fluidRow( 
                    box(width = 2, title = "Upload Data",
                        fileInput('file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')),
                        
                        shinyjs::useShinyjs(),
                        div(
                          id = "side-panel",
                          radioButtons("feat", "Select Feature", c("Species","Head Group", "Length","Double Bonds"), selected = "Species"),
                          hr(),
                          conditionalPanel(
                            condition = "input.feat == 'Species'", selectInput("species", "Which Species: ", c(),  multiple = TRUE)),
                          conditionalPanel(
                            condition = "input.feat == 'Head Group'", selectInput("feat_gh", "Which Head Groups: ", c(),  multiple = TRUE)),
                          conditionalPanel(
                            condition = "input.feat == 'Length'", selectInput("feat_len", "Which Length: ", c(), multiple = TRUE)),
                          conditionalPanel(
                            condition = "input.feat == 'Double Bonds'", selectInput("feat_db", "Which Double Bonds: ",c(), multiple = TRUE))
                        ),
                        checkboxGroupInput("exp_con", "Choose experimental conditions:",
                                           c(#All = 'all',
                                             GrowthStage ='gs',
                                             Temparature = 'temp',
                                             Salt = 'salt',
                                             Methanol = 'met',
                                             Triton = "tri")),
                        helpText(em("Note: Unselect if you want to delete an experimental condition!")),
                        selectInput("select", "Select columns to display.",c(), multiple = TRUE),
                        helpText(em("Note: Use delete button to de-select columns. Make sure you leave class, length and DB column!")),
                        actionButton("update", "Update Data Set", class = "btn-primary",style='padding:4px; font-size:120%')
                    ),
                    box(width = 10, title = "Data", status = "primary",  div(style = 'overflow-x: scroll',  dataTableOutput("contents")), downloadButton("downloadData", "Download")))
        ),
        
        tabItem( tabName = "plot",
                 
                 fluidRow(
                   
                   box(width = 2, radioButtons("plot_type", "Select Plot Type", c("Lipid Abundance","PCA", "Box Plot","Standard Deviation", "Line Plot"), selected = "Lipid Abundance"),
                       hr(),
                       #conditionalPanel(
                        # condition = "input.plot_type == 'Standard Deviation'", numericInput("obs", "Set y axes:", 7)),
                       conditionalPanel(
                         condition = "input.plot_type == 'Line Plot'", checkboxInput("hg", "Seperate by Head Group Class", FALSE)),
                       textInput('xlab', 'X axis label', value = "Abundance [mol %]"),
                       textInput('ylab', 'Y axis label', value = "Lipid Species"),
                       textInput('plotTitle', 'Plot title', value = ""),
                       textInput('Legend', 'Legend', value = "Lipid Abundance"),
                       selectInput('legendposition', label ='Legend Position',
                                   choices=c("left", "right", "bottom", "top"),
                                   multiple=FALSE, selectize=TRUE,selected="bottom"),
                       actionButton("update_plot", "Plot", class = "btn-primary",style='padding:4px; font-size:120%')),
                   box(
                     #h1("Experimental conditions:", textOutput("selected_var")),
                     textOutput("selected_var"),
                     width = 10, title = "Plot", status = "primary", column(
                     12,
                     plotOutput('plot'),
                     
                     div(
                       id = "save_plot_area",
                       inline_ui(
                         textInput("save_plot_name", NULL, "",
                                   placeholder = "Enter plot name to save")
                       ),
                       actionButton("save_plot_btn", "Save plot", icon = icon("star")),
                       shinyjs::hidden(
                         span(
                           id = "save_plot_checkmark",
                           icon("check")
                         )
                       )
                     )
                   )) #end box
                   
                 )
                 ), #end tabItem
        tabItem( tabName = "export",
                 fluidRow(      conditionalPanel(
                   condition = "!output.saved_plots_exist",
                   h2("You do not have any saved plots to export")
                 ),
                 conditionalPanel(
                   condition = "output.saved_plots_exist",
                   fluidRow(
                     column(
                       4,
                       h2("Export Options"),
                       div(
                         id = "exporting_plots_options",
                         selectInput("export_file_type", "File type",
                                     c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png", "EPS" = "eps")),
                         conditionalPanel(
                           condition = "input.export_file_type == 'pdf'",
                           selectInput("export_pdf_orientation", "Page orientation",
                                       c("Portrait (8.5\" x 11\")" = "portrait",
                                         "Landscape (11\" x 8.5\")" = "landscape",
                                         "Custom dimensions" = "custom")
                           ),
                           conditionalPanel(
                             condition = "input.export_pdf_orientation == 'custom'",
                             numericInput("export_pdf_width", "Page width (inches)",
                                          value = 8.5, min = 1, max = 50, step = 0.5),
                             numericInput("export_pdf_height", "Page height (inches)",
                                          value = 11, min = 1, max = 50, step = 0.5)
                           )
                         ),
                         conditionalPanel(
                           condition = "input.export_file_type != 'pdf'",
                           numericInput("export_file_width", "Image width (pixels)",
                                        value = 480, min = 100, max = 2000),
                           numericInput("export_file_height", "Image height (pixels)",
                                        value = 480, min = 100, max = 2000)
                         ),
                         checkboxInput("export_multiple", "Multiple plots per page"),
                         conditionalPanel(
                           condition = "input.export_multiple",
                           selectInput("export_arrangement", NULL,
                                       c("Arrange plots by row" = "byrow",
                                         "Arrange plots by column" = "bycol")),
                           numericInput("export_nrow", "Rows per page",
                                        value = 1, min = 1, max = 20),
                           numericInput("export_ncol", "Columns per page",
                                        value = 1, min = 1, max = 20)
                           
                         ),
                         uiOutput("export_btn_ui")
                       )
                     ),
                     column(
                       8,
                       h2("Preview"),
                       strong("Remove plot"), br(),
                       inline_ui(uiOutput("plots_remove_ui")),
                       actionButton("remove_plot_btn", "Remove"),
                       uiOutput("plots_order_ui"),
                       div(
                         id = "preview_plots_options",
                         uiOutput("plots_select_page_ui"),
                         plotOutput("plot_preview", height = "auto")
                       )
                     )
                   )
                 )))
      )
    )
  )
))
