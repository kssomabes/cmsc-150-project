# http://shiny.rstudio.com/gallery/upload-file.html

fluidPage(
  titlePanel("CMSC 150 Project"),
  tabsetPanel(type = "tabs",
              
              # TAB PANEL FOR QSI
              tabPanel("Quadractic Spline Interpolation", 
                       
                       # INPUT (QSI)
                       sidebarPanel(
                         tags$hr(),
                         fileInput('datafile', 'Choose file to upload',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv'
                                   )
                         ),
                         tags$hr(),
                         numericInput("toEstimate", 
                                      "Evaluate at:", 
                                      value = 1),
                         checkboxInput("checkboxQSI", "Show results", FALSE)
                       ),
                       
                       # OUTPUT (QSI)
                       mainPanel(
                         tableOutput("QSITable"),
                         tableOutput("QSIValue"),
                         tableOutput("QSIEquations")
                       )
                      
              ),
              
              # TAB PANEL FOR POLYNOMIAL REGRESSION
              tabPanel("Polynomial Regression",
                       # INPUT (PR)
                       sidebarPanel(
                         tags$hr(),
                         fileInput('datafile2', 'Choose file to upload',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv'
                                   )
                         ),
                         tags$hr(),
                         numericInput("degree", 
                                      "Enter degree:", 
                                      value = 1),
                         checkboxInput("checkboxPR", "Show results", FALSE)
                       ),
                       mainPanel(
                         tableOutput("PRTable"),
                         plotOutput("PRGraph")
                       )
              ),
              
              # TAB PANEL FOR SIMPLEX
              tabPanel("Simplex Implementation",
                       sidebarPanel(
                         tags$hr(),
                         numericInput("numVar",
                                      "Number of variables:",
                                      value = 1),
                         numericInput("numConst",
                                      "Number of constraints:",
                                      value = 1),
                         checkboxInput("checkboxSimplex", "Show results", FALSE)
                       ),
                       mainPanel(
                         
                       )
              )
  )
)