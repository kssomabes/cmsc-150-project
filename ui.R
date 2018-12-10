# http://shiny.rstudio.com/gallery/upload-file.html
library(rhandsontable)

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
                         br(),
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
                         numericInput("toEstimate2", 
                                      "Evaluate at:", 
                                      value = 1),
                         checkboxInput("checkboxPR", "Show results", FALSE)
                       ),
                       mainPanel(
                         br(),
                         tableOutput("PRTable"),
                         verbatimTextOutput("PREquation"),
                         tableOutput("PRValue")
                       )
              ),
              
              # TAB PANEL FOR SIMPLEX
              tabPanel("Simplex Implementation",
                       sidebarPanel(
                         tags$hr(),
                         checkboxInput("showInitialTab", "Show initial tableau", FALSE),
                         actionButton("actionSimplex", "Solve")
                       ),
                       mainPanel(
                          br(),
                          rHandsontableOutput("simplexInputDemand"),
                          br(),
                          rHandsontableOutput("simplexInputSupply"),
                          br(),
                          rHandsontableOutput("showFinalTableau")
                       )
              )
  )
)