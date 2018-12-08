# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
# Reference: http://shiny.rstudio.com/gallery/upload-file.html
options(shiny.maxRequestSize = 9*1024^2)

source("Gauss.R")
source("QuadSpline.R")
source("PolyReg.R")

function(input, output) {

  # QSI OUTPUTS 
  
  output$QSITable <- renderTable({
    req(input$datafile)
    values = read.table(input$datafile$datapath, header = TRUE, sep =",")
    values
  })
  
  output$QSIEquations  <- renderTable({
    req(input$datafile)
    req(input$toEstimate)
    
    if (is.null(input$datafile) && is.null(input$toEstimate)) return (NULL)
    values = read.table(input$datafile$datapath, header = TRUE, sep = ",")
    
    
    QSIRes <- QuadSpline(values[,1], values[,2], input$toEstimate)
    
    if (is.null(QSIRes) == TRUE){
      # Some error handling
      return(NULL)
    }
    
    output = matrix(nrow = length(QSIRes$equations), ncol = 2, dimnames=list(c(), c("Interval", "Function")))
    
    if (input$checkboxQSI) return(output)
    else return(NULL)
  })
  
  output$QSIValue <- renderTable({
    req(input$datafile)
    req(input$toEstimate)
    
    if (is.null(input$datafile) && is.null(input$toEstimate)) return (NULL)
    
    toEstimate = input$toEstimate
    
    values = read.table(input$datafile$datapath, header = TRUE, sep = ",")
    QSIRes <- QuadSpline(values[,1], values[,2], toEstimate)
    
    if (is.null(QSIRes) == TRUE){
      # Some error handling
      return(NULL)
    }
    
    output = matrix(nrow=1, ncol=2, byrow = TRUE, dimnames = list(c(), c("x", "f(x)")))
    output[1,1] = toEstimate
    output[1,2] = QSIRes$value
    
    if (input$checkboxQSI) return(output)
    else return (NULL)
  })
  
  # POLYNOMIAL REGRESSION OUTPUTS
  
  output$PRTable <- renderTable({
    req(input$datafile2)
    values = read.table(input$datafile2$datapath, header = TRUE, sep =",")
    values
  })
  
  output$PREquation <- renderText({
    req(input$datafile2)
    req(input$degree)
    
    if (is.null(input$datafile2) && is.null(input$toEstimate)) return (NULL)
    
    degree = input$degree
    values = read.table(input$datafile2$datapath, header = TRUE, sep = ",")
    result <- PolyReg(values[,1], values[,2], degree)
    
    
    print(result)
  })
}