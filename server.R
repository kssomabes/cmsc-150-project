# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
# Reference: http://shiny.rstudio.com/gallery/upload-file.html
library(shiny)
library(rhandsontable)

options(shiny.maxRequestSize = 9*1024^2)

source("Gauss.R")
source("QuadSpline.R")
source("PolyReg.R")
source("Simplex.R")

# Initial data for demands and supply for Simplex method

demandConst <<- matrix(c(180, 80, 200, 160, 220), 1, 5, byrow = TRUE, dimnames = list(c("Demands"), c("S", "SL", "A", "C", "NY")))
supplyConst <<- matrix(c(310, 10, 8, 6, 5, 4, 260, 6, 5, 4, 3, 6, 280, 3, 4, 5, 5, 9), 3,6, byrow = TRUE, dimnames = list(c("Denver", "Phoenix", "Dallas"), c("Supply", "S", "SL", "A", "C", "NY")))

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
    
    for (i in 1:nrow(output)){
      minX = values[i,1]
      maxX = values[i+1,1]
      output[i,1] = paste(minX, "x", "<=", "x", maxX) 
      output[i,2] = QSIRes$equations[[i]]
    }
    
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
    
    if (input$checkboxPR) return(output)
    else return (NULL)
  })
  
  # POLYNOMIAL REGRESSION OUTPUTS
  
  output$PRTable <- renderTable({
    req(input$datafile2)
    values = read.table(input$datafile2$datapath, header = TRUE, sep =",")
    return(values)
  })
  
  output$PRValue <- renderTable({
    req(input$datafile2)
    req(input$degree)
    req(input$toEstimate2)
    
    if (is.null(input$datafile2) && is.null(input$toEstimate2)) return (NULL)
    
    toEstimate2 = input$toEstimate2
    degree = input$degree 
    
    values = read.table(input$datafile2$datapath, header = TRUE, sep = ",")
    PRRes <- PolyReg(values[,1], values[,2], degree, toEstimate2)
    
    if (is.null(PRRes) == TRUE){
      # Some error handling
      return(NULL)
    }
    
    output = matrix(nrow=1, ncol=2, byrow = TRUE, dimnames = list(c(), c("x", "f(x)")))
    output[1,1] = toEstimate2
    output[1,2] = PRRes$value
    
    if (input$checkboxPR) return(output)
    else return (NULL)
  })
  
  output$PREquation <- renderText({
    req(input$datafile2)
    req(input$degree)
    req(input$toEstimate2)
    
    toEstimate2 = input$toEstimate2
    degree = input$degree
    if (is.null(input$datafile2) && is.null(degree)) return (NULL)

    values = read.table(input$datafile2$datapath, header = TRUE, sep = ",")
    result <- PolyReg(values[,1], values[,2], degree, toEstimate2)
    if (input$checkboxPR) return(result$equation)
    else return(NULL)
  })
  
  # SIMPLEX OUTPUTS
  output$simplexInputDemand <- renderRHandsontable({
    rhandsontable(demandConst, width = 1000)
  })
  
  output$simplexInputSupply <- renderRHandsontable({
    rhandsontable(supplyConst, width = 1000)
  })
  
  actionSimplex <- eventReactive(input$actionSimplex, {
    demandMatrix <- as.matrix(hot_to_r(input$simplexInputDemand))
    supplyMatrix <- as.matrix(hot_to_r(input$simplexInputSupply))
    
    initialTab <- Simplex(supplyMatrix[1:3, 2:6], demandMatrix, supplyMatrix[1:3, 1])
    return(initialTab$final_matrix)
  })
  
  output$showFinalTableau <- renderRHandsontable({
    rhandsontable(actionSimplex())
  })
}