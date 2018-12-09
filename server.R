# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
# Reference: http://shiny.rstudio.com/gallery/upload-file.html
library(shiny)
library(rhandsontable)

options(shiny.maxRequestSize = 9*1024^2)

source("Gauss.R")
source("QuadSpline.R")
source("PolyReg.R")

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
    
    if (input$checkboxQSI) return(output)
    else return (NULL)
  })
  
  # POLYNOMIAL REGRESSION OUTPUTS
  
  output$PRTable <- renderTable({
    req(input$datafile2)
    values = read.table(input$datafile2$datapath, header = TRUE, sep =",")
    return(values)
  })
  
  output$PRGraph <- renderPlot({
    req(input$datafile2)
    req(input$degree)

    degree = input$degree
    if (is.null(input$datafile2) && is.null(degree)) return (NULL)
    
    
    values = read.table(input$datafile2$datapath, header = TRUE, sep = ",")
    result <- PolyReg(values[,1], values[,2], degree)
    if (input$checkboxPR) return(result)
    else return(NULL)
  })
  
  output$simplexInputDemand <- renderRHandsontable({
    rhandsontable(demandConst, width = 1000)
  })
  
  output$simplexInputSupply <- renderRHandsontable({
    rhandsontable(supplyConst, width = 1000)
  })
  
  actionSimplex <- eventReactive(input$actionSimplex, {
    demandMatrix <- as.matrix(hot_to_r(input$simplexInputDemand))
    supplyMatrix <- as.matrix(hot_to_r(input$simplexInputSupply))
    
    print(demandMatrix)
    # print(typeof(demandMatrix))
    # print(demandMatrix[,1])
    # print(demandMatrix[1,])
    print(supplyMatrix)
    
    # Objective Function (supplyMatrix[1:3, 2:6])
    # Number to ship & demand constraints (demandMatrix)
    # Number to ship & supply constraints (supplyMatrix[1:3, 1])
  
  })
  
  output$showInitialTableau <- renderRHandsontable({
    rhandsontable(actionSimplex())
  })
}