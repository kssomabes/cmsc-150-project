# Kia Mei S. Somabes
# 2015-07760
# B-3L

# Function for checking if pivoting is needed
# If result is not equal to current i, swap is needed
ToSort <- function(var_length, curr_index, matrix){
  # print(paste(curr_index, "< "))
  max_value = abs(matrix[curr_index, curr_index])
  max_index = curr_index
  
  start = curr_index+1
  if (start <= var_length){
    i = start
    while (i <= var_length){
      # print(paste(i, " ", curr_index))
      # print(paste("i: ", i, " curr: ", curr_index))
      if (abs(matrix[i, curr_index] > max_value)){
        max_index = i
      }
      
      i = i + 1
    }
  }
  
  return(max_index)
}

# Function for swapping rows
SwapRows <- function(old_i, new_i, matrix){
  
  matrix_copy = matrix
  
  matrix[old_i, ] = matrix_copy[new_i, ]
  matrix[new_i, ] = matrix_copy[old_i, ]
  #print("Swapping")
  return(matrix)
}

GaussJordan <- function(matrix){
  
  # The goal of Gauss-Jordan is an identity matrix
  # Steps 
  # Swap if needed
  # Normalize by dividing to the diag element
  # Perform formula to row J below
  # CurrValueInJ - (ValueToBe0 * CorrespNormalizedElement)
  
  # pass the number of columns since it will solve that ncol times
  for (i in 1:nrow(matrix)){
      
      # pass the number of rows
      new_i = ToSort(nrow(matrix), i, matrix)
      
      # Swap
      if (new_i != i){
        matrix = SwapRows(i, new_i, matrix)
      }
      
      # Normalize 
      matrix[i,] = matrix[i,] / matrix[i, i]
      
      # Rows that are not equal to i 
      for (j in 1:nrow(matrix)){
        
        if (i==j) next
        # print(paste("j: ", j, " i: ", i))
        valueToBeZero = matrix[j, i]
        
        subtrahend = matrix[i,] * valueToBeZero 
        
        matrix[j, ] = matrix[j, ] - subtrahend
        
      }
  }
  
  solutionSet = c(1:ncol(matrix)-1)
  solutionSet[] = 0L # https://stackoverflow.com/a/28031687
  # get the index of the RHS 
  RHS = ncol(matrix)
  last_var_ind = RHS-1
  
  # a1 is always 0 
  solutionSet[1] = 0 
  
  # Get the solution set
  
  for (i in 1:last_var_ind){
    sol_ind = i+1
    solutionSet[sol_ind] = matrix[i, RHS]
  }
  

  return(list(matrix=matrix, solutionSet=solutionSet))
}

Gaussian <- function(matrix){
  print(matrix)
  # This loop gets the upper triangular matrix
  
  # i_length = length(variables)-1
  
  for (i in 1:nrow(matrix)){
    
    new_i = ToSort(ncol(matrix), i, matrix)
    if (new_i != i){
      # must be sorted since the largest is not equal to curr i
      matrix = SwapRows(i, new_i, matrix)
    }
    
    # get the diagonal element
    pivot = matrix[i, i]
    
    j_start = i+1
  
    for (j in j_start:ncol(matrix)){

      # get the value to be 0 
      valueToBeZero = matrix[j, i]
      
      # Formula : 
      # currElementInJ - (Multiplier * Pivot)
      
      # the actual multiplier but named before_multiplier for the grouping in above formula
      before_multiplier = valueToBeZero / pivot
      multiplier = before_multiplier * matrix[i,]
      
      # for each element in j traversal
      matrix[j, ] = matrix[j, ] - multiplier
      
    }
  }
  # This loop performs backward substitution
  
  #for i in n - 1 to 1:
  #x[i] = (b[i] - sum(a[i, i+1:n] * x[i+1:n])) / a[i,i]
  
  # declare the vector where the answers will be stored 
  solutionSet = c(1:ncol(matrix))
  solutionSet[] = 0L # https://stackoverflow.com/a/28031687
  # get the index of the RHS 
  RHS = nrow(matrix)
  for (i in ncol(matrix):1){
    
    # get the RHS element
    RHS_element = matrix[i, RHS]
    
    if (i == length(variables)){
      solution = RHS_element / matrix[i,i]
      solutionSet[i] = solution
    }else{
      start2 = i+2
      sumOtherVars = sum(matrix[i, start2:RHS-1] * solutionSet[start2:RHS-1])
      solution = RHS_element - sumOtherVars
      solution = solution / matrix[i, i]
      solutionSet[i] = solution
    }
    
  }
  
  return(list(matrix=matrix, solutionSet=solutionSet))
}
