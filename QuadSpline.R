
source("Gauss.R")

CheckInterval <- function(min, max, toCheck){
  # this function checks whether toCheck is in the range of min and max
  
  if (min < toCheck && max > toCheck){
    return (TRUE)
  }
  
  return (FALSE)
}

QuadSpline <<- function(x, y, givenX){
  
  given = data.frame(x=x, y=y)
  
  # sort by x 
  given = given[order(given$x),]
  
  ind_vect = given$x
  dep_vect = given$y
  
  # n + 1 data points; n interval
  # 3 unknowns for every interval - 3n unknowns in the system 
  # 3n unknowns = generate 3n equations 
  
  if (length(ind_vect) != length(dep_vect)){
    return (NULL)
  }
  
  max_rng_ind = length(ind_vect)-1
  
  imax_ind = 0
  imin_ind = 0
  
  for (i in 1:max_rng_ind){
    inRange = CheckInterval(ind_vect[i], ind_vect[i+1], givenX)
    
    if (inRange == TRUE){
      imin_ind = i
      imax_ind = i+1
      break
    }
  }
  
  if (imax_ind == 0 && imin_ind == 0) return(NA)
  
  # n is the intervals (number of data points - 1) 
  dp_n = length(ind_vect)
  n = length(ind_vect)-1
  
  equations = list()
  
  equation_ctr = 1
  
  # -1 since the last equation is removed
  
  before_matrix = matrix(nrow=3*n-1, ncol=3*n, byrow = FALSE)
  before_matrix[] = 0L
  
  # print(paste("Intervals: ", n))
  
  # padding in n because of 1-based indexing
  padded_len = n+1
  col_in = 1
  for (i in 3:padded_len){
    # the variable subscripts will be -2 and -1 due to the padding
    # first column will always be 0 
    # Condition 1 - interior knots
      a = i-2 
      b = i-1
      
      if (a == 1){
        # only occupy the first two columns since a1 = 0 / omitted in the table
        col_in_plus = col_in+1
        before_matrix[equation_ctr, col_in:col_in_plus] = c(ind_vect[i-1], 1)
        before_matrix[equation_ctr, ncol(before_matrix)] = dep_vect[i-1]
        col_in = col_in + 2
      }else {
        b = b+1
        col_in_plus = col_in+2
        before_matrix[equation_ctr, col_in:col_in_plus] = c(ind_vect[i-1]^2, ind_vect[i-1], 1)
        before_matrix[equation_ctr, ncol(before_matrix)] = dep_vect[i-1]
        col_in = col_in + 3
      }
      
      equation_ctr = equation_ctr + 1
      
      col_in_plus = col_in + 2 
      before_matrix[equation_ctr, col_in:col_in_plus] = c(ind_vect[i-1]^2, ind_vect[i-1], 1)
      before_matrix[equation_ctr, ncol(before_matrix)] = dep_vect[i-1]
  
      equation_ctr = equation_ctr + 1

  }
  
  # Condition 2 - First and last functions must pass through the end points
  before_matrix[equation_ctr, 1:2] = c(ind_vect[1], 1)
  before_matrix[equation_ctr, ncol(before_matrix)] = dep_vect[1]
  equation_ctr = equation_ctr + 1
  
  start_c3 = ncol(before_matrix) - 3
  before_matrix[equation_ctr, start_c3:ncol(before_matrix)] = c(ind_vect[dp_n]^2, ind_vect[dp_n], 1, dep_vect[dp_n])

  equation_ctr = equation_ctr + 1
  
  col_ind = 1
  for (i in 3:padded_len){
    # Condition 3 
    
    a = i-2 
    d = i+1
    e = i-1 
  
    if (a == 1){
      # skip the first column
      col_ind_plus = col_ind + 3
      before_matrix[equation_ctr, col_ind:col_ind_plus] = c(1, 0, -2*ind_vect[i-1], -1)
      before_matrix[equation_ctr, ncol(before_matrix)] = 0
      col_ind = col_ind + 2
    }else {
      d = d + 1
      col_ind_plus= col_ind + 4
      before_matrix[equation_ctr, col_ind:col_ind_plus] = c(2*ind_vect[i-1], 1, 0, -2*ind_vect[i-1], -1) 
      before_matrix[equation_ctr, ncol(before_matrix)] = 0       
      col_ind = col_ind + 3
    }
    
    equation_ctr = equation_ctr + 1
  }

  # print(before_matrix)
  
  result = GaussJordan(before_matrix) 
  # result$solutionSet holds a1, b1, c1 and so on
  
  ss_ind = 1
  for (i in 1:n){
    equations[i] = paste("function (x) ", result$solutionSet[ss_ind], "*x^2 +", result$solutionSet[ss_ind+1], "*x +", result$solutionSet[ss_ind+2])
    ss_ind = ss_ind + 3
  }
  
  # get the imin_ind th equation 
  
  eval_eqn = eval(parse(text = equations[imin_ind]))
  value  = eval_eqn(givenX)
  # print(equations)
  return (list(equations = equations, finalMatrix = result$matrix, solutionSet = result$solutionSet, value = value))
}
# 
x = c(3, 4.5, 7, 9)
y = c(2.5, 1, 2.5, 0.5)
given = data.frame(x=x, y=y)

given = given[order(given$x),]

givenX = 3.2
QSI = QuadSpline(given$x, given$y, givenX)
