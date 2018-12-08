
~source("Gauss.R")

QuadSpline <- function(ind_vect, dep_vect){
  # n + 1 data points; n interval
  # 3 unknowns for every interval - 3n unknowns in the system 
  # 3n unknowns = generate 3n equations 
  
  if (length(ind_vect) != length(dep_vect)){
    return (NA)
  }
  
  # n is the intervals (number of data points - 1) 
  dp_n = length(ind_vect)
  n = length(ind_vect)-1
  
  equations = list()
  
  equation_ctr = 1
  
  # -1 since the last equation is removed
  
  before_matrix = matrix(nrow=3*n-1, ncol=3*n, byrow = FALSE)
  before_matrix[] = 0L
  
  print(paste("Intervals: ", n))
  
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
        before_matrix[i-2, a:b] = c(ind_vect[i-1], 1)
        before_matrix[i-2, ncol(before_matrix)] = dep_vect[i-1]
      }else {
        before_matrix[i-2, a:b+1] = c(ind_vect[i-1]^2, ind_vect[i-1], 1)
        before_matrix[i-2, ncol(before_matrix)] = dep_vect[i-1]
      }
      equation_ctr = equation_ctr + 1
      
      c = i+2+1
      before_matrix[i-1, i:c] = c(ind_vect[i-1]^2, ind_vect[i-1], 1, dep_vect[i-1])
      equation_ctr = equation_ctr + 1
      
    
    # Condition 3 
      d = i+2
      e = i-1 
      if (a == 1){
        # skip the first column
        before_matrix[i, a:d] = c(1, 0, -2*ind_vect[i-1], -1, 0)
      }else {
        d = d + 1
        before_matrix[i, a:d] = c(2*ind_vect[i-1], 1, 0, -2*ind_vect[i-1], -1, 0) 
      }
      
      equation_ctr = equation_ctr + 1
      
      
      
  }
  
  # Condition 2 - First and last functions must pass through the end points
  before_matrix[equation_ctr, 1:2] = c(ind_vect[1], 1)
  before_matrix[equation_ctr, ncol(before_matrix)] = dep_vect[1]
  equation_ctr = equation_ctr + 1
  
  start_c3 = ncol(before_matrix) - 3
  before_matrix[equation_ctr, start_c3:ncol(before_matrix)] = c(ind_vect[dp_n]^2, ind_vect[dp_n], 1, dep_vect[dp_n])

  print(before_matrix)
  equation_ctr = equation_ctr + 1

  result = GaussJordan(before_matrix) 
  
  return (result)
}

x = c(1.6, 2, 2.5)
y = c(2, 8, 14)
given = data.frame(x, y)
equations = QuadSpline(given$x, given$y)
