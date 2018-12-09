# Kia Mei S. Somabes
# 2015-07760
# B-3L

# Exercise (Dynamic)
# -> Coefficient Matrix
# * Gauss Jordan n = 3
# Function w solved unknowns
# Plot data w regression function

PolyReg <<- function(ind_vect, dep_vect, degree){
  
  # check if the ind_vect has same length with dep_vect
  if (length(ind_vect) != length(dep_vect)){
    return(NA)
  }
  
  data_points = data.frame(ind_vect, dep_vect)
  colnames(data_points)[1] = "x"
  colnames(data_points)[2] = "y"
  
  # check if the number of data points are enough for the polynomial degree
  # check if the value of n is less than 1
  # if one of conditions above occur, return NA
  if (degree >= nrow(data_points) || degree < 1){
    return(NULL)
  }
  
  # Matrix for a polynomial regression of degree (n)
  # Get the augmented coefficient matrix given the data points
  
  row_length = degree + 1
  col_length = degree + 2
  
  before_matrix = matrix(nrow=row_length, ncol=col_length, byrow = FALSE)
  before_matrix[] = 0L
  
  constant_ctr = 0 # will serve as the exponent of Xi for constants
  
  for (i in 1:row_length){
    # will serve as row traversal for the matrix
    
    for (j in 1:col_length){
      #will serve as the column traversal
      
      if (j == col_length){
        # formula for constants
        
        x_copy = data_points$x^constant_ctr
        y_copy = data_points$y
        
        before_matrix[i,j] = sum(x_copy * y_copy)
        
        constant_ctr = constant_ctr + 1
      }else{
        # i - 1 and j - 1 since the pattern starts from 0
        exponent = (i - 1) + (j - 1)
        
        # get all the summation of the data set
        x_copy = data_points$x
        x_copy = x_copy ^ exponent
        before_matrix[i, j] = sum(x_copy)
        
      }
      
    }
  }
  # print(before_matrix)
  gaussian = GaussJordan(before_matrix)
  
  # print(gaussian)
  
  reg_eqn = "function (x)"
  degree_dec = degree
  
  # solutionSet[1] is a[0] * X^0
  
  # print(length(gaussian$solutionSet))
  
  # changed the last index to 2 since the GaussJordan has been padded with the first element
  # as A1 but the algorithm remained for that function
  for (i in length(gaussian$solutionSet):2){
    
    reg_eqn = paste(reg_eqn, gaussian$solutionSet[i])
    
    if (i != 2){
      reg_eqn = paste(reg_eqn, "* (x^", degree_dec, ")")
      reg_eqn = paste(reg_eqn, "+")
    }
    
    degree_dec = degree_dec - 1
  }
  
  reg_eqn_eval = eval(parse(text = reg_eqn))
  
  values  = reg_eqn_eval(data_points$x)
  # print(reg_eqn_eval)

  plot(data_points$x, data_points$y, pch = 20, col = "red", main = "X vs. Y",
       xlab = "X", ylab = "Y")
  lines(data_points$x, values, col = "green")
}

# temp = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
# yield = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)
# sample = data.frame(temp, yield)
# 
# regression = PolynomialRegression(sample$temp, sample$yield, 4)