# Kia Mei S. Somabes
# 2015-07760
# B-3L
# Modified params of deparse to catch more variables 

# Given these equations, return the augmented coefficient matrix
# Bonus cases: 0 and 1 coefficients and incomplete function parameters but all have same vars
E1 <- function (x0, x1, x2) 0.3 * x0 + -0.2 * x1 + 10 * x2 + -71.4
E2 <- function (x0, x1, x2) 3 * x0 + x2 + -0.1 * x1 + -7.85
E3 <- function (x0, x1, x2) 0.1 * x0 + 7 * x1 + -0.3 * x2 + 19.3

AugCoeffMatrix <- function(system){
  
  variables = c()
  var_ctr = 1
  
  equation_ctr = length(system) # used for the row names later on
  
  for (fxn in system){
    # deparse
    # fxn[1] will return " function ( ) " or the variables
    fxn = deparse(fxn)
    var = fxn[1]
    
    
    var_to_split = gsub("^function \\(", "", var)
    var_to_split = gsub("\\) $", "", var_to_split)
    var_split = strsplit(var_to_split, ", ")
    
    # insert these variables to the list
    for (i in var_split){
      variables[[var_ctr]] = i
      var_ctr = var_ctr + 1
    }
    
  }
  
  variables = variables[[1]]
  variables = unique(variables) # remove duplicates in the variables
  variables = sort(variables)   # sort them so that x0, x1, .. xN 
  
  column_total = length(variables)+1 # +1 is for the RHS
  # compute for the matrix dimensions
  # col = column_total
  # row = equation_ctr
  # instantiate matrix with all 0s
  
  new_matrix = matrix(nrow=equation_ctr, ncol= column_total, byrow=FALSE)
  new_matrix[] = 0L # https://stackoverflow.com/a/28031687
  
  row_num = 1
  
  for (fxn in system){
    # deparse
    # fxn[2] will return " Ax - B "
    
    fxn = deparse(fxn, width.cutoff = 500L)
    eqn = fxn[2]
    
    # terms are separated with '+' 
    eqn_terms = strsplit(eqn, " \\+ ")
    
    for (term in eqn_terms[[1]]){
      rhs_var = 1 # if this is greater than the size of variable (used for traversing the variables list only), rhs not coefficient
      
      # grep each variable to the current term; no match would mean that it is RHS
      for (var in variables){
        rhs_var = rhs_var + 1
        if (grepl(var, term)){
          # get column number by checking index of var in variables list
          col_num = match(var, variables)
          
          if (grepl("\\*", term)==FALSE){
            new_matrix[row_num, col_num] = 1
          }else{
            coeff = gsub(var, "", term)
            coeff = gsub(" \\* ", "", coeff)
            coeff = as.double(coeff)
            new_matrix[row_num, col_num] = coeff
          }
          
          
          break
        }
        
        if (rhs_var == column_total){
          term = as.numeric(term) * -1
          new_matrix[row_num, column_total] = term
          break
        }
      }
    }
    row_num = row_num + 1 # increment, next equation
  }
  
  # change the dimension names
  
  col_names = variables
  col_names[column_total] = "RHS"
  row_names = list(1:equation_ctr)
  
  dimnames(new_matrix) = list(row_names[[1]], col_names)
  
  # return list with variables and augcoeffmatrix
  return(list(variables=variables, augcoeffmatrix=new_matrix))
}

system = list(E1, E2, E3)
result = AugCoeffMatrix(system)
result$variables
result$augcoeffmatrix
