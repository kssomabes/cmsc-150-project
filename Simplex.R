
GetSmallestPositiveIndex <- function(test_ratio){
  
  init_small_p = test_ratio[which(test_ratio>0,arr.ind = TRUE)[1]] # get the smallest positive test_ratio to set as initial 
  smallest_ind = 1
  
  for (i in 1:length(test_ratio)){
    if (test_ratio[[i]] < init_small_p && test_ratio[[i]] > 0 && !is.na(test_ratio) && !is.null(test_ratio) && !is.nan(test_ratio[[i]]) && !is.infinite(test_ratio[[i]])){
      init_small_p = test_ratio[[i]]
      smallest_ind = i
    } 
  }
  return (smallest_ind)
}

Simplex <<- function(objMat, demMat, supMat){
  
  print("In simplex method")
  print(objMat)
  # print(demMat) # number to ship and supply demand constraints
  # print(supMat)
  # Objective Function (supplyMatrix[1:3, 2:6])
  # Number to ship & demand constraints (demandMatrix)
  # Number to ship & supply constraints (supplyMatrix[1:3, 1])
  
  init_table = matrix(0, nrow = 9, ncol = 25)
  init_table_iter = 1
  
  # init_table_iter + 15 will also serve as the index for the slack variable
  
  demC = matrix(0, length(demMat), ncol(init_table))
  
  # demand constraints have >= inequality sign, multiply by -1 except for slack variable
  for (i in 1:length(demMat)){
    
    # demC[[i]] = list(rep(0, ncol(init_table))) # initialize list of 0s
    demC[i, ncol(init_table)] = demMat[i] # assign RHS
    
    demC[i,i] = 1
    demC[i,(i+5)] = 1
    demC[i, (i+10)] = 1
    
    demC[i, ] = demC[i, ] * -1 # multiply the whole row by - `1
    
    demC[i, (init_table_iter + 15)] = 1 # add slack variable
    
    init_table_iter = init_table_iter + 1  # increment slack variable iterator
  }
  
  #  assign demC to initial tableau
  init_table[1:5, 1:ncol(init_table)] = demC
  
  supC = matrix(0, length(supMat), ncol(init_table))
  # supply constraints have <= inequality sign, apply normal simplex method
  
  j_ctr = 0
  for (i in 1:length(supMat)){
    
    supC[i, (j_ctr+1):(j_ctr+5)] = 1
    supC[i, ncol(init_table)] = supMat[i] # assign RHS
    supC[i, (init_table_iter + 15)] = 1 # add slack variable
    
    init_table_iter = init_table_iter + 1 # increment slack variable iterator
    j_ctr = j_ctr + 5
  }
  
  # assign supC to initial tableau
  init_table[6:8, 1:ncol(init_table)] = supC 
  
  # Get the objective function
  obj_init = matrix(0, 1, ncol(init_table))
  j_ctr = 0
  for (i in 1:nrow(objMat)){
    obj_init[1, (j_ctr+1):(j_ctr+5)] = objMat[i, 1:ncol(objMat)]
    j_ctr = j_ctr + 5
  }
  obj_init[1, (ncol(init_table)-1)] = 1 # assign Z
  
  # assign obj_init to initial tableau
  init_table[nrow(init_table), ] = obj_init
  
  print("Hi")
  print(init_table)
  
  # negate everything 
  obj = c(10, 8, 6, 5, 4, 
          6, 5, 4, 3, 6,
          3, 4, 5, 5, 9
  )
  
  # Number to ship and Demand Constraints
  
  # if ( >= ), multiply by -1
  # else as is
  
  whA = c(-1, 0, 0, 0, 0,
          -1, 0, 0, 0, 0,
          -1, 0, 0, 0, 0
  )
  whA_R = -180
  
  whB = c(0, -1, 0, 0, 0,
          0, -1, 0, 0, 0,
          0, -1, 0, 0, 0
  )
  whB_R = -80
  
  whC = c(0, 0, -1,0, 0,
          0, 0, -1,0, 0,
          0, 0, -1,0, 0
  )
  whC_R = -200
  
  whD = c(0, 0, 0, -1, 0,
          0, 0, 0, -1, 0,
          0, 0, 0, -1, 0
  )
  whD_R = -160
  
  whE = c(0, 0, 0, 0, -1,
          0, 0, 0, 0, -1,
          0, 0, 0, 0, -1
  )
  whE_R = -220
  
  # Supply & Number to ship constraints
  sA = c(1, 1, 1, 1, 1,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)
  sA_R = 310
  
  sB = c(0, 0, 0, 0, 0,
         1, 1, 1, 1, 1,
         0, 0, 0, 0, 0)
  sB_R = 260
  
  sC = c(0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         1, 1, 1, 1, 1)
  sC_R = 280
  
  # Set up slack variables, later on consider their inequality signs
  
  # if (sign == "GT")
  # else if (sign == "LT")
  
  # the dimensions of the matrix:
  # number of rows = number of constraints + objective function
  # number of cols = number of variables + number of rows 
  #  (constraints (slack variables needed is same as # of constraints) + objective function (Z)) 
  #   + 1 (RHS or solution) 
  
  number_col = 15 + 9 + 1 
  initial_tableau = matrix(nrow = 9, ncol = number_col)
  initial_tableau[] = 0L
  
  # negative slack variables
  initial_tableau[1,1:15] = whA
  initial_tableau[1, ncol(initial_tableau)] = whA_R
  
  initial_tableau[2,1:15] = whB
  initial_tableau[2, ncol(initial_tableau)] = whB_R
  
  initial_tableau[3,1:15] = whC
  initial_tableau[3, ncol(initial_tableau)] = whC_R
  
  initial_tableau[4,1:15] = whD
  initial_tableau[4, ncol(initial_tableau)] = whD_R
  
  initial_tableau[5,1:15] = whE
  initial_tableau[5, ncol(initial_tableau)] = whE_R
  
  # positive slack variables
  initial_tableau[6,1:15] = sA
  initial_tableau[6, ncol(initial_tableau)] = sA_R
  
  initial_tableau[7,1:15] = sB
  initial_tableau[7, ncol(initial_tableau)] = sB_R
  
  initial_tableau[8,1:15] = sC
  initial_tableau[8, ncol(initial_tableau)] = sC_R
  
  initial_tableau[9,1:15] = obj
  initial_tableau[9, ncol(initial_tableau)-1] = 1
  
  # add slack variables 
  
  for (i in 1:nrow(initial_tableau)-1){
    # TODO replace the hardcoded number with the number of variables
    initial_tableau[i, (i+15)] = 1
  }
  
  print("hardcoded")
  print(initial_tableau)
  # return (initial_tableau)
  # modifiedGaussJordan(initial_tableau)
  
  matrix2 = initial_tableau
  
  # TODO change 1:8
  # checks if there is a negative in the RHS
  ph1cond = any(matrix2[1:8, ncol(matrix2)] < 0)
  iter = 1 
  
  while (ph1cond){
    # Stop if all RHS are positive
    
    # TODO change 1:8
    min_index = which.min(matrix2[1:8,ncol(matrix2)]) # returns the row index of the smallest negative
    
    # TODO change 1:8
    # Get the smallest negative in RHS
    minimum = min(matrix2[1:8,ncol(matrix2)])
    
    if (minimum > 0) break
    
    # Check if there is a negative in the min_index's row of variables
    hasNegative = which(matrix2[min_index, 1:15] < 0)
    if (length(hasNegative) > 0){
      
      # Get the test ratio of the negative values 
      # TODO edit 1:15
      neg_indeces = which(matrix2[min_index, 1:15] < 0)
      
      # stores the test ratio
      tr_neg = c()
      
      largest_TR_index = 0 # returns the column index
      largest_TR = 0
      
      RHS = ncol(matrix2)
      curr_RHS = matrix2[min_index, RHS]
      
      for (i in 1:length(neg_indeces)){
        
        TR = matrix2[min_index ,neg_indeces[i]] / curr_RHS
        tr_neg[i] = TR
        if (TR > largest_TR){
          largest_TR = TR
          largest_TR_index = neg_indeces[[i]]
        }
      }
      
      matrix2 = modifiedGaussJordan(matrix2, min_index, largest_TR_index)
    }else break
    ph1cond = any(matrix2[1:8, ncol(matrix2)] < 0)
  }
  
  # print(matrix2)
  
  # return TRUE if the last row still has negative values in it from 1:length(variables)
  # -2 because of solution and Z
  ph2cond = any(matrix2[nrow(matrix2), 1:(ncol(matrix2)-2)] < 0)
  while (ph2cond){
    
    # TODO change 1:15
    
    # min_index_last = pivot_col 
    min_index_last = which.min(matrix2[nrow(matrix2), 1:(ncol(matrix2)-2)])
    min_last = min(matrix2[nrow(matrix2), 1:(ncol(matrix2)-2)])
    
    ph2_tr = c()
    # smallest_pos_TR_ind = 0
    
    # Look for the pivot row by getting the test ratio 
    for (i in 1:(nrow(matrix2)-1)){
      # print(paste("i: ", i, "num: ", matrix2[i, min_index_last], "RHS: ", matrix2[i, ncol(matrix2)]))
      ph2_tr[i] = matrix2[i, ncol(matrix2)] / matrix2[i, min_index_last]
    }
    
    smallest_pos_TR_ind = GetSmallestPositiveIndex(ph2_tr)
    # get the smallest positive TR 
    # ph2_tr[nrow(matrix2)-1] = NA
    
    # print(ph2_tr)
    # matrix3 = matrix2
    # matrix3[ ,ncol(matrix2)+1] = ph2_tr
    # 
    # print(matrix2)
    #     print(ph2_tr)
    # 
    # print(paste("r ", smallest_pos_TR_ind, " c ", min_index_last))
    matrix2 = modifiedGaussJordan(matrix2, smallest_pos_TR_ind, min_index_last)
    
    ph2cond = any(matrix2[nrow(matrix2), 1:(ncol(matrix2)-2)] < 0)
  }
  # print(matrix2)
  
  return(list(x=initial_tableau, fm=matrix2))
  
}

modifiedGaussJordan <- function(matrix2, pivot_row, pivot_col){
  
  # normalize
  matrix2[pivot_row,] = matrix2[pivot_row, ] / matrix2[pivot_row, pivot_col]
  
  for (i in 1:nrow(matrix2)){
    if (i == pivot_row) next 
    temp = matrix2[i, pivot_col] * matrix2[pivot_row, ]
    matrix2[i, ] = matrix2[i, ] - temp
  }
  
  return(matrix2)
}