
reshaping <- function(user_data,
                      rvs,
                      tbl_no,
                      col1,
                      col2,
                      col3,
                      horizon_year_1,
                      horizon_year_2,
                      horizon_year_3){
  print(paste0("RUNNING: Reshaping Function for table: ", tbl_no))
  #browser()
  
  old_rvs <- rvs[rvs$table_no_ui == tbl_no,] 
  
  for(r in unique(user_data$row)){
    for(c in unique(user_data$col)){
      print(r)
      print(c)
      value <- user_data$value[user_data$row == r & user_data$col == c]
      print(value)
      old_rvs[r,c+1] <- value
      
    }
  }
  
  return(old_rvs)
  }
  
