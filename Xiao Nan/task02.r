
my_f <- function(data_frame, row_sel,col_sel) {
  sub_data <- data_frame[row_sel,col_sel]
  result_list <- list()
  for (i in colnames(sub_data)) {
    if (is.numeric(sub_data[,i])) {
      sum_sub <- sum(sub_data[, i])
      result_list[[i]] <- sum_sub
    } else {
      freq_table <- table(sub_data[,i])
      result_list[[i]] <- freq_table
    }
  }
  return(list(sub_data, result_list))
}

#for example
my_f(iris, c(1,3,5),1:3) 
my_f(iris, c(1:10), c(1:5))

