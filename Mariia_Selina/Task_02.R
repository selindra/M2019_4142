fun <- function(df,row,column){
  result<- list()
  for (i in 1: length(column)) {
  data <- df[row, column[i]];
  
  if (is.numeric(data)) {
    s <- sum(data)
    result <- s
  } else {
    s <- table(data)
    result <- s
  }
  print (result)
  }
}
fun(iris, 2:10, 1)
fun(mtcars, 2:20, 1:3)
