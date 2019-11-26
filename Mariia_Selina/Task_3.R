helpfunc<-function(column){
 if(is.numeric(column)){
   result<-sum(column)
 } else {
   result<- table(column)
 }
  
}


fun <- function(df,row,column){
  result<- list()
  data <- df[row, column]
  lapply(data,helpfunc)

}

fun(mtcars, 10:20, 1:5)
fun(iris, 1:70, 1:5)
fun(mtcars, 1:30, 2:3) 
