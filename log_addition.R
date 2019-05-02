log_addition <-function(vector) {
  logxpy <- function(lx,ly) max(lx,ly) + log1p(exp(-abs(lx-ly)))
  prev=logxpy(vector[1],vector[2])
  if (length(vector) <= 2){
    return(prev)
  }
  else{
  for (i in c(3:length(vector))){
    prev=logxpy(prev,vector[i])
  }
  return(prev)
  }
}