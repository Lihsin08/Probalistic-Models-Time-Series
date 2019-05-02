#Obs=data  #set the input data in numeric list
Obs=test2

##Seting initial
nt_guess <- 300
states=6


threshold=10

# Expextation Maximization
nt_pre <- nt_guess
nt_now <- EM_nt_test(Obs, states, nt_pre, threshold)
#added threshold 
#threshold=mean((nt_now[[5]]+nt_now[[7]]))*3

iters=0
error=c()
while (abs(nt_now[[1]]-nt_pre) >=10 && iters < 10 ) {
  message(paste('forward2=',iters))
  iters <- iters+1
  print(nt_now[[1]])
  nt_pre <- nt_now[[1]]
  nt_now <- EM_nt_test(Obs, states, nt_pre, threshold)
  #added threshold 
  #threshold=mean((nt_now[[5]]+nt_now[[7]]))*3
  error=c(error, abs(nt_now[[1]]-nt_pre))
  
}







