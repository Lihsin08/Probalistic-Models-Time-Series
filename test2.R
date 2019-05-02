#Obs=data  #set the input data in numeric list
Obs=test2

##Seting initial
nt_guess<-150

states=3
threshold=12

# Expextation Maximization
nt_pre <- nt_guess
nt_now <- EM_nt_test(Obs, states, nt_pre, threshold)
#added threshold 
#threshold=mean((nt_now[[5]]+nt_now[[7]]))
threshold=mean((nt_now[[5]]+nt_now[[7]]))