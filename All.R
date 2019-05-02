#initialization
Obs=test2
states=3

nt_winfow = round(length(Obs)/4, digits = 0)
Tmin=1+nt_winfow
Tmax=1+3*nt_winfow
step=round((Tmax-Tmin)/2)

##Seting initial
probability=c()
transition=c()
for (i in 1:step){
  nt=Tmin+(i*step)
  #message(paste('nt=',nt))

  result=main_MAP( Obs, nt, states)
  transition=c(transition,result[[1]])
  probability=c(probability,result[[2]])

}




out=transition[which(probability == max(probability))[1]]
