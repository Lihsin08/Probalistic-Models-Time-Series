EM_nt_test <-function( Obs, states, nt, threshold){
  

  Obs_split <- split(Obs,as.numeric(gl(2,nt,length(Obs))))
  mean1=initialization(as.numeric(Obs_split[[1]]),states)[[1]]
  mean2=initialization(as.numeric(Obs_split[[2]]),states)[[1]]

  sd1 <- initialization(as.numeric(Obs_split[[1]]),states)[[2]]
  sd2 <- initialization(as.numeric(Obs_split[[2]]),states)[[2]]
  
  # mean1=initialization(Obs,states)[[1]]
  # mean2=initialization(Obs,states)[[1]]
  # sd1 <- as.numeric(rep(10, states))
  # sd2 <- as.numeric(rep(10, states))

  PT <- matrix(1/states, nrow = states, ncol = states)
  # delta <- as.numeric(rep(0, states))
  # delta[3]=1
  delta=as.numeric(rep((1/states), states))
  
  
  Set1 <- dthmm(as.numeric(Obs_split[[1]]), PT, delta,
                "norm",list(mean=mean1,sd=sd1))
  Set2 <- dthmm(as.numeric(Obs_split[[2]]), PT, delta,
                "norm",list(mean=mean2, sd=sd2))
  
  HMM1 <- BaumWelch(Set1)
  HMM2 <- BaumWelch(Set2)
  
  ## use test_statistic to calculate initial nt
  forward1=forward(Obs,HMM1$Pi,HMM1$delta,HMM1$distn,HMM1$pm)
  forward2=forward(Obs,HMM2$Pi,HMM2$delta,HMM2$distn,HMM2$pm)
  #message(paste('forward2=',forward2))
  
  p1log=Test_statistics(forward1,states)
  p2log=Test_statistics(forward2,states)
  
  p1=exp(p1log)
  p2=exp(p2log)
  
  Test_statistics=log(p2/p1)
  
  
  #calculate nt_ini
  Cumindex=c()
  index=0  # set the previous cumulative value to 0
  nt_index=0  # set the nt time
  nt_new= 200
  # error state:nt_new=0
  for (i in c(1:length(Test_statistics))){
    pre <- index
    if (index <= threshold*(-1)){
      index=0
      index=index+Test_statistics[i]
      Cumindex=c(Cumindex,index)
    }
    else if (index > threshold*(-1) && index < threshold ){
      index=index+Test_statistics[i]
      Cumindex=c(Cumindex,index)
    }
    else{
      if (nt_index==0){
        nt_index=1
        nt_new=i
        index=index+Test_statistics[i]
        Cumindex=c(Cumindex,index)
      }
      else{
        index=index+Test_statistics[i]
        Cumindex=c(Cumindex,index)
      }
      
    }  
   if(is.nan(index)){ index <- pre }  
  }
  test=cumsum(Test_statistics)
  plot(Test_statistics,type="l",xlim=c(1,length(Test_statistics)))
  #plot(Cumindex,type="l", ylim=c(-10,400))
  #plot(Cumindex,type="l")
  #abline(h=5.559942,col='red')
  return(list(nt_new,HMM1$Pi,HMM2$Pi,HMM1$pm$mean,HMM1$pm$sd,HMM2$pm$mean,HMM2$pm$sd))
  
  
}
