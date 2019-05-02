MAP <-function ( Obs, HMM1, HMM2, states){
  nt_winfow= round(length(Obs)/4, digits = 0)
  Tmin=nt_winfow
  Tmax=3*nt_winfow
  probability=c()
  for (T in Tmin:Tmax){
    Obs_split <- split(Obs,as.numeric(gl(2,T,length(Obs))))
    forward1 <- forward(as.numeric(Obs_split[[1]]),HMM1$Pi,HMM1$delta,HMM1$distn,HMM1$pm)
    forward2 <- forward(as.numeric(Obs_split[[2]]),HMM2$Pi,HMM2$delta,HMM2$distn,HMM2$pm)
    alpha1 <- matrix(forward1,ncol = states)
    alpha2 <- matrix(forward2,ncol = states)
    p1 <- log_addition(alpha1[nrow(alpha1),])
    p2 <- log_addition(alpha2[nrow(alpha2),])
    ptotal <- p1+p2
    probability=c(probability,ptotal)
  }
  
  plot( probability,type="l")
  return(c(Tmin+which(probability == max(probability,na.rm=TRUE))[1],max(probability,na.rm=TRUE)))
  #return(probability)

  
  
}