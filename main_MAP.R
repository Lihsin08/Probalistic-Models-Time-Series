main_MAP <-function ( Obs, nt_guess, states){


Obs_split <- split(Obs,as.numeric(gl(2,nt_guess,length(Obs))))
mean1=initialization(as.numeric(Obs_split[[1]]),states)[[1]]
mean2=initialization(as.numeric(Obs_split[[2]]),states)[[1]]

sd1 <- initialization(as.numeric(Obs_split[[1]]),states)[[2]]
sd2 <- initialization(as.numeric(Obs_split[[2]]),states)[[2]]

PT <- matrix(1/states, nrow = states, ncol = states)

delta=as.numeric(rep((1/states), states))


Set1 <- dthmm(as.numeric(Obs_split[[1]]), PT, delta,
              "norm",list(mean=mean1,sd=sd1))
Set2 <- dthmm(as.numeric(Obs_split[[2]]), PT, delta,
              "norm",list(mean=mean2, sd=sd2))

HMM1 <- BaumWelch(Set1)
HMM2 <- BaumWelch(Set2)


# Expextation Maximization
nt_pre <- nt_guess
nt_now <- MAP( Obs, HMM1, HMM2, states)
#added threshold 


iters=0
error=c()

while (abs(nt_now[[1]]-nt_pre) >=1 && iters < 10 ) {
  iters <- iters+1
  nt_pre <- nt_now[[1]]
  # generate Model parameters
  Obs_split <- split(Obs,as.numeric(gl(2,nt_pre,length(Obs))))
  Set1 <- dthmm(as.numeric(Obs_split[[1]]), HMM1$Pi, HMM1$delta,
                "norm",HMM1$pm)
  Set2 <- dthmm(as.numeric(Obs_split[[2]]), HMM2$Pi, HMM2$delta,
                "norm",HMM2$pm)
  
  HMM1 <- BaumWelch(Set1)
  HMM2 <- BaumWelch(Set2)
  
  # Maximization (choose the maximum likelihood of transition point T)
  nt_now <- MAP( Obs, HMM1, HMM2, states)


  error=c(error, abs(nt_now[[1]]-nt_pre))
  
}

tran_point=nt_now[[1]]
Prob_max=nt_now[[2]]

return(c(tran_point,Prob_max))

}





