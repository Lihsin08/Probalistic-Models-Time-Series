Test_statistics <-function(forward_alpha, states )
# this function calculate the test statistic if an observed sequence 
#  is related to an HMM parameter
## input: forward prob(alpha), number of states
## output: a vector of confidence prob of each time point given previous observation
## ** The output vector eliminate the 1st and 2nd time points starts from 3rd  
{
  alpha=matrix(forward_alpha,ncol = states)
  time_step<-nrow(alpha)
  
  # discard the time 1 and 2
  #calculate the conditional prob
  test_statistic=c()
  marginal=c()

  for (i in c(2:time_step)) {
    temp <- log_addition(alpha[i,])
    marginal <- c(marginal, temp)
  }
  for (i in c(2:length(marginal))) {
    temp <- marginal[i]-marginal[i-1]
    test_statistic=c(test_statistic,temp)
  }
  
  
  return(test_statistic)
  
  ##convert the object to numeric for visualization
  
}
