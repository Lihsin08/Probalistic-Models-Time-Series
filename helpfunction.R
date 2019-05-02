library(fitdistrplus)

# myfunction <- function(arg1, arg2, ... ){
#         statements
#         return(object)
# }
# 
# n=stateNumber
# temp="C:/Users/zhongda/Desktop/ImportOut.csv"
# train=fread(temp,showProgress=T)
# data=train$V2


initialization<-function(data,stateNumber){
n=stateNumber

data=sort(data)

len<-round(length(data)/n)
# splitData<-split(data,1:n,lex.order = TRUE)

means=numeric()
sds=numeric()


# for (i in 1:n) {
#   
#   temp<-data[(i*len-len+1):(n*len)]
#   temp <- temp[!is.na(temp)]
#   #fit.norm <- fitdist(temp, "norm")
#   means<-c(means,mean(temp))
#   sds<-c(sds,sd(temp))
#   
#   
# }

for (i in 3:n) {

  temp<-data[(i*len-len+1):(n*len)]
  temp <- temp[!is.na(temp)]
  #fit.norm <- fitdist(temp, "norm")
  means<-c(means,mean(temp))
  sds<-c(sds,sd(temp))


}

means<-c(means,data[length(data)])
sds<-c(sds,5)
means<-c(means,data[1])
sds<-c(sds,5)

return(list(means,sds))

}

