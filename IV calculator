
tiedosto <- read.csv("dataiv2.csv",header = TRUE, sep=";")
na.omit(tiedosto)
as.numeric(tiedosto$callprice)
as.numeric(tiedosto$strike)
as.numeric(tiedosto$risk)
as.numeric(tiedosto$underlying)
as.numeric(tiedosto$timetomat)
print(tiedosto[ ,3])




BlackScholes <- function(S, K, r, T, sig){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)
  

}



# CallValueperSigma <- BlackScholes(S,K,r,Time,sigma,type)
for(i in 1:nrow(tiedosto)){
  sigma <- seq(0.01,0.5,0.005)
  scholes <- BlackScholes(tiedosto[i,3],tiedosto[i,2],tiedosto[i,5],tiedosto[i,4],sigma,type)
  iv<-which.min(abs(scholes-tiedosto[i,1]))/2
  impl_vol[i] <- iv
  
}

tiedosto$iv <- impl_vol
