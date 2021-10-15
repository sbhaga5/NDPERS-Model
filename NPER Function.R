#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  if(nper == 0){
    a <- 0
  }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Bisection Method
GetNPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g)
  TempValue <- R*abs(pmt-PV)+PV
  print(TempValue)
  print(R)
  NPER <- log(R*pmt/TempValue,R)
  return(NPER)
}
