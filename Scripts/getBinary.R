getDomBinary = function(x){
  if (is.na(x)) {
    return(NA)
    break
  }
  if (x == "Neutral") {
    return(0)
  }
  return(1)
  
}

getSumBinary = function(x){
  if (is.na(x)) {
    return(NA)
    break
  }
  if (x>0.5) {
    return(0)
    break
  }
  if (x<0.5) {
    return(1)
  }
}