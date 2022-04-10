library(stringr)

getDomEmo = function(x){
  if (is.na(x[1])) {
    return(NA)
    break
  }
  if (!is.na(x[1])) {
    o = order(x, decreasing = T)[1]
    dom = names(x[o])
    dom = strsplit(dom, split = "_")[[1]][2]
    dom = str_to_title(dom)
    return(dom)
  }

}

getSumEmo = function(x){
  if (is.na(x)) {
    return(NA)
    break
  }
  if (x>0.5) {
    return("Neutral")
    break
  }
  if (x<0.5) {
    return("Emotion")
  }
}