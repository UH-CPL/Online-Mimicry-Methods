getemo = function(x){
   x = as.numeric(x)
   if (x == 1) {
      return("Neutral")
   }
   elif (x == 2) {
      return("Calm")
   }
   elif (x == 3) {
      return("Happy")
   }
}
