t2s <- function(x){
   a = as.character(x)
   a = as.numeric(strsplit(x, split = "[.]")[[1]][1]) * 3600
   b = as.numeric(strsplit(x, split = "[.]")[[1]][2]) * 60
   c = as.numeric(strsplit(x, split = "[.]")[[1]][3])
   return(a+b+c)
   #print(a+b+c)
}