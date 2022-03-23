renamematrix = function(z){
   y = z
   cn = c("Angry","Afraid","Happy","Sad","Neutral")
   rownames(y) = cn
   colnames(y) = cn
   y<<-y
}

renamematrix2 = function(z){
   y = z
   cn = c("An","Af","Ha","Sa","Ne")
   rownames(y) = cn
   colnames(y) = cn
   y<<-y
}
