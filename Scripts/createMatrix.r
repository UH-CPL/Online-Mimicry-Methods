creatematrix = function(df, size){
   dt = df
   mat = matrix(0, size, size)
   dt = dt[,emocol]
   dt = na.omit(dt)
   for (i in 1:nrow(dt)) {
      rw = dt[i,]
      rw = convertRowsToList(rw)[[1]]
      rw = as.numeric(rw)
      p = outer(rw, rw)
      p[lower.tri(p)] = 0
      p1 = p/sum(p)
      mat = mat+p1
   }
   mat[lower.tri(mat)] <- NA
   mat = renamematrix(mat)
   mt <<- mat
}