library(BBmisc)

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

creatematrix2 = function(df, size){
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
   mat = renamematrix2(mat)
   mt <<- mat
}


creatematrix3 = function(df, size){
   dt = df
   mat = matrix(0, size, size)
   dt = dt[,emocol]
   dt = na.omit(dt)
   if (nrow(dt)>0) {
      for (i in 1:nrow(dt)) {
         rw = dt[i,]
         rw = convertRowsToList(rw)[[1]]
         rw = as.numeric(rw)
         p = outer(rw, rw)
         p[lower.tri(p)] = 0
         p[upper.tri(p)] = p[upper.tri(p)]*2
         mat = mat+p
      }
      mat[lower.tri(mat)] <- NA
      mat = renamematrix2(mat)
      mt <<- mat
   }
   else{
      mata = matrix(data = 0,nrow = 5, ncol = 5)
      mata = renamematrix2(mata)
      mt <<- mata
   }

}


