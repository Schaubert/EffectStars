effect.stars.vglm <- function(vglm.obj){

  coef.orig <- coefficients(vglm.obj, matrix = TRUE)
  vcov.orig <- vcov(vglm.obj)
  
  kat.names <- vglm.obj@misc$ynames
  
  
  
}