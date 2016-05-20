insert_subsamples <- function(vars, trt, ss){
  pos = which(vars == trt)
  po1 = pos - 1
  po2 = pos + 1
  n = length(vars)
  if(n == 1) return(ss)
  if(po1 == 0) return(c(ss, vars[po2:n]))
  if(po2 > n) return(c(vars[1:po1], ss))
  c(vars[1:po1], ss, vars[po2:n])
}
