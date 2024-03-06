func_max = function(x, na.rm = TRUE) {
  if(all(is.na(x))) return(x[1])
  return(max(x, na.rm = na.rm))
}

func_min = function(x, na.rm = TRUE) {
  if(all(is.na(x))) return(x[1])
  return(min(x, na.rm = na.rm))
}