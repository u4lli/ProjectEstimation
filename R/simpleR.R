#' @export
testAdd <- function(a,b)
{
  a+b
}

#' @export
signlog <- function(x){
  sign(x) * log(sign(x)*x + 1)
}

#' @export
signlog_numeric <- function(x){
  if (is.numeric(x)) signlog(x) else x 
}

#' @export
inverse_signlog <- function(x){
  sign(x)*(exp(1)^(sign(x)*x) -1)
}
