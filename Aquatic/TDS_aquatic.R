# A function to calculte TDS (in mg/L) by using standard gravimetric method (weight measured in mg)

TDS <- function(evdish.b, evdish.a, data = NULL){
  evdish.b <- eval(substitute(evdish.b), data, parent.frame())
  evdish.a <- eval(substitute(evdish.a), data, parent.frame())
  TDS <- (evdish.a-evdish.b)*10
  print(TDS)
}