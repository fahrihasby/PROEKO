#A function to calculate net primary (in mg O2/L/hour) productivity by using light-dark bottle method

npp <- function(light.b, light.a, dark.b, dark.a, t, data = NULL){
  light.b <- eval(substitute(light.b), data, parent.frame())
  light.a <- eval(substitute(light.a), data, parent.frame())
  dark.b <- eval(substitute(dark.b), data, parent.frame())
  dark.a <- eval(substitute(dark.a), data, parent.frame())
  npp <- ((light.a-light.b)-(dark.a-dark.b))/t
  print(npp)
}