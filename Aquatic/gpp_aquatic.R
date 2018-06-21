#A function to calculate gross primary (in mg O2/L/hour) productivity by using light-dark bottle method

gpp <- function(light.b, light.a, t, data = NULL){
  light.b <- eval(substitute(light.b), data, parent.frame())
  light.a <- eval(substitute(light.a), data, parent.frame())
  gpp <- (light.a-light.b)/t
  print(gpp)
}