# A function to calculate soil bulk dencity in gr/cm3 by standard proeko PVC core sampler 

bulk <- function(dry, data = NULL, d = 7.62, h =5){
  dry <- eval(substitute(dry), data, parent.frame())
  density <- dry/(pi*(0.5*d)^2*h)
  print(density)
}
