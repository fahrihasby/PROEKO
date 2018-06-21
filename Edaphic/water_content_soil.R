# A function to calculate water content (in percent) in soil by gravimetric method

wacont <- function(wet, dry, alfol, data = NULL) {
  wet <- eval(substitute(wet), data, parent.frame())
  dry <- eval(substitute(dry), data, parent.frame())
  alfol <- eval(substitute(alfol), data, parent.frame())
  water <- (dry-alfol)/(wet-alfol)*100
  print(water)
}