# A function to calculate organic content (in percent) in soil by furnace method

orcont <- function(dry, ash, kreuz, data = NULL){
  dry <- eval(substitute(dry), data, parent.frame())
  ash <- eval(substitute(ash), data, parent.frame())
  kreuz <- eval(substitute(kreuz), data, parent.frame())
  organic <- ((dry-kreuz)-(ash-kreuz))/(dry-kreuz)*100
  print(organic)
}
