# A function to calculate mineral content (in percent) in soil by furnace method

mincont <- function(dry, ash, kreuz, data = NULL){
  dry <- eval(substitute(dry), data, parent.frame())
  ash <- eval(substitute(ash), data, parent.frame())
  kreuz <- eval(substitute(kreuz), data, parent.frame())
    mineral <- (ash-kreuz)/(dry-kreuz)*100
  print(mineral)
}
