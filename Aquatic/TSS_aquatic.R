# A function to calculte TSS (in mg/L) by using standard gravimetric method (weight measured in mg)

TSS <- function(filter.b, filter.a, kreuz, data = NULL){
  filter.b <- eval(substitute(filter.b), data, parent.frame())
  filter.a <- eval(substitute(filter.a), data, parent.frame())
  kreuz <- eval(substitute(kreuz), data, parent.frame())
  TSS <- ((filter.a+kreuz)-(filter.b+kreuz))*10
  print(TSS)
}