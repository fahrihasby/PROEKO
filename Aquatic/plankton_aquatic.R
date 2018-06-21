# A function to calculate phytoplankton or cultured microalgae cell density in cell/mL by using standard o.1 mm depth 
# haemocytometer, counted in smaller grid (3 line square in the middle grid)

haemo <- function(cell, dil, cham, data = NULL){
  cell <- eval(substitute(cell), data, parent.frame())
  dil <- eval(substitute(dil), data, parent.frame())
  cham <- eval(substitute(cham), data, parent.frame())
  dens <- (cell/(cham*4))*dil*1000000
  print(dens)
}