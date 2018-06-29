# A function to calculate Shannon-Wiener Index

Hindex <- function(data) {
  require(dplyr) # require dplyr
  
  data2 <- data %>% 
    group_by(sp) %>% 
    summarise(tot.ind = sum(ind)) # Sum of individual of each species across plot
  
  data2 <- data2 %>% 
    mutate(Pi = tot.ind / sum(data2$tot.ind)) # Calculate Pi
  
  data2 <- data2 %>% 
    mutate(Pi.LnPi = log(data2$Pi)) # Calculate Pi.LnPi
  
  H <- -(sum(data2$Pi.LnPi))
  
  print(data2) # Print H' Table
  print(H) # Print H'
}
