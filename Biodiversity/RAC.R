# A function to generate rank abundance curve

RAC <- function(data) {
  require(dplyr) # require dplyr
  require(ggplot2) # require ggplot2
  
  data2 <- data %>% 
    group_by(sp) %>% 
    summarise(tot.ind = sum(ind)) # Sum of individual of each species across plot
  
  g <- ggplot(data2, aes(x= reorder(sp, -tot.ind), y = tot.ind)) +
    geom_bar(stat = "identity") + 
    xlab("Spesies") + 
    ylab ("Total Individu") + 
    theme_classic()
  
  print(g) # Display graphic
}