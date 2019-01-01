################################################################
#
#    Biodiversity Indices
#
################################################################

# 1. Data frame format creation
library(dplyr)

plot <- rep(c(1,2), each = 5)
sp <- c("A", "B", "C", "D", "E", "B", "C", "D", "E", "F")
ind <- sample(1:10, 10, replace = T)
df <- tibble(plot, sp, ind)
df$plot <- as.factor(df$plot)
df$sp <- as.factor(df$sp)
str(df)
df

# 2. Shannon-Wiener Index Calculation

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

# Function to calculate Simpson Dominance Index
Dindex <- function (data){
  require(dplyr)
  
  data2 <- data %>% 
    group_by(sp) %>% 
    summarise(tot.ind = sum(ind)) # Sum of individual of each species across plot
  
  data2 <- data2 %>% 
    mutate(Pi = tot.ind / sum(data2$tot.ind)) # Calculate Pi
  
  data2 <- data2 %>% 
    mutate(Pi2 = Pi^2) # calcuate Pi^2
  
  D <- sum(data2$Pi2)
  print(data2) # Print D table
  print(D)     # Print D
}


# 3. Community Similarity Index
CCindex <- function (data = data, method = c("Sorensen", "Jaccard")){
  # Count number of species found in two communities
  plot1 <- data %>%
    filter(plot==1) %>%
    select(sp)
  
  plot2 <- data %>%
    filter(plot==2) %>%
    select(sp)
  
  # Count Number of species in communities 1
  b <- count(plot1)
  # Count Number of species in communtities 2
  c <- count(plot2)
  # ount number of species found within two communities
  a <- count(intersect(plot1,plot2))
  
  # Jaccard Index
  if (method == "Jaccard"){
    CC_Jac <- a/(a+b+c)
    print(CC_Jac)
    # Sorensen Index
  } else {
    CC_Sor <- 2*a/((2*a)+b+c)
    print(CC_Sor)
  }
}

#-------------------------------------------------------------------------------------------------