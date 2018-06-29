library(dplyr)

plot <- rep(c(1,2), each = 5)
sp <- c("A", "B", "C", "D", "E", "B", "C", "D", "E", "F")
ind <- sample(1:10, 10, replace = T)
df <- tibble(plot, sp, ind)
df$plot <- as.factor(df$plot)
df$sp <- as.factor(df$sp)
str(df)
df