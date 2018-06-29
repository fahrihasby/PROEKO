###########################
#
#  SPECIES ABUNDANCE CURVE
#
###########################

##
## Note
##
##
# Rank abundance curve of Whittaker is a plot of char used by ecologist
# to display relative species abundance, a component of bidodiversity.
# It can also be used to visualize species evenness.
# x-axis: the abundance rank. The most abundant species is given in rank 1
# Y-axis: the Relative abundances, usually measured on a log scale

# Package dependencies
install.packages("xlsx")
install.packages("ggplot2")

# Import library
library (xlsx)
library (ggplot2)

#----------
# Example
#----------
# Importing data
raw_data <- read.xlsx(choose.files(), sheetIndex = 1, header=T)

# Function
Rank_abundance_curve <- function (x){
  require(ggplot2)
  # data results
  data <- data.frame (t(x), stringsAsFactors = F)
  data <- data[-1,]
  data[colnames(data)] <- sapply (data [colnames(data)], as.numeric)
  sum_data <- data.frame(abundance=colSums(data), rank=0)
  sum_data <- sum_data[order(sum_data$abundance, decreasing = T),]
  sum_data$rank <- seq(1, nrow(sum_data))
  
  #plot results
  plot <- ggplot(data=sum_data, aes(x=rank, y=abundance)) +
            geom_line()+
            geom_point()+
            ggtitle("Ecosytem A")
  
  return (list(sum_data, plot))
}

# output
result <- Rank_abundance_curve(raw_data)
plot(result[[2]])

# export the output
