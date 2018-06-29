###########################
#
#  SPECIES ABUNDANCE CURVE
#
###########################

##-------------------------------------------------------------------------------
## Note
##
##
# Rank abundance curve of Whittaker is a plot of char used by ecologist
# to display relative species abundance, a component of bidodiversity.
# It can also be used to visualize species evenness.
# x-axis: the abundance rank. The most abundant species is given in rank 1
# Y-axis: the Relative abundances, usually measured on a log scale
#
# WARNING!
# Don't change the column variable name (EKOSISTEM)!!
#--------------------------------------------------------------------------------

# Package dependencies
install.packages("xlsx")
install.packages("ggplot2")
install.packages("dplyr")

# Import library
library (xlsx)
library (ggplot2)
library (dplyr)

#----------
# Example
#----------
# Importing data
raw_data <- read.xlsx(choose.files(), sheetIndex = 1, header=T)

# Function for ecosystem rank abundance comparison
Rank_abundance_curve <- function (x){
  require(ggplot2)
  require(dplyr)
  data <- transform(x,abundance=rowSums(raw_data[,3:ncol(x)]))
  data <- data %>% group_by(EKOSISTEM) %>%
            mutate(mx=max(abundance)) %>%
            arrange(desc(mx), desc(abundance)) %>%
            select(-mx)
  data$rank <- ave(data$abundance, data$EKOSISTEM, FUN=seq_along)
  
  #plot the graph
  plot <- ggplot(data=data, aes(x=rank, y=abundance, group=EKOSISTEM)) +
    geom_line(aes(linetype=EKOSISTEM, color=EKOSISTEM))+
    geom_point(aes(shape=EKOSISTEM))
  
  return (list(data, plot))
  
}

# Output
result <- Rank_abundance_curve(raw_data)

# Export datasheet into local folder
write.xlsx(result[1], file=choose.files(caption="Save As...",filters = c("MS Excel (.xlsx)", "*.xlsx")),col.names = TRUE)

# Export the graph
# Follow this three line commands
png(filename = choose.files(caption="Save As...",filters = c("png (.png)", "*.png")))
plot(result[[2]])
dev.off()



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#==================================================================================
#  END OF THE LINE
#==================================================================================

# Raw data of scripts
#---------------------------------
# #sum total abundance in each species and each ecosystem
# raw_data_total <- transform(raw_data, sum=rowSums(raw_data[,3:ncol(raw_data)]))
# raw_data_total <- raw_data_total %>% group_by(EKOSISTEM) %>% 
#   mutate(mx = max(sum)) %>% 
#   arrange(desc(mx), desc(sum)) %>% 
#   select(-mx)
# 
# #give rank number for each species in each ecosystem
# raw_data_total$rank <- ave(raw_data_total$sum, raw_data_total$EKOSISTEM, FUN = seq_along)
# 
# plot <- ggplot(data=raw_data_total, aes(x=rank, y=sum, group=EKOSISTEM)) +
#   geom_line(aes(linetype=EKOSISTEM, color=EKOSISTEM))+
#   geom_point(aes(shape=EKOSISTEM))


# Function for only one ecosystem
#------------------------------------
# Rank_abundance_curve <- function (x){
#   require(ggplot2)
#   # data results
#   data <- data.frame (t(x), stringsAsFactors = F)
#   colnames(data)<-data[1,]
#   data <- data[-(1),]
#   data[colnames(data)] <- sapply (data [colnames(data)], as.numeric)
#   sum_data <- data.frame(abundance=colSums(data), rank=0)
#   sum_data <- sum_data[order(sum_data$abundance, decreasing = T),]
#   sum_data$rank <- seq(1, nrow(sum_data))
#   
#   #plot results
#   plot <- ggplot(data=raw_data, aes(x=rank, y=abundance)) +
#             geom_line()+
#             geom_point()+
#             ggtitle("Ecosytem A")
#   
#   return (list(sum_data, plot))
# }
# 
# # output
# result <- Rank_abundance_curve(raw_data)
# plot(result[[2]])

# export the output
