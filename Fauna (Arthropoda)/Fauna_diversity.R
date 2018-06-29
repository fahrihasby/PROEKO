#####################################
#
#       Fauna Diversity
#
#####################################


######
# NOTE
#
#
########


# importing library
library (xlsx)

# imporing data
fauna_data <- read.xlsx(choose.files(),sheetIndex = 1,header = TRUE,row.names=1)

# Total arthropoda found
fauna_data$Total <- rowSums(fauna_data[,1:ncol(fauna_data)-1])

#-----------------------------------
# Relative Abundance Calculation
#-----------------------------------
# Relative abundance function
RelativeAbundance <- function (x){
  Population_total <- sum(fauna_data$Total)
  Fauna_population <- fauna_data$Total[x]
  Kr <- Fauna_population * 100 /Population_total
  return(Kr)
}

#  Creating new column for Relative Abundance
fauna_data$Relative_abundance <- 0

# find the highest number of site
High_Site <- max(as.numeric(gsub("Site.", "", colnames(fauna_data))), na.rm = T)

# Relative Abundance Results
for (i in 1:nrow(fauna_data)){
    fauna_data$Relative_abundance[i] <- RelativeAbundance(i)
}

#-----------------------------------
# Shannon-Wiener Diversity
#-----------------------------------
# Shannon-Wiener diversity function
Diversity_ShanWien <- function (y){  #y is for site
  Site <- paste0("Site.",y)
  #convert zero value into NA
  fauna_data[fauna_data==0] <- NA
  #Proportion
  NA_removal <- na.omit(fauna_data[,Site])
  Pi <- NA_removal/sum(NA_removal)
  lnPi <- log(NA_removal/sum(NA_removal))
  pilnpi <- Pi * lnPi
  #diversity
  Diversity <- -(sum(pilnpi))
  return (Diversity)
}

# Creating New data.frame
Fauna_diversity_calculation <- data.frame(Diversity = 1:High_Site, row.names = paste0("Site.",1:High_Site))

# Shannon- Wiener Diversity Calculation
for (i in 1:High_Site){
  Fauna_diversity_calculation$Diversity[i] <- Diversity_ShanWien(i)
}

