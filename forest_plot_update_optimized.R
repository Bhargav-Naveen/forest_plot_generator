library(tidyverse)
library(data.table)

forestPlotMatrix <- function(data, threshold) {
  
  
  data$AboveThreshold = ""
  
  numCols <- colnames(data[sapply(data, is.numeric)])
  
  # Loop through rows
  for (i in 1:nrow(data)) {
    
    # Find columns that exceed the threshold
    above_threshold <- colnames(data[numCols])[data[i,numCols] >= threshold]
    
    # Store column names in the new column
    data$AboveThreshold[i] <- paste(above_threshold, collapse = ",")
  }
  
  data <- data[data$AboveThreshold != "",]
  data <- separate_rows(data, AboveThreshold, sep = ",")
  
  # Return the modified data frame
  return(data)
}

#z_score <- function(x){
  
#  return((x-mean(x, na.rm=T))/sd(x, na.rm=T))
  
#}

alleleMatch <- function(baseTable){
  for (i in 1:nrow(baseTable)) {
    if(baseTable$`T2D risk increasing allele`[i] != baseTable$ea[i])
    {
      baseTable$beta[i] <-  -(baseTable$beta[i])
      baseTable$ea[i] <- baseTable$`T2D risk increasing allele`[i]
    }
  }
  return(baseTable)
}

generatePlot <- function(data) {
  
  ggforestplot::forestplot(
  df = data,
  name = locus,
  estimate = beta,
  se = se,
  colour = cluster
) + ggforce::facet_col(
  facets = ~cluster,
  scales = "free_y",
  space = "free"
) +
  theme(axis.text.y = element_text(size = 8, face = "italic"), 
        axis.title.x = element_text(size = 9), 
        axis.text.x = element_text(size = 8), 
        legend.position = "none")  +
  xlab("Change in BW Z score per fetal T2D risk allele")

}

data <- as.data.frame(readxl::read_xlsx('/Users/bg384/Desktop/forest_Plot_update.xlsx'))
threshold <- 0.832
data <- forestPlotMatrix(data, threshold)
#table(data$AboveThreshold)

#ncol(data)
#str(data)

bwGwasSummary <- fread('/Users/bg384/Downloads/Fetal_Effect_European_meta_NG2019.txt.gz')

bwGwasSummary <- as.data.frame(bwGwasSummary)

joint_clust_bw <- inner_join(data, 
                             bwGwasSummary, 
                             join_by(x$"rsID" == y$"RSID"), 
                             multiple = "all")

joint_clust_bw$ea <- toupper(joint_clust_bw$ea)

#table(joint_clust_bw$locus)



joinData <- joint_clust_bw
joinData <- alleleMatch(joinData)

#joinData$`T2D risk increasing allele` == joinData$ea

#names(joinData)

joinData <- select(joinData, 
                   rsID, 
                   locus,
                   "cluster" = AboveThreshold, 
                   'risk_allele' = "T2D risk increasing allele",
                   beta, 
                   p, 
                   se)


table(joinData$cluster)

dat2 <- joinData %>% arrange(cluster)

png("test_f.png", width = 4, height = 8, units = 'in', res = 800)
generatePlot(dat2[dat2$cluster %in% c("Obesity"),])
dev.off()
