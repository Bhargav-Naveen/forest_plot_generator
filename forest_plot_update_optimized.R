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
  return(data   )
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
  xlab("Insert_plot_title")

}

png("test_f.png", width = 4, height = 8, units = 'in', res = 800)
generatePlot(data)
dev.off()
