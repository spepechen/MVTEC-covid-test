# library(tidyr)
library(dplyr)


load(file="output/joined_data_by_isocode.RData")

View(joined)

# checking NA count in each column 
missing_value_rate_table  <- function(your_dataframe) {
  missing_value_rate <- colMeans(is.na(your_dataframe))*100
  
  df <- data.frame(missing_value_rate)
  
  # set index as column
  df <- cbind(col_name = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  
  m <- df %>%
    rename(rate = missing_value_rate) %>%
    mutate(rate= round( rate, 2))%>% 
    arrange(desc(rate))
  
  return(m) 
}

missing_value_rate_table(joined)
#write.csv(m,'output/missing_value_rate.csv')

