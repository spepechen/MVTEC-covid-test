# library(tidyr)
library(dplyr)


covid_data <- read.csv(file = 'data/owid-covid-data.csv')

# preview dataset
View(covid_data)

# check each column's class
lapply(covid_data,class)

# covert to right data type
covid_data$date <- as.Date(covid_data$date, format='%Y-%m-%d')


# checking NA count in each column 
missing_value_rate <- colMeans(is.na(covid_data))*100

df <- data.frame(missing_value_rate)

# set index as column
df <- cbind(col_name = rownames(df), df)
rownames(df) <- 1:nrow(df)

m <- df %>%
  rename(rate = missing_value_rate) %>%
  mutate(rate= round( rate, 2))%>% 
  arrange(desc(rate))

write.csv(m,'output/missing_value_rate.csv')

