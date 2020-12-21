library(dplyr)
library(tidyverse)
library(ggplot2)
library(DMwR) #for KNN
# DMwR needs a few dependency
#https://stackoverflow.com/questions/39845442/getting-error-while-installing-dmwr-package


load(file="output/joined_data_by_isocode.RData")

View(joined)


## PCA for continuous variables
# weekly newcases
# new_cases_smoothed_per_million
# new_deaths_smoothed_per_million
# hospital_beds_per_thousand

# preprocessing ---------------

selected_eu <- c("France", "Portugal", "Italy", "Germany", "Finland", "Denmark", "Norway", "Netherlands", "Spain", "United Kingdom") 

df <- joined %>%
  # filter(location %in% selected_eu) %>%
  filter(Continent == 'Europe') %>%
  select(iso_code, 
         location,
         date, 
         new_cases_smoothed_per_million, 
         new_deaths_smoothed_per_million, 
         hospital_beds_per_thousand, 
         total_deaths_per_million, 
         total_cases_per_million, 
         stringency_index) %>%
  mutate(month = as.numeric(as.factor(months(date)))) %>%
  mutate(hospital_beds_per_thousand = replace(hospital_beds_per_thousand,
                                              is.na(hospital_beds_per_thousand),
                                              median(hospital_beds_per_thousand, na.rm = T))) %>% #replace missing value with median
  group_by(location, month) %>%
  summarise(sum_new_cases_smoothed_per_million = sum(new_cases_smoothed_per_million),
            sum_new_deaths_smoothed_per_million = sum(new_deaths_smoothed_per_million),
            hospital_beds_per_thousand = median(hospital_beds_per_thousand),
            total_cases_per_million = max(total_cases_per_million), 
            total_deaths_per_million = max(total_deaths_per_million),
            stringency_index = max(stringency_index)) %>%
  mutate(location_mo = paste(location, month, sep = '_')) %>%
  tibble::column_to_rownames('location_mo')

# missing value imputation with KNN---------------
exclude_chr <- df[ , names(df) != "location"]
knn_output <- knnImputation(exclude_chr, k = 10, scale = T, meth = "weighAvg",
              distData = NULL)

anyNA(knn_output)

oct_df <- knn_output %>% 
  filter(month == 10) %>%
  select(-month)

oct_df <- data.matrix(oct_df)



# PCA ---------------
pca <- prcomp(t(oct_df), scale=TRUE)
plot(pca$x[,1], pca$x[,2])

pca_var <- pca$sdev^2
pca_var_per <- round(pca_var/sum(pca_var)*100, 1)

# scree plot
barplot(pca_var_per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca_data <- data.frame(Variables=rownames(pca$x), 
                       X=pca$x[,1], 
                       Y=pca$x[,2])

ggplot(data=pca_data, aes(x=X, y=Y, label=Variables)) + 
  geom_text(size=3) +  #plot labels rather than dots or shapes
  xlab(paste("PC1 : ", pca_var_per[1], "%", sep= "")) +
  ylab(paste("PC2 :  ", pca_var_per[2], "%", sep= "")) +
  theme_bw() +
  ggtitle("PCA Graph of EU")


loading_scores <- pca$rotation[,1]
variables_scores <- abs(loading_scores)