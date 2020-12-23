library(dplyr)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(FactoMineR)


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
  tibble::rownames_to_column('location') %>%
  separate(location, c("location", "temp"), "_") %>% 
  filter(month == 10) %>%
  select(-month, -temp) %>%
  tibble::column_to_rownames('location')


oct_df <- data.matrix(oct_df)

# PCA ---------------
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
  
pca <- prcomp((oct_df), scale=TRUE)
fviz_eig(pca)

fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

f.pca <- PCA(oct_df, ncp = 7, graph = FALSE)

summary(f.pca)


# ---- 

plot(pca$x[,1], pca$x[,2])

pca_var <- pca$sdev^2
pca_var_per <- round(pca_var/sum(pca_var)*100, 1)

# scree plot
png("output/pca/scree_plot.png")
barplot(pca_var_per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
dev.off()


pca_data <- data.frame(Variables=rownames(pca$x), 
                       X=pca$x[,1], 
                       Y=pca$x[,2])

ggplot(data=pca_data, aes(x=X, y=Y, label=Variables)) + 
  geom_text(size=3) +  #plot labels rather than dots or shapes
  xlab(paste("PC1 : ", pca_var_per[1], "%", sep= "")) +
  ylab(paste("PC2 :  ", pca_var_per[2], "%", sep= "")) +
  theme_bw() +
  ggtitle("PCA Graph of EU countries")
ggsave("output/pca/pca_plot.png")



loading_scores <- pca$rotation[,1]
variables_scores <- abs(loading_scores)
variables_scores_ranked <- sort(variables_scores, decreasing = TRUE)
top_10_var <- names(variables_scores_ranked[1:10])

pca$rotation[top_10_var,1]