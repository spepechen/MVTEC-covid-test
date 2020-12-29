#' # Principle components analysis
#' [knitr::spin ref](https://github.com/yihui/knitr/blob/master/inst/examples/knitr-spin.R)
#' 
#' `knitr::spin('/Users/spechen/Desktop/MVTEC/mid-term/MVTEC-covid-test/script/spe_pca_test.R', precious = TRUE)`

#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = TRUE)

#+ message=FALSE
library(dplyr)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(DMwR)

load(file="/Users/spechen/Desktop/MVTEC/mid-term/MVTEC-covid-test/output/joined_data_by_isocode.RData")


#' PCA for continuous variables
#' * weekly newcases
#' * new_cases_smoothed_per_million
#' * new_deaths_smoothed_per_million
#' * hospital_beds_per_thousand 
#' 
#' ## Preprocessing

#selected_eu <- c("France", "Portugal", "Italy", "Germany", "Finland", "Denmark", "Norway", "Netherlands", "Spain", "United Kingdom") 

df <- joined %>%
  # filter(location %in% selected_eu) %>%
  # filter(Continent == 'Europe') %>%
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

#' ### Missing value imputation with KNN methods
#+ test-a, cache=FALSE
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

#' ## PCA starts here 
#' http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

pca <- prcomp((oct_df), scale=TRUE)
fviz_eig(pca)


#' ### Scree plot to identify the most prolific components
#' Sum of PC1 and PC2 is 76.4% meaning these two components can explain majority of the variance in the data set, so ok to retain only two.  

#+ scree-plot
# https://stackoverflow.com/questions/62117449/is-there-a-way-to-add-a-cum-sum-to-a-fviz-eig-plot
fviz_eig(pca,
         addlabels = T, 
         barcolor = "#E7B800", 
         barfill = "#E7B800", 
         linecolor = "#00AFBB", 
         choice = "variance", 
         ylim=c(0,60))


#+ score-plot
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#' ### Loading plot/ Correlation circle: identify the most influential variables
#' Variables projected onto PC1 and PC2 and show how strongly each characteristic influences a principal component.
#' 
#' sum_new_deaths_smoothed_per_million have more say on PC1.
#' hospital_beds_per_thousand and stringency_index have strong influence on PC2 
#' 
#' total_death_million is perpendicular to PC2 meaning it has close to zero influence on it.
#' hospital_beds_per_thousand and stringency_index diverge from each other and form a largest angle (> 120 degree) They are negative correlated.  

#+ chart2
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#' not sure how to read biplot

#+ chart3, fig.width=10, fig.height=10
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

f.pca <- PCA(oct_df, ncp = 7, graph = FALSE)

summary(f.pca)

var <- get_pca_var(pca)
# plot by coord
var$coord
# color by contribution
var$contrib

