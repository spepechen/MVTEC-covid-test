library(dplyr) # for data cleaning
library(cluster)
library(dendextend)

load(file="output/joined_data_by_isocode.RData")

View(joined)
View(ddExtra)
View(eu)

# Preprocessing  -------------------------------------
# select only EU countries 

selected_countries <- c('Germany', 'United Kingdom', 'France', 'Spain', 'Portugal', 
                 'Finland', 'Norway', 'Denmark', 'Italy', 'Netherlands') 
                 #'Ireland', 'Switzerland','Austria'

selected_col <- c('location','total_cases_per_million', 'total_deaths_per_million', 'total_cases_per_million', 
                  'gdp_per_capita', 'hospital_beds_per_thousand', 'Government_Type')

eu <- joined %>% 
  filter(location %in% selected_countries) %>%
  filter(hospital_beds_per_thousand > 0) %>% # just to make sure 
  filter(date == as.Date("2020-10-31", format = "%Y-%m-%d")) %>% 
  select(one_of(selected_col))
  
glimpse(eu) # 4 continuous, 1 catergorical

# Clustering -------------------------------------
# https://dpmartin42.github.io/posts/r/cluster-mixed-types#calculating-distance 
gower_dist <- daisy(eu[, -1],
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)


gower_mat <- as.matrix(gower_dist)

# The most similar 
eu[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# The most dissimilar
eu[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Plot the tree -------------------------------------

hclust_avg <- hclust(gower_dist, method = 'average')
plot(hclust_avg)

# cutting tree

cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3 ,border = 2:6)
abline(h =0.4, col = 'yellow')

#dealing with labels
# with hardcoded chr labels but can't add rect
dend <- dendextend::color_branches(as.dendrogram(hclust_avg), k = 3)
dendextend::labels(dend) <- c("France", "Portugal", "Italy", "Germany", "Findland", "Denmark", "Norway", "Netherlands", "Spain", "United Kingdom") 
plot(dend)

# more on dendextend pkg 
# https://stackoverflow.com/questions/32720128/labels-in-dendrogram-in-r
# https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html#introduction
#  https://cran.r-project.org/web/packages/dendextend/vignettes/Quick_Introduction.html