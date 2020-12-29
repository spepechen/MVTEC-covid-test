# Principle components analysis
[knitr::spin ref](https://github.com/yihui/knitr/blob/master/inst/examples/knitr-spin.R)

`knitr::spin('/Users/spechen/Desktop/MVTEC/mid-term/MVTEC-covid-test/script/spe_pca_test.R', precious = TRUE)`



```r
library(dplyr)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(DMwR)

load(file="/Users/spechen/Desktop/MVTEC/mid-term/MVTEC-covid-test/output/joined_data_by_isocode.RData")
```

PCA for continuous variables
* weekly newcases
* new_cases_smoothed_per_million
* new_deaths_smoothed_per_million
* hospital_beds_per_thousand 

## Preprocessing


```r
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
## `summarise()` regrouping output by 'location' (override with `.groups` argument)
```

### Missing value imputation with KNN methods


```r
exclude_chr <- df[ , names(df) != "location"]
knn_output <- knnImputation(exclude_chr, k = 10, scale = T, meth = "weighAvg",
                            distData = NULL)
anyNA(knn_output)
## [1] FALSE

oct_df <- knn_output %>%
  tibble::rownames_to_column('location') %>%
  separate(location, c("location", "temp"), "_") %>% 
  filter(month == 10) %>%
  select(-month, -temp) %>%
  tibble::column_to_rownames('location')

oct_df <- data.matrix(oct_df)
```

## PCA starts here 
http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


```r
pca <- prcomp((oct_df), scale=TRUE)
fviz_eig(pca)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Scree plot to identify the most prolific components
Sum of PC1 and PC2 is 76.4% meaning these two components can explain majority of the variance in the data set, so ok to retain only two.  


```r
# https://stackoverflow.com/questions/62117449/is-there-a-way-to-add-a-cum-sum-to-a-fviz-eig-plot
fviz_eig(pca,
         addlabels = T, 
         barcolor = "#E7B800", 
         barfill = "#E7B800", 
         linecolor = "#00AFBB", 
         choice = "variance", 
         ylim=c(0,60))
```

![plot of chunk scree-plot](figure/scree-plot-1.png)

```r
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
## Warning: ggrepel: 176 unlabeled data points (too many overlaps). Consider increasing
## max.overlaps
```

![plot of chunk score-plot](figure/score-plot-1.png)

### Loading plot/ Correlation circle: identify the most influential variables
Variables projected onto PC1 and PC2 and show how strongly each characteristic influences a principal component.

sum_new_deaths_smoothed_per_million have more say on PC1.
hospital_beds_per_thousand and stringency_index have strong influence on PC2 

total_death_million is perpendicular to PC2 meaning it has close to zero influence on it.
hospital_beds_per_thousand and stringency_index diverge from each other and form a largest angle (> 120 degree) They are negative correlated.  


```r
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
```

![plot of chunk chart2](figure/chart2-1.png)

not sure how to read biplot


```r
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
## Warning: ggrepel: 137 unlabeled data points (too many overlaps). Consider increasing
## max.overlaps
```

![plot of chunk chart3](figure/chart3-1.png)

```r

f.pca <- PCA(oct_df, ncp = 7, graph = FALSE)

summary(f.pca)
## 
## Call:
## PCA(X = oct_df, ncp = 7, graph = FALSE) 
## 
## 
## Eigenvalues
##                        Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6
## Variance               3.475   1.108   0.699   0.360   0.270   0.088
## % of var.             57.912  18.475  11.646   5.998   4.505   1.465
## Cumulative % of var.  57.912  76.387  88.032  94.030  98.535 100.000
## 
## Individuals (the 10 first)
##                                         Dist    Dim.1    ctr   cos2    Dim.2    ctr   cos2  
## Afghanistan                         |  1.870 | -1.594  0.339  0.727 | -0.333  0.046  0.032 |
## Albania                             |  0.584 |  0.477  0.030  0.667 | -0.200  0.017  0.117 |
## Algeria                             |  1.773 | -0.861  0.099  0.236 | -1.140  0.543  0.414 |
## Andorra                             |  7.014 |  5.503  4.034  0.616 |  0.262  0.029  0.001 |
## Angola                              |  1.401 | -1.217  0.197  0.754 | -0.339  0.048  0.059 |
## Anguilla                            |  1.455 | -1.438  0.276  0.976 |  0.113  0.005  0.006 |
## Antigua and Barbuda                 |  1.495 | -0.857  0.098  0.328 | -0.160  0.011  0.011 |
## Argentina                           |  3.466 |  3.105  1.285  0.802 | -0.152  0.010  0.002 |
## Armenia                             |  4.583 |  4.534  2.739  0.979 |  0.026  0.000  0.000 |
## Aruba                               |  2.542 |  1.653  0.364  0.423 | -0.321  0.043  0.016 |
##                                      Dim.3    ctr   cos2  
## Afghanistan                         -0.746  0.369  0.159 |
## Albania                              0.201  0.027  0.119 |
## Algeria                              1.021  0.691  0.332 |
## Andorra                             -3.293  7.183  0.220 |
## Angola                               0.601  0.239  0.184 |
## Anguilla                             0.108  0.008  0.005 |
## Antigua and Barbuda                  1.168  0.903  0.610 |
## Argentina                            0.254  0.043  0.005 |
## Armenia                             -0.241  0.039  0.003 |
## Aruba                               -1.005  0.670  0.156 |
## 
## Variables
##                                        Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3
## sum_new_cases_smoothed_per_million  |  0.910 23.847  0.829 |  0.113  1.152  0.013 | -0.033
## sum_new_deaths_smoothed_per_million |  0.902 23.440  0.814 |  0.016  0.022  0.000 |  0.121
## hospital_beds_per_thousand          |  0.351  3.539  0.123 |  0.809 59.094  0.655 |  0.428
## total_cases_per_million             |  0.888 22.704  0.789 | -0.003  0.001  0.000 | -0.289
## total_deaths_per_million            |  0.839 20.243  0.703 | -0.112  1.125  0.012 | -0.288
## stringency_index                    |  0.465  6.227  0.216 | -0.654 38.607  0.428 |  0.578
##                                        ctr   cos2  
## sum_new_cases_smoothed_per_million   0.151  0.001 |
## sum_new_deaths_smoothed_per_million  2.084  0.015 |
## hospital_beds_per_thousand          26.224  0.183 |
## total_cases_per_million             11.915  0.083 |
## total_deaths_per_million            11.893  0.083 |
## stringency_index                    47.732  0.334 |

var <- get_pca_var(pca)
# plot by coord
var$coord
##                                         Dim.1        Dim.2       Dim.3       Dim.4       Dim.5
## sum_new_cases_smoothed_per_million  0.9102818 -0.112978460  0.03253309 -0.33806633  0.08952973
## sum_new_deaths_smoothed_per_million 0.9024803 -0.015708579 -0.12068495 -0.18564407 -0.34265817
## hospital_beds_per_thousand          0.3506776 -0.809342889 -0.42805835  0.18625254  0.06375962
## total_cases_per_million             0.8882132  0.003238692  0.28854270  0.02535437  0.32176009
## total_deaths_per_million            0.8386766  0.111674808  0.28827063  0.40768604 -0.15768345
## stringency_index                    0.4651697  0.654173410 -0.57751247  0.09786607  0.11144269
##                                             Dim.6
## sum_new_cases_smoothed_per_million  -0.1877767138
## sum_new_deaths_smoothed_per_million  0.1372566981
## hospital_beds_per_thousand           0.0002704947
## total_cases_per_million              0.1537452618
## total_deaths_per_million            -0.0998918317
## stringency_index                    -0.0125077322
# color by contribution
var$contrib
##                                         Dim.1        Dim.2      Dim.3      Dim.4     Dim.5
## sum_new_cases_smoothed_per_million  23.846746 1.151508e+00  0.1514741 31.7582735  2.965342
## sum_new_deaths_smoothed_per_million 23.439743 2.226124e-02  2.0844616  9.5766846 43.437253
## hospital_beds_per_thousand           3.539105 5.909362e+01 26.2236798  9.6395655  1.503944
## total_cases_per_million             22.704493 9.462696e-04 11.9153789  0.1786317 38.300506
## total_deaths_per_million            20.242607 1.125087e+00 11.8929194 46.1854008  9.198402
## stringency_index                     6.227306 3.860658e+01 47.7320861  2.6614438  4.594553
##                                            Dim.6
## sum_new_cases_smoothed_per_million  4.012666e+01
## sum_new_deaths_smoothed_per_million 2.143960e+01
## hospital_beds_per_thousand          8.326585e-05
## total_cases_per_million             2.690004e+01
## total_deaths_per_million            1.135558e+01
## stringency_index                    1.780355e-01
```

