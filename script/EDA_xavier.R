library(tidyverse)
df <- load("output/merged_data.RData")


# GROUP BY MONTH | LOCATION  -----------------------------------  
  dfmonthly <- df %>%
    group_by(location,month=lubridate::floor_date(date,"month")) %>% 
    select(-date) %>% 
    summarize_all(min) %>% # TODO check functions for each var
    mutate(year_month=format(month, "%Y - %m"))
  
  dfweekly <- df %>%
    group_by(location,week=lubridate::floor_date(date,"week")) %>% 
    select(-date) %>% 
    summarize_all(min) %>% 
    mutate(year_week=format(week, "%Y - week %W")) 
  
  dflocation <- df %>%
    group_by(location) %>% # TODO check functions for each var
    select(-date, -iso_code, -continent) %>%
    summarize_all(max)
  
  # Transpose data by location
  t_dflocation <- t(dflocation) %>% .[-1,] # delete first row

# MISSING VALUES ANALYSIS  -----------------------------------
  # Plot missing values Use grouped data to ease visualization
  visdat::vis_miss(dfweekly)
  # Observations:
  # Weekly data is mostly missing
  # Hospital data is also scarce (ICU, patients...)
  # Data about `new_tests` also shows around 60% missing data
  # TODO: Notice that `tests_units` is not numerical and should be recoded
  # Other data with missing patterns
  # `total_deaths`, `total_deaths_per_million`
  # `reproduction_rate`
  # `stringency_index`
  # `extreme_poverty`
  # workers by gender demographics
  # `handwashing_facilities`
  
  # Transpose data by location
  t_dflocation <- t(dflocation) %>% 
    .[-1,] %>% # delete first row
    as.data.frame() 
  colnames(t_dflocation) <- t(dflocation[,1]) # countries as columns
  view(t_dflocation)
  visdat::vis_miss(t_dflocation[,1:108]) # first half
  visdat::vis_miss(t_dflocation[,109:216]) # second half
  VIM::aggr(t_dflocation)