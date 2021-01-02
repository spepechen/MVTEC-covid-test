library(tidyverse)
# library(plotly)

# LOAD DATA | DROP NON WANTED DATA---- 
df0 <-readRDS("output/merged_data.RDS") %>% as.data.frame()  
df <- df0 %>%
  select(
    !contains('weekly') & # Drop weekly data columns
    !ends_with('thousand')) %>% # Drop per_thousand columns
  filter(location != "International" & location != "World" )  # Drop non-countries in location.
  
# EASING VARIABLE SELECTION ----
# simplify variable selection `select()` 

# COUNTRY DATA
# `scan()` is used to split a string "...,..." into items c("...","...")

demography <- scan(text="population, population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,life_expectancy, human_development_index, urban_pop, gdp_energy",
                    what="character",
                    sep=",",
                    strip.white=T)
country_categorical <- scan(text="continent, country_class, gov_type, corruption, dev_status, land_conditions",
                         what="character",
                         sep=",",
                         strip.white=T)

# STUDY DATA
# usage: df %>% select( per_million() ,country_categoric )
per_million <- function(x) contains('million') 
smoothed <- function(x) contains('smooth')
tests <- function(x) contains('test')
totals <- function(x) contains('total')
hospitals_icu <- function(x) contains('patient')

# EXAMPLE USAGE:  
# df %>% select(location, smoothed(), tests()) %>% names()
#       [1] "location"                        "new_cases_smoothed"              "new_deaths_smoothed"            
#       [4] "new_cases_smoothed_per_million"  "new_deaths_smoothed_per_million" "new_tests_smoothed"             
#       [7] "total_tests"                     "new_tests"                       "tests_per_case"                 
#       [10] "tests_units"      



# COLUMN GROUPS AND AGGREGATION CRITERIA ----
# BY DATE AND LOCATION
# Function: SUM
    # Raw data main stats
        # new_cases, new_deaths
    # Per million
        # new_cases_per_million, new_deaths_per_million
    # Smoothed
        # new_cases_smoothed, new_deaths_smoothed, 
    # Smoothed per million
        # new_cases_smoothed_per_million, new_deaths_smoothed_per_million
    # Tests
        # new_tests
    # (only if aggregated by country)
      # Demography
        # population
      
# Function: MAX
    # Raw data  
        # total_cases, total_deaths,
    # Per million
        # total_cases_per_million, total_deaths_per_million
    # Tests
        # total_tests
   
# Function: MEAN 
    # Tests  
        # tests_per_case, positive_rate
    # Hospital and icu variables are values for a specific date
        # icu_patients, hosp_patients
        # icu_patients_per_million, hosp_patients_per_million
    # Rates
        # reproduction_rate, stringency_index
    # Demographics
        # population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,life_expectancy, human_development_index, urban_pop, gdp_energy

# CATEGORICAL
    # Tests    
        # tests_units
    # Country data
        # continent, country_class, gov_type, corruption, dev_status, land_condition

agg_sum <- scan(text="new_tests, new_cases_smoothed_per_million, new_deaths_smoothed_per_million, new_cases_smoothed, new_deaths_smoothed, new_cases_per_million, new_deaths_per_million,new_cases, new_deaths",
               what="character",
               sep=",",
               strip.white=T)
agg_max <- scan(text="total_cases, total_deaths,total_cases_per_million, total_deaths_per_million,total_cases, total_deaths, total_tests, total_cases, total_deaths",
               what="character",
               sep=",",
               strip.white=T)
agg_mean <- scan(text="population_density, median_age, aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities,life_expectancy, human_development_index, urban_pop, gdp_energy, reproduction_rate, stringency_index, icu_patients_per_million, hosp_patients_per_million, icu_patients, hosp_patients, tests_per_case, positive_rate",
                what="character",
                sep=",",
                strip.white=T)

categ <- scan(text="test_units, continent, country_class, gov_type, corruption, dev_status, land_condition",
              what="character",
              sep=",",
              strip.white=T)
 
# GROUPING DATA  -----------------------------------
# Monthly          
  dfmonthly <- df %>% 
    group_by(location,month=lubridate::floor_date(date,"month")) %>% 
    mutate(month=format(month, "%Y - %b")) %>%
    summarize(
      across(agg_max, max),
      across(agg_sum, sum),
      across(agg_mean, mean))
    # mutate_if(is.numeric,function(x) coalesce(x,0L)) %>%
                
  dfweekly <- df %>% as.data.frame() %>%
    group_by(location,week=lubridate::floor_date(date,"week")) %>% 
  #  mutate(year_week=format("week", "%b %d"))  %>%
    select(-date) %>% 
    summarize(
      across(agg_max, max),
      across(agg_sum, sum),
      across(agg_mean, mean))
    # mutate_if(is.numeric,function(x) coalesce(x,0L)) %>%
    
  dflocation_permillion_totals <- df %>%
    group_by(iso_code, location) %>%
    summarize(
      across(agg_max, max),
      across(agg_sum, sum),
      across(agg_mean, mean)) %>%
    select(iso_code,location,per_million() & !starts_with("new"))
       mutate_if(is.numeric,function(x) coalesce(x,0L)) 

      # across(contains("new") | contains('population'), ~ sum(.x,na.rm=T))  %>%

  
    # Transpose data by location
  t_dflocation <- t(dflocation) %>% .[-1,] # delete first row
  colnames(t_dflocation) <- t(dflocation[,1]) # countries as columns
  

# MISSING VALUES ANALYSIS  -----------------------------------
  # Uncomment both lines below if packages are not installed 
    # libraries_used <- c("visdat", "VIM")
    # install.packages(librariesused)
  
  # Plot missing values Use grouped data to ease visualization
  visdat::vis_miss(dfweekly)
    # Observations:
      # Weekly data is mostly missing
      # Hospital data is also scarce (ICU, patients...)
      # Data about `new_tests` also shows around 60% missing data
    
    # Other data with missing patterns
      # `total_deaths`, `total_deaths_per_million`
      # `reproduction_rate`
      # `stringency_index`
      # `extreme_poverty`
      # 'workers by gender demographics'
      # `handwashing_facilities`        
    
  # Transpose data by location
  colnames(t_dflocation) <- t(dflocation[,1]) # countries as columns
  
  
  visdat::vis_miss(as.data.frame(t_dflocation[,1:108])) # first half
  visdat::vis_miss(as.data.frame(t_dflocation[,109:216])) # second half
  na_matrix <- summary(VIM::aggr(dfweekly,cex.axis=.6, oma = c(13,0,3,0)))$combinations
  
# pivot weeks on countries  -----------------------------------
  weekly_nwdeaths_sm <- dfweekly %>%
    pivot_wider(id_cols=c('location','new_deaths_smoothed'),
                names_from='year_week',
                values_from='new_deaths_smoothed',
                values_fill=0)
  
  dflocation  %>% arrange(desc(total_deaths_per_million)) %>% slice_head(n = 50)

               
               
# Misc: ----
  library(gapminder)
  gap_with_colors <-
    data.frame(df,
               cc = I(country_colors[match(df$location,
                                           names(country_colors))]))
  
  # bubble plot, focus just on Africa and Europe in 2007
  keepers <- with(gap_with_colors,
                  continent %in% c("Africa", "Europe") & date == '2020-10-01')
  plot(life_expectancy ~ total_deaths_per_million, gap_with_colors,
       subset = keepers, log = "x", pch = 21,
       cex = sqrt(gap_with_colors$pop[keepers]/pi)/1500,
       bg = gap_with_colors$cc[keepers])
  
## Max value 
  # Total values (accumulators)
df %>% select(starts_with('total')) %>% colnames()
  # [1] "total_cases"              "total_deaths"             "total_cases_per_million" 
  # [4] "total_deaths_per_million" "total_tests"              "total_tests_per_thousand"
 
## Sum (smoothed new reported values contain no irregularity)
df %>% select(!starts_with('new')) %>% colnames()
   # [1] "new_cases_smoothed"  "new_deaths_smoothed" "new_tests_smoothed" 


p <- df %>% filter(iso_code == 'ESP') %>% ggplot(aes(x = date)) +
  geom_point(aes(text = paste("Country:", location)), size = 4) +
  geom_smooth(aes(colour = vars(), fill = vars())) +
  facet_grid(cols= vars(df))

  
  

