library(tidyverse)

# DATA EXAMINATION -----------------------------------

## main covid dataset from ourworldindata.org && country data in extra excel file .xls data
  # COVID DATASET -------------------------------------- 
  dd <- read.csv("./data/owid-covid-data.csv",header=T)
  
  # validate rows x columns, assign 
  dim(dd)
  n <- dim(dd)[1]
  K <- dim(dd)[2]
  c(n,K)
  
  # validate dataset class
  class(dd)
  names(dd)
      
  # declare factors
  attach(dd)
  
  # identify types and categorical 
  sapply(dd, class)
    # Most of variables are numerical
  
  # convert date type
  dd$date <- as.Date(dd$date, format='%Y-%m-%d')
  class(dd$date)
  
  # COUNTRY DATA -------------------------------------- 
  ddExtra <- readxl::read_xlsx("./data/InfoPaisosExtra.xlsx", na = "NA")
  dim(ddExtra)
  nExtra <- dim(ddExtra)[1]
  KExtra <- dim(ddExtra)[2]
  c(nExtra,KExtra)
  
  # validate dataset class
  class(ddExtra)
  names(ddExtra)
  
  # declare factors
  attach(ddExtra)
  
  # identify types and categorical 
  sapply(ddExtra, class)
      
    # Column Government_Type ----
      GovType <- factor(ddExtra$Government_Type)
      levels(GovType)
      
      # recoding levels in Government_type
      newvalues <- c("AbsMonar", "Communist", "ConstMonar", "Dictatorship", "Transition", "IslParRep","IslPreRep","IslSemPreRep","ParRep","PreLimDemo","PreRep","SemPreRep")
      GovType <- newvalues[match(GovType,levels(GovType))]


# GROUP BY MONTH | LOCATION  -----------------------------------  
  ddmonthly <- dd %>%
    group_by(location,month=lubridate::floor_date(date,"month")) %>% 
    select(-date) %>% 
    summarize_all(min) %>% 
    mutate(year_month=format(month, "%Y - %m"))
  
  ddweekly <- dd %>%
    group_by(location,week=lubridate::floor_date(date,"week")) %>% 
    select(-date) %>% 
    summarize_all(min) %>% 
    mutate(year_week=format(week, "%Y - week %W")) 
  
  ddlocation <- dd %>%
    group_by(location) %>%
    select(-date, -iso_code, -continent) %>%
    summarize_all(max)
    
  # Transpose data by location
  t_ddlocation <- t(ddlocation) %>% .[-1,] # delete first row
  colnames(t_ddlocation) <- t(ddlocation[,1]) # countries as columns
  VIM::aggr(t_ddlocation)
  
# MISSING VALUES ANALYSIS  -----------------------------------
  # Plot missing values Use grouped data to ease visualization
  visdat::vis_miss(ddweekly)
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
  t_ddlocation <- t(ddlocation) %>% 
    .[-1,] %>% # delete first row
    as.data.frame() 
  colnames(t_ddlocation) <- t(ddlocation[,1]) # countries as columns
  visdat::vis_miss(t_ddlocation[,1:108]) # first half
  visdat::vis_miss(t_ddlocation[,109:216]) # second half
    
  
  
  
  
  

  
  
  
  
  
  
  