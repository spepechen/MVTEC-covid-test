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
  
    # Column `test_units` 
    
      test_units <- factor(dd$tests_units)
      levels(test_units)
      # [1] ""                                "people tested"                  
      # [3] "people tested (incl. non-PCR)"   "samples tested"                 
      # [5] "tests performed"                 "tests performed (incl. non-PCR)"
      # [7] "units unclear"  
      
    
      # recoding levels in `test_units`
      newvalues <- c("unknown", "people_PCR", "people_All", "samples", "performed_PCR", "performed_All","unclear")
      test_units <- newvalues[match(test_units,levels(test_units))] # declare recoded levels in factor
      
    # propagate levels
      dd[,34] <- test_units

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
    
    # RECODING LEVELS
    # Column Continent 
      Continent <- factor(ddExtra$Continent)
      levels(Continent)
      # [1] "Africa"  "America" "Asia"    "Europe"  "Oceania"
      
    # Column Country_Clasification 
      Country_classification <- factor(ddExtra$Country_Clasification)
      levels(Country_Clasification)
      # [1] "BRICS" "NC"    "OECD" 
      
    # Column Government_Type 
      GovType <- factor(ddExtra$Government_Type)
      levels(GovType)
      # [1] "Absolut monarchy"                       "Communist state"                       
      # [3] "Constitutional monarchy"                "Dictatorship"                          
      # [5] "In Transition (provisional government)" "Islamic Parlamentary Republic"         
      # [7] "Islamic Presidential Republic"          "Islamic Semipresidential Republic"     
      # [9] "Parlamentary republic"                  "Presidential limited democracy"        
      # [11] "Presidential republic"                  "Semipresidential republic"   
      
      # recoding levels in Government_type
      newvalues <- c("AbsMonar", "Communist", "ConstMonar", "Dictatorship", "Transition", "IslParRep","IslPreRep","IslSemPreRep","ParRep","PreLimDemo","PreRep","SemPreRep")
      GovType <- newvalues[match(GovType,levels(GovType))] # declare recoded levels in factor
    
    # Column Corruption_preception  
      Corruption_preception <- factor(ddExtra$Corruption_preception)
      levels(Corruption_preception)
      # [1] "Highly corrupt" "Less corrupt"   "NI"  
      
      newvalues <- c("high", "low", "unknown")
      Corruption_preception <- newvalues[match(Corruption_preception,levels(Corruption_preception))] # declare recoded levels in factor
      
    # Column Development_Status 
      Development_Status  <- factor(ddExtra$Development_Status)
      levels(Development_Status)
      # [1] "Developed economies"  "Developing economies" "Transition economies"
      
      newvalues <- c("developed", "developing", "transition")
      Development_Status <- newvalues[match(Development_Status,levels(Development_Status))] # declare recoded levels in factor
      
    # Column Land_Conditions       
      Land_Conditions <- factor(ddExtra$Land_Conditions)
      levels(Land_Conditions)
      # [1] "Islands"    "Landlocked" "Sea Access"
      newvalues <- c("islands", "landlocked", "sea_access")
      Land_Conditions <- newvalues[match(Land_Conditions,levels(Land_Conditions))] # declare recoded levels in factor
      
    # Propagate new levels 
      ddExtra[,2] <- Continent
      ddExtra[,3] <- Country_Clasification
      ddExtra[,4] <- GovType
      ddExtra[,5] <- Corruption_preception
      ddExtra[,6] <- Development_Status
      ddExtra[,7] <- Land_Conditions
      
# DATA DROP -----------------------------------
    # Quick calculation of 'total population' differences between datasets to drop the least updated
    CountryPopul <- aggregate(ddExtra$Population_total, by = list(factor(ddExtra$COUNTRY)), FUN=mean)
    dd_CountryPopul <- aggregate(dd$population, by = list(factor(dd$location)), FUN=mean)
    
    Popul_diff <- CountryPopul %>%
      left_join(dd_CountryPopul,by = "Group.1")
    
    Popul_diff$diff <- Popul_diff$x.x - Popul_diff$x.y
    Popul_diff$percent <-scales::percent( (Popul_diff$x.x - Popul_diff$x.y)/Popul_diff$x.x )
    
    # head(Popul_diff)
    #                            Group.1        x.x        x.y      diff    percent
    # 1                      Afghanistan   33370794   38928341  -5557547 -16.65392%
    # 2                          Albania    2889104    2877800     11304   0.39126%
    # 3                          Algeria   38923687   43851043  -4927356 -12.65902%
    # 4                   American Samoa      55791         NA        NA       <NA>
    # 5                          Andorra      79213      77265      1948   2.45919%
    # 6                           Angola   26941779   32866268  -5924489 -21.98997%
    # ...
    
    # Drop population from country extra data
    ddExtra <- ddExtra %>% select(-Population_total)
    
    #TODO: Should we also drop `Urban_population_PC_of_total_population` as it might be calculated with the totals we drop?
    
# DATA JOIN -----------------------------------
    # Antijoin will make visible mismatching country 'strings' between datasets
    missmatched_countries <- dd %>% anti_join(ddExtra, by = c("location" = "COUNTRY"))
    missmatched_countriesExtra <- ddExtra %>% anti_join(dd, by = c("COUNTRY" = "location"))
    
    # Show names not matching in ddExtra$COUNTRY
    levels(factor(missmatched_countriesExtra$COUNTRY))
    
    # Show names not matching in dd$location
    levels(factor(missmatched_countries$location))
    
    ## UNMATCHED country names in ddExtra
    # [1] "American Samoa"                 "Bahamas, The"                   "Brunei Darussalam"             
    # [4] "Cabo Verde"                     "Channel Islands"                "Congo, Dem. Rep."              
    # [7] "Congo, Rep."                    "Egypt, Arab Rep."               "Eswatini"                      
    # [10] "Faroe Islands"                  "Gambia, The"                    "Hong Kong SAR, China"          
    # [13] "Iran, Islamic Rep."             "Kiribati"                       "Korea, Dem. People’s Rep."     
    # [16] "Korea, Rep."                    "Kyrgyz Republic"                "Lao PDR"                       
    # [19] "Macao SAR, China"               "Micronesia, Fed. Sts."          "Nauru"                         
    # [22] "North Macedonia"                "Palau"                          "Russian Federation"            
    # [25] "Samoa"                          "Slovak Republic"                "St. Kitts and Nevis"           
    # [28] "St. Lucia"                      "St. Martin (French part)"       "St. Vincent and the Grenadines"
    # [31] "Syrian Arab Republic"           "Timor-Leste"                    "Tonga"                         
    # [34] "Turkmenistan"                   "Tuvalu"                         "Venezuela, RB"                 
    # [37] "Virgin Islands (U.S.)"          "West Bank and Gaza / Palestine" "Yemen, Rep."                   
    
    ## UNMATCHED country names in dd
    # [1] "Anguilla"                         "Bahamas"                          "Bonaire Sint Eustatius and Saba" 
    # [4] "Brunei"                           "Cape Verde"                       "Congo"                           
    # [7] "Democratic Republic of Congo"     "Egypt"                            "Faeroe Islands"                  
    # [10] "Falkland Islands"                 "Gambia"                           "Guernsey"                        
    # [13] "Hong Kong"                        "International"                    "Iran"                            
    # [16] "Jersey"                           "Kyrgyzstan"                       "Laos"                            
    # [19] "Macedonia"                        "Montserrat"                       "Palestine"                       
    # [22] "Russia"                           "Saint Kitts and Nevis"            "Saint Lucia"                     
    # [25] "Saint Vincent and the Grenadines" "Slovakia"                         "South Korea"                     
    # [28] "Swaziland"                        "Syria"                            "Taiwan"                          
    # [31] "Timor"                            "United States Virgin Islands"     "Vatican"                         
    # [34] "Venezuela"                        "Wallis and Futuna"                "Western Sahara"                  
    # [37] "World"                            "Yemen"                          
    
    # Rename COUNTRY strings to match main dataset
    ddExtra$COUNTRY <- gsub("Bahamas, The", "Bahamas", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Brunei Darussalam", "Brunei", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Cabo Verde", "Cape Verde", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Congo, Rep.", "Congo", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Congo, Dem. Rep.", "Democratic Republic of Congo", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Egypt, Arab Rep.", "Egypt", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Faroe Islands", "Faeroe Islands", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Gambia, The", "Gambia", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Hong Kong SAR, China", "Hong Kong", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Iran, Islamic Rep.", "Iran", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Kyrgyz Republic", "Kyrgyzstan", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Lao PDR", "Laos", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Korea, Dem. People’s Rep.", "South Korea", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("North Macedonia", "Macedonia", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Russian Federation", "Russia", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("St. Kitts and Nevis", "Saint Kitts and Nevis", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("St. Lucia", "Saint Lucia", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("St. Vincent and the Grenadines", "Saint Vincent and the Grenadines", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Slovak Republic", "Slovakia", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Syrian Arab Republic", "Syria", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Timor-Leste", "Timor", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Virgin Islands (U.S.)", "United States Virgin Islands", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Venezuela, RB", "Venezuela", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("West Bank and Gaza / Palestine", "Palestine", ddExtra$COUNTRY)
    ddExtra$COUNTRY <- gsub("Yemen, Rep.", "Yemen", ddExtra$COUNTRY)
    
    # YET UNMATCHED
    # from covid data
    # [1] "Anguilla"                        "Bonaire Sint Eustatius and Saba" "Falkland Islands"               
    # [4] "Guernsey"                        "International"                   "Jersey"                         
    # [7] "Montserrat"                      "Swaziland"                       "Taiwan"                         
    # [10] "United States Virgin Islands"    "Vatican"                         "Wallis and Futuna"              
    # [13] "Western Sahara"                  "World"        
    
    # from excel country data
    # [1] "American Samoa"           "Channel Islands"          "Eswatini"                 "Kiribati"                
    # [5] "Korea, Rep."              "Macao SAR, China"         "Micronesia, Fed. Sts."    "Nauru"                   
    # [9] "Palau"                    "Samoa"                    "St. Martin (French part)" "Tonga"                   
    # [13] "Turkmenistan"             "Tuvalu"                   "Virgin Islands (U.S.)" 
   
    # TODO: `Guersney` and `Jersey` could pull data from `Channel Islands` 
    # TODO: Not sure why Virgin Islands still don't match
    
    # Merge data
    merged_dataset <- dd %>%
      left_join(ddExtra, by = c("location" = "COUNTRY")) %>%
      write.csv('./output/merged_data.csv')
    
    
    
    
    