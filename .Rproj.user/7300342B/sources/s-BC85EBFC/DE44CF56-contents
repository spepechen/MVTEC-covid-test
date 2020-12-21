library(dplyr)
library(tidyverse)
library(countrycode)


ddExtra <- readxl::read_xlsx("./data/InfoPaisosExtra.xlsx", na = "NA")
dd <- read.csv("./data/owid-covid-data.csv",header=T)


countrycode('Albania', origin = 'country.name', destination = 'iso3c')

ddExtra2 <- ddExtra %>% 
  mutate(iso_code = countrycode(COUNTRY, origin = 'country.name', destination = 'iso3c')) %>% 
  remove_rownames %>% 
  column_to_rownames(var="COUNTRY")

# XK for Kosovo
# GB-CHA for Channel Islands
# https://simple.wikipedia.org/wiki/ISO_3166-2:GB

ddExtra2["Kosovo", "iso_code"] = 'XK'
ddExtra2["Channel Islands", "iso_code"] = 'GB-CHA'

# set rowname back to column
ddExtra2 <- tibble::rownames_to_column(ddExtra2, "COUNTRY")

# reorder columns
ddExtra2 <- ddExtra2[c(1,11,2, 3, 4, 5, 6, 7, 8, 9, 10)]

View(ddExtra2)
# write.csv(ddExtra2,'output/ddExtra2.csv')

joined <- left_join(dd, ddExtra2, by="iso_code") 
write.csv(joined, 'output/joined_by_isocode.csv')
  

# Data type --------------------------------------------------------
sapply(joined, class)

# ordered factors/ ordinal categorical variables
# unordered factors/ nominal categorical variables

joined <- joined %>% 
  mutate(Continent = factor(Continent),
         Country_Clasification = factor(Country_Clasification), 
         Government_Type = factor(Government_Type), 
         Corruption_preception = factor(Corruption_preception),
         Development_Status = factor(Development_Status, ordered=TRUE, c( "Developed economies","Transition economies","Developing economies")), 
         Land_Conditions =  factor(Land_Conditions))

levels(joined$Continent) 
levels(joined$Country_Clasification) 
levels(joined$Government_Type) 
levels(joined$Corruption_preception)
levels(joined$Development_Status)  # ordered 
levels(joined$Land_Conditions)


joined$date <- as.Date(joined$date, format='%Y-%m-%d')

save(joined, file="output/joined_data_by_isocode.RData")

