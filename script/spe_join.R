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

write.csv(ddExtra2,'output/ddExtra2.csv')
