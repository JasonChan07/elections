library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Load data

## county election returns 2000-2016
election_returns <- fread('countypres_2000-2016.csv')

## county unemployment 2007-2018
county_unemployment <- fread('county-unemployment.csv')

## What were the pivot counties from 2008-2016?
## What demographics pivoted the most?
## What changes happend on a county-level from 2008-2016
## How did pivot counties vote in the midterms between 2008-2016
## How did pivot counties vote for governors?
## What was turnout like in these pivot counties


GetCountyReturns <- function(election_returns, county_unemployment) {
  
  # winning candidate for each county
  pivot_counties <- election_returns[year > 2004,
                       .SD[which.max(candidatevotes)],
                       by = .(year, state, county)]
  
  pivot_counties <- dcast(pivot_counties, state + county + FIPS ~ candidate, length)
  
  names(pivot_counties) <- str_replace_all(names(pivot_counties), c(" " = "."))
  
  # join pivot counties with county unemployment
  setkeyv(pivot_counties, 'FIPS')
  setkeyv(county_unemployment, 'FIPS')
  
  pivot_counties <- merge(pivot_counties, county_unemployment, all = FALSE)
  
  # remove unnecessary columns
  pivot_counties[, c('State',
                     'Area_name',
                     'Rural_urban_continuum_code_2013',
                     'Urban_influence_code_2013',
                     'Metro_2013') := NULL]
  
  # add indicator variable if county is pivot
  pivot_counties[ , pivot_indicator := as.factor(ifelse(Barack.Obama == 2 & Donald.Trump == 1, 1, 0))]
  
  return(pivot_counties)
}


counties <- GetCountyReturns(election_returns, county_unemployment)

# unemployment columns
unemployment_rate <- c('state',
                       'county',
                       'pivot_indicator',
                       'Unemployment_rate_2007', 
                       'Unemployment_rate_2008', 
                       'Unemployment_rate_2009',
                       'Unemployment_rate_2010',
                       'Unemployment_rate_2011',
                       'Unemployment_rate_2012',
                       'Unemployment_rate_2013',
                       'Unemployment_rate_2014',
                       'Unemployment_rate_2015',
                       'Unemployment_rate_2016',
                       'Unemployment_rate_2017',
                       'Unemployment_rate_2018'
                       )

counties_ue <- counties[, unemployment_rate, with = FALSE]

# convert to long
counties_ue_long <- data.table(gather(counties_ue, year, unemployment, Unemployment_rate_2007:Unemployment_rate_2018, factor_key = TRUE))

# remove non-numeric characters from year column
counties_ue_long[, year := gsub("\\D+", "", year)]

# plot county unemployment rates
