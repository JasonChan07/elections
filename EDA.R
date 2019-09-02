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
## How large were the pivots (percentage swing)
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
unemployment_rate <- c('FIPS',
                       'state',
                       'county',
                       'pivot_indicator',
                       'Civilian_labor_force_2007', 
                       'Unemployed_2007', 
                       'Civilian_labor_force_2008', 
                       'Unemployed_2008',
                       'Civilian_labor_force_2009', 
                       'Unemployed_2009',
                       'Civilian_labor_force_2010', 
                       'Unemployed_2010',
                       'Civilian_labor_force_2011', 
                       'Unemployed_2011',
                       'Civilian_labor_force_2012', 
                       'Unemployed_2012',
                       'Civilian_labor_force_2013', 
                       'Unemployed_2013',
                       'Civilian_labor_force_2014', 
                       'Unemployed_2014',
                       'Civilian_labor_force_2015', 
                       'Unemployed_2015',
                       'Civilian_labor_force_2016', 
                       'Unemployed_2016',
                       'Civilian_labor_force_2017', 
                       'Unemployed_2017',
                       'Civilian_labor_force_2018', 
                       'Unemployed_2018'
                       )

counties_ue_wide <- counties[, unemployment_rate, with = FALSE]

# convert data from wide to long
counties_ue_long <- melt(counties_ue_wide, id.vars = c('FIPS', 'state', 'county', 'pivot_indicator'), measure.vars = c(''))
  
# remove non-numeric characters from year column
counties_ue_long[, year := gsub("\\D+", "", year)]

# plot county unemployment rates

