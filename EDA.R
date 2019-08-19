library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)

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


GetPivotCounties <- function(election_returns, county_unemployment) {
  
  # winning candidate for each county
  pivot_counties <- election_returns[year > 2004,
                       .SD[which.max(candidatevotes)],
                       by = .(year, state, county)]
  
  pivot_counties <- dcast(pivot_counties, state + county + FIPS ~ candidate, length)
  
  names(pivot_counties) <- str_replace_all(names(pivot_counties), c(" " = "."))
  
  pivot_counties <- pivot_counties[Barack.Obama == 2 & Donald.Trump == 1][
    , .(state, county, FIPS)]
  
  # join pivot counties with county unemployment
  setkeyv(pivot_counties, 'FIPS')
  setkeyv(county_unemployment, 'FIPS')
  
  pivot_counties <- merge(pivot_counties, county_unemployment, all = FALSE)
  
  # remove unnecessary columns
  pivot_counties[, c('State', 'Area_name', 'Rural_urban_continuum_code_2013', 'Urban_influence_code_2013', 'Metro_2013') := NULL]
  
  return(pivot_counties)
}


pivot_counties <- GetPivotCounties(election_returns, county_unemployment)







