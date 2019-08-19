library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)

# Load data
election_returns <- fread('countypres_2000-2016.csv')
senate_2016 <- fread('2016-precinct-senate.csv')

## What were the pivot counties from 2008-2016?
## What demographics pivoted the most?
## What changes happend on a county-level from 2008-2016
## How did pivot counties vote in the midterms between 2008-2016
## How did pivot counties vote for governors?
## What was turnout like in these pivot counties


GetPivotCounties <- function(DT) {
  pivot_counties <- DT[year > 2004,
                       .SD[which.max(candidatevotes)],
                       by = .(year, state, county)]
  
  pivot_counties <- dcast(pivot_counties, state + county ~ candidate, length)
  
  names(pivot_counties) <- str_replace_all(names(pivot_counties), c(" " = "."))
  
  pivot_counties <- pivot_counties[Barack.Obama == 2 & Donald.Trump == 1][
    , .(state, county)
  ]
  
  return(pivot_counties)
}


pivot_counties <- GetPivotCounties(election_returns)


senate_2016[, .(state, jurisdiction)]

