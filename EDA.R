library(data.table)
library(ggplot2)
library(dplyr)

# Load data
election_returns <- fread('countypres_2000-2016.csv')
senate_2016 <- fread('2016-precinct-senate.csv')

## What were the pivot counties from 2008-2016?
## What demographics pivoted the most?
## What changes happend on a county-level from 2008-2016
## How did pivot counties vote in the midterms between 2008-2016
## How did pivot counties vote for governors?
## What was turnout like in these pivot counties


# pivot counties: voted Obama twice -> Trump
pivot_counties <- election_returns[year > 2004, 
                                   .SD[which.max(candidatevotes)], 
                                   by = .(year, state, county)]

pivot_counties <- pivot_counties[, 
                         .(vote_history = list(candidate)), 
                         by = .(county, state)]

x <- pivot_counties[vote_history == c('Barack Obama', 'Barack Obama', 'Donald Trump')]
