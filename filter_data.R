library(tidyverse)
library(data.table)

setwd("/Users/bencullen/Projects/patent_dashboard")
patent <- fread('g_patent_2012_2021.csv')
assignee <- fread('g_assignee_disambiguated_2012_2021.csv')
cpc <- fread('g_cpc_current_2012_2021.csv')

# Convert patent_id to character
cpc$patent_id <- as.character(cpc$patent_id)

# Filter the cpc codes
dt <-
  cpc %>% filter(grepl(
    pattern = 'B60L',
    x = cpc$cpc_group,
    ignore.case = TRUE
  ))

# Merge with patents and assignee
dt <- merge(dt, patent, by = 'patent_id')
dt <- merge(dt, assignee, by = 'patent_id')

fwrite(dt, "filtered_data.csv")
