rm(list = ls())

library(tidyverse)
library(DBI)
library(odbc)
library(eagxlsx)
library(readxl)
library(openxlsx)

datasets_path <- "datasets_full.xlsx"

datasets <- read.xlsx(datasets_path)



# check 1 -----------------------------------------------------------------

unique(datasets$Population)

# [1] "Not present in the metadata"
# [2] "Refugees, People of concern"
# [3] "People of concern"
# [4] "People of concern, Individuals"
# [5] "Refugees, People of concern, Individuals"
# [6] "Refugees"
# [7] "Refugees, IDPs, Host communities, Returnees" --> directly support comparative analysis
# [8] "Host-community members"
# [9] "Asylum seekers, Refugees, Individuals"
# [10] "IDPs, Returnees"
# [11] "Asylum seekers, Refugees, Stateless, IDPs, Host communities" --> directly support comparative analysis
# [12] "Stateless"
# [13] "Refugees, IDPs, Returnees"
# [14] "Refugees, Individuals"

dateset_filtered1 <- datasets |>
  filter(Population %in% c("Refugees, IDPs, Host communities, Returnees", "Asylum seekers, Refugees, Stateless, IDPs, Host communities"))
# --> only two datasets have exactly both populations

datasets |>
  count(Population == "Not present in the metadata")


# check 2 -----------------------------------------------------------------

# Count how many distinct population types each country has
country_pop_counts <- datasets |>
  group_by(Country) |>
  summarise(population_types = list(unique(Population)),
            num_types = n_distinct(Population)) |>
  arrange(desc(num_types)) |>
  filter(num_types > 1)

# View top candidates
print(country_pop_counts, n = 20)



# check 3 -----------------------------------------------------------------

# Define population keywords
host_keywords <- c("Host", "host")
displaced_keywords <- c("Refugees", "IDPs", "Asylum seekers", "People of concern", "Stateless")

# Find matching entries
datasets_filtered <- datasets |>
  filter(grepl(paste(c(host_keywords, displaced_keywords), collapse = "|"), Population, ignore.case = TRUE))

# Now see which countries appear more than once
datasets_filtered |>
  count(Country, sort = TRUE) |>
  filter(n > 1)

