# This will anonymise the dataset so they analysis can be reproduced
# without compromising the NDAs and the vendors proprietary data.

# Read in packages
library(tidyverse)

# Read in data - actual data used will not be made available due to NDAs
dat <- read_csv('tool_dat.csv')

# Define columns to anonymise
cols_to_anon <- c("company", "isin", "ticker", "metric_name", "tool")

# Function to create consistent anonymised values per column
anonymise_column <- function(vec, prefix) {
  unique_vals <- unique(vec)
  anon_map <- setNames(paste0(prefix, "_", seq_along(unique_vals)), unique_vals)
  unname(anon_map[vec])
}

# Apply anonymisation
dat_anonymised <- dat %>%
  select(-year, -sales) %>%
  mutate(across(
    all_of(cols_to_anon),
    .fns = ~ anonymise_column(.x, cur_column()),
    .names = "{.col}"
  ))

# Write to CSV
write_csv(dat_anonymised, 'dat_anonymised.csv')
