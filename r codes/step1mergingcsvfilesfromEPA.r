# ──────────────────────────────────────────────────────────────
# 0.  Packages
# ──────────────────────────────────────────────────────────────
library(tidyverse)

# ──────────────────────────────────────────────────────────────
# 1.  File locations / year range
# ──────────────────────────────────────────────────────────────
data_dir <- "C:/Users/ayatawara/Documents/Research/Research Fall 2024/EPA daily data extraction/epa_daily_csv_files"
years    <- 2000:2023
files    <- file.path(data_dir, sprintf("daily_42101_%d.csv", years))

# ──────────────────────────────────────────────────────────────
# 2.  Helper: read one file → keep CA rows → keep only needed cols
# ──────────────────────────────────────────────────────────────
read_co <- function(file) {
  readr::read_csv(file, col_types = cols(.default = col_guess())) %>% 
    filter(`State Name` == "California") %>% 
    transmute(
      date   = as.Date(`Date Local`, format = "%Y-%m-%d"),
      county = str_trim(`County Name`),
      co     = `Arithmetic Mean`
    )
}

# ──────────────────────────────────────────────────────────────
# 3.  Read all years and row-bind
# ──────────────────────────────────────────────────────────────
co_ca <- purrr::map_dfr(files, read_co)

# ──────────────────────────────────────────────────────────────
# 4.  Collapse to one value per (county, date)
# ──────────────────────────────────────────────────────────────
co_ca <- co_ca %>% 
  group_by(county, date) %>% 
  summarise(
    co      = mean(co, na.rm = TRUE),
    n_sites = n(),
    .groups = "drop"
  ) %>% 
  arrange(date, county)

# ──────────────────────────────────────────────────────────────
# 5.  Save
# ──────────────────────────────────────────────────────────────
readr::write_csv(co_ca, file.path(data_dir, "co_california_2000_2023.csv"))
