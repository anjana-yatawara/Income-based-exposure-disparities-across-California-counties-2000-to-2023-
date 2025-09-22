library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(scales)


co <- read.csv("co_california_2000_2023.csv")

# ───────────────────────────────────────────────────────────────
# C.  Read & tidy the MHI file, keep only those good counties
# ───────────────────────────────────────────────────────────────
mhi_raw <- readr::read_csv("cali_income.csv", show_col_types = FALSE)

mhi <- mhi_raw %>% 
  filter(!County %in% c("United States", "California")) %>% 
  mutate(
    County = str_remove(County, "\\s+County$") |> str_trim(),
    mhi    = `Value (Dollars)`
  ) %>% 
  select(County, FIPS, mhi)

q30 <- quantile(mhi$mhi, 0.30, na.rm = TRUE)
q70 <- quantile(mhi$mhi, 0.70, na.rm = TRUE)

mhi <- mhi %>% 
  mutate(income_grp = case_when(
    mhi <= q30 ~ "low",
    mhi <= q70 ~ "mid",
    TRUE       ~ "high"
  ))

co_strat <- co %>% 
  left_join(mhi %>% select(County, income_grp),
            by = c("county" = "County")) %>% 
  filter(!is.na(income_grp))        # drops only counties lacking MHI data

# ───────────────────────────────────────────────────────────────
# 6.  Population series (2000–2023) → clean → long → join
# ───────────────────────────────────────────────────────────────

pop <- readr::read_csv("Pop_2000_to_2023.csv", show_col_types = FALSE) %>% 
  rename(raw_name = 1) %>% 
  filter(str_detect(raw_name, "County,\\s*California")) %>% 
  
  mutate(
    county = raw_name |>
      str_remove(",\\s*California$") |>      # drop trailing ', California'
      str_remove("\\s+County$") |>           # drop trailing ' County'
      str_remove("^.*?\\.")   |>             # NEW: drop everything through the first dot
      str_trim()
  ) %>% 
  
  select(-raw_name) %>% 
  
  pivot_longer(
    cols      = -county,
    names_to  = "year",
    values_to = "pop"
  ) %>% 
  
  mutate(
    year = as.integer(str_remove(year, "^X")),
    pop  = as.numeric(str_remove_all(pop, ","))
  )

co_wpop <- co_strat %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(pop, by = c("county", "year"))

stopifnot(!anyNA(co_wpop$pop))  # should pass now

cowt_daily <- co_wpop %>% 
  group_by(date, income_grp) %>% 
  summarise(
    co = weighted.mean(co, w = pop, na.rm = TRUE) %>% 
      replace_na(NA_real_),       # turn NaN → NA
    .groups = "drop"
  ) %>% 
  pivot_wider(names_from = income_grp, values_from = co) %>% 
  mutate(disparity = high - low)       # absolute gap; use ratio if preferred

cowt_daily_complete <- cowt_daily %>% 
  filter(!is.na(low) & !is.na(mid) & !is.na(high)) %>%   # keep fully populated days
  mutate(disparity = high - low)                         # recompute just in case


