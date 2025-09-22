# ================================================================
# Summary table: mean annual population-weighted exposure by income group
# Columns are period averages of annual means:
#   2000–2005, 2005–2010, 2010–2015, 2015–2020, 2020–2023
# Assumption: intervals are half-open [start, end), except the last is [2020, 2023] inclusive.
# Outputs:
#   outputs/summary_group_means_by_period_all_pollutants.csv
# ================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(lubridate); library(scales); library(purrr)
})

dir.create("outputs", showWarnings = FALSE)

# ---------- helpers ----------
clean_county <- function(x) {
  x |>
    str_remove(",\\s*California$") |>
    str_remove("\\s+County$") |>
    str_remove("^.*?\\.") |>
    str_trim()
}

wm <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# ---------- income groups (your exact approach) ----------
mhi_raw <- read_csv("cali_income.csv", show_col_types = FALSE)

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

# ---------- population (your exact approach) ----------
pop <- read_csv("Pop_2000_to_2023.csv", show_col_types = FALSE) %>% 
  rename(raw_name = 1) %>% 
  filter(str_detect(raw_name, "County,\\s*California")) %>% 
  mutate(
    county = raw_name |>
      str_remove(",\\s*California$") |>
      str_remove("\\s+County$") |>
      str_remove("^.*?\\.") |>
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

stopifnot(all(is.finite(pop$pop)))

# ---------- pollutant configs (exact columns you gave) ----------
polls <- tibble::tribble(
  ~id,    ~file,                              ~value_col,
  "PM2.5","pm25_california_2000_2023.csv",    "pm2_5",
  "NO2",  "no2_california_2000_2023.csv",     "no2",
  "O3",   "ozone_california_2000_2023.csv",   "ozone",
  "SO2",  "so2_california_2000_2023.csv",     "so2",
  "CO",   "co_california_2000_2023.csv",      "co"
)

# ---------- time windows ----------
# Half-open [start, end) except last is inclusive at end.
periods <- tibble::tribble(
  ~label,        ~start, ~end, ~inclusive_end,
  "2000-2005",   2000L,  2005L, FALSE,
  "2005-2010",   2005L,  2010L, FALSE,
  "2010-2015",   2010L,  2015L, FALSE,
  "2015-2020",   2015L,  2020L, FALSE,
  "2020-2023",   2020L,  2023L, TRUE
)

assign_period <- function(y) {
  # returns the label for year y based on periods above
  row <- periods %>%
    filter(
      ( y >= start & y < end & !inclusive_end ) |
        ( y >= start & y <= end &  inclusive_end )
    )
  if (nrow(row) == 1) row$label else NA_character_
}

# ---------- core builder for one pollutant ----------
build_group_annual_means <- function(fpath, value_col) {
  pol <- read_csv(fpath, show_col_types = FALSE)
  stopifnot(all(c("county", "date", value_col) %in% names(pol)))
  
  pol_strat <- pol %>% 
    left_join(mhi %>% select(County, income_grp),
              by = c("county" = "County")) %>% 
    filter(!is.na(income_grp))
  
  pol_wpop <- pol_strat %>% 
    mutate(year = lubridate::year(date)) %>% 
    left_join(pop, by = c("county", "year"))
  
  stopifnot(!anyNA(pol_wpop$pop))
  
  pol_wpop <- pol_wpop %>% rename(val = !!rlang::sym(value_col))
  
  # daily population-weighted means per group
  pwt_daily <- pol_wpop %>% 
    group_by(date, income_grp) %>% 
    summarise(
      val = wm(val, pop),
      .groups = "drop"
    ) %>% 
    tidyr::pivot_wider(names_from = income_grp, values_from = val)
  
  # keep days where all three groups present (consistent with your earlier workflow)
  pwt_daily_complete <- pwt_daily %>% 
    filter(!is.na(low) & !is.na(mid) & !is.na(high))
  
  # annual means per group
  annual_grp <- pwt_daily_complete %>% 
    mutate(year = year(as.Date(date))) %>% 
    group_by(year) %>% 
    summarise(
      low  = mean(low,  na.rm = TRUE),
      mid  = mean(mid,  na.rm = TRUE),
      high = mean(high, na.rm = TRUE),
      .groups = "drop"
    )
  
  annual_grp
}

# ---------- run for all pollutants and aggregate to periods ----------
sum_list <- vector("list", length = nrow(polls))

for (k in seq_len(nrow(polls))) {
  id        <- polls$id[k]
  fpath     <- polls$file[k]
  value_col <- polls$value_col[k]
  
  message("Summarizing: ", id)
  
  ann <- build_group_annual_means(fpath, value_col)
  
  # map years to period labels
  ann$period <- vapply(ann$year, assign_period, character(1))
  ann <- ann %>% filter(!is.na(period))
  
  # average annual means within each period for each group
  by_period <- ann %>%
    group_by(period) %>%
    summarise(
      low  = mean(low,  na.rm = TRUE),
      mid  = mean(mid,  na.rm = TRUE),
      high = mean(high, na.rm = TRUE),
      .groups = "drop"
    )
  
  # reshape to requested layout: rows = income group, cols = periods
  wide <- by_period %>%
    pivot_longer(cols = c(low, mid, high), names_to = "income_group", values_to = "mean_exposure") %>%
    mutate(income_group = factor(income_group, levels = c("low", "mid", "high"))) %>%
    arrange(income_group, period) %>%
    pivot_wider(names_from = period, values_from = mean_exposure)
  
  wide <- wide %>% mutate(pollutant = id, .before = 1)
  
  sum_list[[k]] <- wide
}

summary_all <- bind_rows(sum_list) %>%
  arrange(pollutant, income_group)

# Optional rounding for readability (keep more decimals if you want raw)
summary_all_rounded <- summary_all %>%
  mutate(across(-c(pollutant, income_group), ~ round(.x, 3)))

# write combined CSV
out_csv <- file.path("outputs", "summary_group_means_by_period_all_pollutants.csv")
write_csv(summary_all_rounded, out_csv)
message("✓ Wrote ", out_csv)

# If you also want separate CSVs per pollutant, uncomment:
# split(summary_all_rounded, summary_all_rounded$pollutant) |>
#   imap(~ write_csv(.x, file.path("outputs", paste0("summary_", .y, "_group_means_by_period.csv"))))
