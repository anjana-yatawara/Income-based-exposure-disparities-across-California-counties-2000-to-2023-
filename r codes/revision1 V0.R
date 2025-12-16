# ================================================================
#   pm25_california_2000_2023.csv   (county, date, pm2_5, n_sites)
#   no2_california_2000_2023.csv    (county, date, no2,   n_sites)
#   ozone_california_2000_2023.csv  (county, date, ozone, n_sites)
#   so2_california_2000_2023.csv    (county, date, so2,   n_sites)
#   co_california_2000_2023.csv     (county, date, co,    n_sites)
#   Pop_2000_to_2023.csv
#   cali_income.csv
# ================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(scales)
  library(ggplot2)
  library(nlme)
  library(sandwich)  # Newey-West / HAC
  library(lmtest)    # coeftest with HAC vcov
})


# ---------- helpers ----------
wm <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

clean_county <- function(x) {
  x |>
    str_remove(",\\s*California$") |>
    str_remove("\\s+County$") |>
    str_remove("^.*?\\.") |>
    str_trim()
}

# ---------- income groups using your exact approach ----------
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

# ---------- population long table exactly like your code ----------
pop <- readr::read_csv("Pop_2000_to_2023.csv", show_col_types = FALSE) %>% 
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

# ---------- per pollutant config (exact column names you gave) ----------
polls <- tibble::tribble(
  ~id,    ~file,                              ~value_col, ~ylab,               ~ydelta,
  "PM2.5","pm25_california_2000_2023.csv",    "pm2_5",    "PM2.5 (µg/m³)",     "Delta PM2.5 (µg/m³)",
  "NO2",  "no2_california_2000_2023.csv",     "no2",      "NO2 (ppb)",         "Delta NO2 (ppb)",
  "O3",   "ozone_california_2000_2023.csv",   "ozone",    "O3 (ppb)",          "Delta O3 (ppb)",
  "SO2",  "so2_california_2000_2023.csv",     "so2",      "SO2 (ppb)",         "Delta SO2 (ppb)",
  "CO",   "co_california_2000_2023.csv",      "co",       "CO (ppm)",          "Delta CO (ppm)"
)

dir.create("outputs", showWarnings = FALSE)

all_disparities <- list()
all_trends      <- list()


for (k in seq_len(nrow(polls))) {
  id        <- polls$id[k]
  fpath     <- polls$file[k]
  value_col <- polls$value_col[k]
  ylab      <- polls$ylab[k]
  ydelta    <- polls$ydelta[k]
  
  out_dir <- file.path("outputs", id)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("=== Processing ", id, " from ", fpath, " ===")
  
  # Read pollutant CSV
  pol <- readr::read_csv(fpath, show_col_types = FALSE)
  stopifnot(all(c("county","date", value_col) %in% names(pol)))
  
  # Join to income groups
  pol_strat <- pol %>% 
    left_join(mhi %>% select(County, income_grp),
              by = c("county" = "County")) %>% 
    filter(!is.na(income_grp))
  
  # Join population by year
  pol_wpop <- pol_strat %>% 
    mutate(year = lubridate::year(date)) %>% 
    left_join(pop, by = c("county", "year"))
  
  stopifnot(!anyNA(pol_wpop$pop))
  
  # Population weighted daily series by group
  pol_wpop <- pol_wpop %>% rename(val = !!rlang::sym(value_col))
  
  pwt_daily <- pol_wpop %>% 
    group_by(date, income_grp) %>% 
    summarise(
      val = weighted.mean(val, w = pop, na.rm = TRUE) %>% replace_na(NA_real_),
      .groups = "drop"
    ) %>% 
    pivot_wider(names_from = income_grp, values_from = val) %>% 
    mutate(disparity = high - low)
  
  pwt_daily_complete <- pwt_daily %>% 
    filter(!is.na(low) & !is.na(mid) & !is.na(high)) %>% 
    mutate(disparity = high - low)
  
  # ---------- Plot 1: daily series with shading ----------
  ann_pct <- pwt_daily_complete %>%
    mutate(date = as.Date(date),
           year = year(date),
           low_gt_high = low > high) %>% 
    group_by(year) %>% 
    summarise(pct_low_gt_high = mean(low_gt_high) * 100,
              .groups = "drop")
  
  col_low   <- "#e6194B"
  col_high  <- "#0571b0"
  fill_blue <- "#64b5f6"
  
  shade <- ann_pct %>% 
    mutate(start = as.Date(make_date(year, 1, 1)),
           end   = as.Date(make_date(year, 12, 31)),
           alpha = rescale(pct_low_gt_high, to = c(0.05, 0.30)))
  
  pwt_daily_complete <- pwt_daily_complete %>%
    mutate(date = as.Date(date))
  
  ymax <- max(c(pwt_daily_complete$low, pwt_daily_complete$high), na.rm = TRUE)
  
  p_series <- ggplot() +
    geom_rect(data = shade,
              aes(xmin = start, xmax = end,
                  ymin = -Inf,  ymax =  Inf,
                  alpha = alpha),
              fill = fill_blue) +
    scale_alpha_identity() +
    geom_line(data = pwt_daily_complete,
              aes(date, low , colour = "Low-income"),  linewidth = .4) +
    geom_line(data = pwt_daily_complete,
              aes(date, high, colour = "High-income"), linewidth = .4) +
    scale_colour_manual(values = c("Low-income"  = col_low,
                                   "High-income" = col_high),
                        name = NULL) +
    geom_text(data = ann_pct,
              aes(x = make_date(year, 7, 1),
                  y = ymax * 1.04,
                  label = sprintf("%.0f%%", pct_low_gt_high)),
              size = 4.2, fontface = "bold", colour = "grey20") +
    scale_x_date(breaks = make_date(unique(ann_pct$year), 1, 1),
                 date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.08))) +
    labs(
      title = paste0("Daily population-weighted ", id, " by income group (California)"),
      subtitle = "Blue shade shows percent of days low-income exceeds high-income per year",
      x = NULL,
      y = ylab,
      caption = "Population weights from county-year population"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position   = "top",
      legend.text       = element_text(size = 11, face = "bold"),
      plot.title        = element_text(face = "bold"),
      plot.margin       = margin(6, 60, 6, 6)
    )
  
  ggsave(file.path(out_dir, paste0(id, "_daily_series_2000_2023.pdf")),
         plot = p_series, width = 9, height = 4.8, units = "in")
  
  # ---------- Plot 2: daily disparity (low - high) ----------
  diff_daily <- pwt_daily_complete |>
    transmute(
      date,
      diff = low - high
    ) |>
    mutate(date = as.Date(date))
  
  p_diff <- ggplot(diff_daily, aes(date, diff)) +
    geom_line(colour = "#08306B", linewidth = 0.4) +
    geom_hline(yintercept = 0, colour = "red", linewidth = 0.8) +
    scale_x_date(
      breaks      = make_date(seq(2000, 2023, 2), 1, 1),
      date_labels = "%Y",
      expand      = expansion(mult = c(0.01, 0.02))
    ) +
    labs(
      title = paste0("Population-weighted daily ", id, " difference (low - high)"),
      x     = NULL,
      y     = ydelta
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 14,
        margin = margin(b = 8)
      ),
      plot.margin = margin(20, 36, 6, 6)
    )
  
  ggsave(file.path(out_dir, paste0(id, "_daily_disparity_2000_2023.pdf")),
         plot = p_diff, width = 9, height = 3.8, units = "in")
  
  # ---------- Annual disparities and CSV ----------
  annual_means <- pwt_daily_complete %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    summarise(
      y_low  = mean(low ,  na.rm = TRUE),
      y_high = mean(high,  na.rm = TRUE),
      .groups = "drop"
    )
  
  disp_year <- annual_means %>% 
    mutate(
      abs_disp = y_low  - y_high,
      rel_disp = y_low  / y_high
    ) %>% 
    select(year, abs_disp, rel_disp)
  
  pct_days <- pwt_daily_complete %>%
    mutate(year = year(date), low_gt_high = low > high) %>%
    group_by(year) %>%
    summarise(pct_days_low_gt_high = mean(low_gt_high) * 100, .groups = "drop")
  
  disp_year <- disp_year %>% left_join(pct_days, by = "year")
  
  write_csv(disp_year, file.path(out_dir, paste0("annual_disparities_low_vs_high_", id, "_2000_2023.csv")))
  all_disparities[[id]] <- disp_year %>% mutate(pollutant = id, .before = 1)
  
  # ---------- Trend models: mixed-effects, OLS-HAC, full-series GLS ----------
  dat <- within(pwt_daily_complete, {
    date      <- as.Date(date)
    year      <- year(date)
    doy       <- yday(date)
    disparity <- low - high
  })
  dat <- dat[is.finite(dat$disparity), ]
  dat <- dat[order(dat$year, dat$doy), ]
  dat$year_c <- dat$year - mean(dat$year, na.rm = TRUE)
  
  ## 3.1 Primary mixed-effects model with within-year ARMA
  fit_ar1 <- lme(
    disparity ~ year_c +
      sin(2 * pi * doy / 365.25) +
      cos(2 * pi * doy / 365.25),
    data = dat,
    random = ~ 1 | year,
    correlation = corAR1(form = ~ doy | year),
    method = "REML",
    control = lmeControl(
      msMaxIter    = 150,
      msMaxEval    = 200,
      tolerance    = 1e-5,
      returnObject = TRUE
    )
  )
  aic_ar1 <- AIC(fit_ar1)
  
  fit_arma11 <- try(
    lme(
      disparity ~ year_c +
        sin(2 * pi * doy / 365.25) +
        cos(2 * pi * doy / 365.25),
      data = dat,
      random = ~ 1 | year,
      correlation = corARMA(p = 1, q = 1, form = ~ doy | year),
      method = "REML",
      control = lmeControl(
        msMaxIter    = 150,
        msMaxEval    = 200,
        tolerance    = 1e-5,
        returnObject = TRUE
      )
    ),
    silent = TRUE
  )
  
  best_mixed    <- fit_ar1
  best_mixed_lab <- "AR(1)"
  if (!inherits(fit_arma11, "try-error")) {
    aic_arma11 <- AIC(fit_arma11)
    if ((aic_ar1 - aic_arma11) > 2) {
      best_mixed     <- fit_arma11
      best_mixed_lab <- "ARMA(1,1)"
    }
  }
  
  tt_mixed <- summary(best_mixed)$tTable
  b1_mixed <- unname(tt_mixed["year_c", "Value"])
  se_mixed <- unname(tt_mixed["year_c", "Std.Error"])
  df_mixed <- unname(tt_mixed["year_c", "DF"])
  p_mixed  <- 2 * pt(abs(unname(tt_mixed["year_c", "t-value"])),
                     df = df_mixed, lower.tail = FALSE)
  ci_mixed <- b1_mixed + c(-1, 1) * qt(0.975, df = df_mixed) * se_mixed
  
  trend_mixed <- tibble(
    pollutant      = id,
    method         = "mixed_within_year",
    corr_structure = best_mixed_lab,
    beta1          = b1_mixed,
    ci_low         = ci_mixed[1],
    ci_high        = ci_mixed[2],
    p_value        = p_mixed
  )
  
  ## 3.2 OLS with Newey-West HAC standard errors
  lm_hac <- lm(
    disparity ~ year_c +
      sin(2 * pi * doy / 365.25) +
      cos(2 * pi * doy / 365.25),
    data = dat
  )
  
  vcov_hac <- vcovHAC(lm_hac)
  co_hac   <- coeftest(lm_hac, vcov. = vcov_hac)
  
  b1_hac <- unname(co_hac["year_c", "Estimate"])
  se_hac <- unname(co_hac["year_c", "Std. Error"])
  p_hac  <- unname(co_hac["year_c", "Pr(>|t|)"])
  ci_hac <- b1_hac + c(-1, 1) * qnorm(0.975) * se_hac
  
  trend_hac <- tibble(
    pollutant      = id,
    method         = "OLS_HAC",
    corr_structure = "HAC",
    beta1          = b1_hac,
    ci_low         = ci_hac[1],
    ci_high        = ci_hac[2],
    p_value        = p_hac
  )
  
  ## 3.3 Full-series GLS AR(1)/ARMA(1,1) on numeric date
  dat$date_num <- as.numeric(dat$date)
  
  fit_full_ar1 <- gls(
    disparity ~ year_c +
      sin(2 * pi * doy / 365.25) +
      cos(2 * pi * doy / 365.25),
    data = dat,
    correlation = corAR1(form = ~ date_num),
    method = "REML"
  )
  aic_full_ar1 <- AIC(fit_full_ar1)
  
  fit_full_arma11 <- try(
    gls(
      disparity ~ year_c +
        sin(2 * pi * doy / 365.25) +
        cos(2 * pi * doy / 365.25),
      data = dat,
      correlation = corARMA(p = 1, q = 1, form = ~ date_num),
      method = "REML"
    ),
    silent = TRUE
  )
  
  best_full     <- fit_full_ar1
  best_full_lab <- "AR(1)"
  if (!inherits(fit_full_arma11, "try-error")) {
    aic_full_arma11 <- AIC(fit_full_arma11)
    if ((aic_full_ar1 - aic_full_arma11) > 2) {
      best_full     <- fit_full_arma11
      best_full_lab <- "ARMA(1,1)"
    }
  }
  
  tt_full <- summary(best_full)$tTable
  b1_full <- unname(tt_full["year_c", "Value"])
  se_full <- unname(tt_full["year_c", "Std.Error"])
  df_full <- unname(tt_full["year_c", "DF"])
  p_full  <- 2 * pt(abs(unname(tt_full["year_c", "t-value"])),
                    df = df_full, lower.tail = FALSE)
  ci_full <- b1_full + c(-1, 1) * qt(0.975, df = df_full) * se_full
  
  trend_full <- tibble(
    pollutant      = id,
    method         = "GLS_full_series",
    corr_structure = best_full_lab,
    beta1          = b1_full,
    ci_low         = ci_full[1],
    ci_high        = ci_full[2],
    p_value        = p_full
  )
  
  ## 3.4 Collect all methods for this pollutant
  trend_all_methods <- bind_rows(trend_mixed, trend_hac, trend_full)
  
  write_csv(
    trend_all_methods,
    file.path(out_dir, paste0("trend_", id, "_all_methods.csv"))
  )
  all_trends[[id]] <- trend_all_methods
  

# ---------- combine across pollutants ----------
  # ---------- combine across pollutants ----------
  disp_all <- dplyr::bind_rows(all_disparities)
  write_csv(disp_all,
            file.path("outputs", "disparities_all_pollutants_2000_2023.csv"))
  
  trend_all <- dplyr::bind_rows(all_trends)
  
  # BH adjustment across the five pollutant tests for the primary mixed model
  trend_primary <- trend_all %>%
    filter(method == "mixed_within_year") %>%
    arrange(pollutant) %>%
    mutate(q_value = p.adjust(p_value, method = "BH")) %>%
    select(pollutant, q_value)
  
  # Attach q_values to all rows (same q per pollutant, NA if not primary)
  trend_all <- trend_all %>%
    left_join(trend_primary, by = "pollutant")
  
  write_csv(
    trend_all,
    file.path("outputs", "trend_all_pollutants_all_methods.csv")
  )
  
  trend_primary_only <- trend_all %>%
    filter(method == "mixed_within_year")
  
  write_csv(
    trend_primary_only,
    file.path("outputs", "trend_all_pollutants_primary.csv")
  )
  
years_pick <- c(2000L, 2010L, 2023L)
disp_selected <- disp_all %>%
  filter(year %in% years_pick) %>%
  relocate(pollutant, year) %>%
  arrange(pollutant, year)

write_csv(disp_selected, file.path("outputs", "disparities_selected_years.csv"))

print(trend_all)
print(disp_selected)
message("Done. See ./outputs/<POLLUTANT>/ and combined tables in ./outputs")
