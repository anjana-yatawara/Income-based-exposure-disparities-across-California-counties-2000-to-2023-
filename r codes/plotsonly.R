# ================================================================
# PLOTS ONLY: population-weighted daily series by income group
# Figures saved under ./plots/<POLLUTANT> as PDF and PNG
# Inputs expected in working directory:
#   pm25_california_2000_2023.csv   (county, date, pm2_5, n_sites)
#   no2_california_2000_2023.csv    (county, date, no2,   n_sites)
#   ozone_california_2000_2023.csv  (county, date, ozone, n_sites)
#   so2_california_2000_2023.csv    (county, date, so2,   n_sites)
#   co_california_2000_2023.csv     (county, date, co,    n_sites)
#   Pop_2000_to_2023.csv
#   cali_income.csv
# ================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(lubridate); library(scales); library(ggplot2)
})

dir.create("plots", showWarnings = FALSE)

# ---------- helpers ----------
wm <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# ---------- income groups (fixed 30/40/30 by reference MHI) ----------
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

# ---------- population (county x year long) ----------
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
  pivot_longer(cols = -county, names_to = "year", values_to = "pop") %>%
  mutate(
    year = as.integer(str_remove(year, "^X")),
    pop  = as.numeric(str_remove_all(pop, ","))
  )

stopifnot(all(is.finite(pop$pop)))

# ---------- pollutant configs (adjust y-labels if needed) ----------
polls <- tibble::tribble(
  ~id,    ~file,                              ~value_col, ~ylab,               ~ydelta,
  "PM2.5","pm25_california_2000_2023.csv",    "pm2_5",    "PM2.5 (µg/m³)",     "Δ PM2.5 (µg/m³)",
  "NO2",  "no2_california_2000_2023.csv",     "no2",      "NO2 (ppb)",         "Δ NO2 (ppb)",
  "O3",   "ozone_california_2000_2023.csv",   "ozone",    "O3 (ppm)",          "Δ O3 (ppm)",
  "SO2",  "so2_california_2000_2023.csv",     "so2",      "SO2 (ppb)",         "Δ SO2 (ppb)",
  "CO",   "co_california_2000_2023.csv",      "co",       "CO (ppm)",          "Δ CO (ppm)"
)

# ---------- loop: build plots per pollutant ----------
for (k in seq_len(nrow(polls))) {
  id        <- polls$id[k]
  fpath     <- polls$file[k]
  value_col <- polls$value_col[k]
  ylab      <- polls$ylab[k]
  ydelta    <- polls$ydelta[k]
  
  out_dir <- file.path("plots", id)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Plotting ", id, " from ", fpath)
  
  # Read pollutant file
  pol <- read_csv(fpath, show_col_types = FALSE)
  if (!all(c("county", "date", value_col) %in% names(pol))) {
    warning("Skipping ", id, " because required columns are missing.")
    next
  }
  
  pol <- pol %>%
    mutate(date = as.Date(date))
  
  # Join income groups and population
  pol_strat <- pol %>%
    left_join(mhi %>% select(County, income_grp),
              by = c("county" = "County")) %>%
    filter(!is.na(income_grp))
  
  pol_wpop <- pol_strat %>%
    mutate(year = year(date)) %>%
    left_join(pop, by = c("county", "year"))
  
  if (anyNA(pol_wpop$pop)) {
    warning("Population join produced NAs for ", id, "; check county names.")
    next
  }
  
  pol_wpop <- pol_wpop %>% rename(val = !!rlang::sym(value_col))
  
  # Population-weighted daily values by group
  pwt_daily <- pol_wpop %>%
    group_by(date, income_grp) %>%
    summarise(val = wm(val, pop), .groups = "drop") %>%
    pivot_wider(names_from = income_grp, values_from = val)
  
  # Keep days where all three groups are present
  pwt_daily_complete <- pwt_daily %>%
    filter(!is.na(low) & !is.na(mid) & !is.na(high)) %>%
    mutate(disparity = low - high)
  
  if (nrow(pwt_daily_complete) == 0) {
    warning("No complete days for ", id, "; skipping.")
    next
  }
  
  # Annual percent of days low > high
  ann_pct <- pwt_daily_complete %>%
    mutate(year = year(date), low_gt_high = low > high) %>%
    group_by(year) %>%
    summarise(pct_low_gt_high = mean(low_gt_high) * 100, .groups = "drop")
  
  # Shading blocks
  shade <- ann_pct %>%
    mutate(
      start = make_date(year, 1, 1),
      end   = make_date(year, 12, 31),
      alpha = rescale(pct_low_gt_high, to = c(0.05, 0.30))
    )
  
  # Colors
  col_low   <- "#e6194B"
  col_high  <- "#0571b0"
  fill_blue <- "#64b5f6"
  
  # Determine sizes
  n_years  <- dplyr::n_distinct(ann_pct$year)
  width_in <- max(16, 0.60 * n_years)  # ~0.6 inch per year; min 16 inches
  height_in <- 5.8
  ymax <- max(c(pwt_daily_complete$low, pwt_daily_complete$high), na.rm = TRUE)
  
  # ----- Plot A: wide daily series with shading and % labels -----
  p_series <- ggplot() +
    geom_rect(data = shade,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, alpha = alpha),
              fill = fill_blue) +
    scale_alpha_identity() +
    geom_line(data = pwt_daily_complete,
              aes(date, low,  colour = "Low-income"),  linewidth = .4) +
    geom_line(data = pwt_daily_complete,
              aes(date, high, colour = "High-income"), linewidth = .4) +
    scale_colour_manual(values = c("Low-income" = col_low, "High-income" = col_high),
                        name = NULL) +
    geom_text(data = ann_pct,
              aes(x = make_date(year, 7, 1), y = ymax * 1.18,
                  label = sprintf("%.0f%%", pct_low_gt_high)),
              size = 5.2, fontface = "bold", colour = "grey20") +
    scale_x_date(breaks = make_date(unique(ann_pct$year), 1, 1),
                 date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.08))) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.28))) +
    labs(
      title = paste0("Daily population-weighted ", id, " by income group (California)"),
      subtitle = "Blue shade shows percent of days low-income exceeds high-income per year",
      x = NULL, y = ylab,
      caption = "Population weights from county-year population"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      legend.text     = element_text(size = 11, face = "bold"),
      plot.title      = element_text(face = "bold"),
      plot.margin     = margin(6, 60, 6, 6),
      text            = element_text(size = 14),
      axis.text.x     = element_text(size = 11)
    )
  
  ggsave(file.path(out_dir, paste0(id, "_daily_series_2000_2023_WIDE.pdf")),
         plot = p_series, width = width_in, height = height_in, units = "in")
  ggsave(file.path(out_dir, paste0(id, "_daily_series_2000_2023_WIDE.png")),
         plot = p_series, width = width_in, height = height_in, units = "in", dpi = 300)
  
  # ----- Plot B: daily disparity (low - high) -----
  p_diff <- ggplot(pwt_daily_complete, aes(date, disparity)) +
    geom_line(colour = "#08306B", linewidth = 0.4) +
    geom_hline(yintercept = 0, colour = "red", linewidth = 0.8) +
    scale_x_date(breaks = make_date(seq(min(ann_pct$year), max(ann_pct$year), 2), 1, 1),
                 date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    labs(
      title = paste0("Population-weighted daily ", id, " difference (low - high)"),
      x = NULL, y = polls$ydelta[k]
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14,
                                margin = margin(b = 8)),
      plot.margin = margin(20, 36, 6, 6)
    )
  
  ggsave(file.path(out_dir, paste0(id, "_daily_disparity_2000_2023.pdf")),
         plot = p_diff, width = max(14, 0.55 * n_years), height = 4.0, units = "in")
  ggsave(file.path(out_dir, paste0(id, "_daily_disparity_2000_2023.png")),
         plot = p_diff, width = max(14, 0.55 * n_years), height = 4.0, units = "in", dpi = 300)
  
  # ----- Plot C: percent-only panel -----
  p_pct <- ggplot(ann_pct, aes(year, pct_low_gt_high)) +
    geom_col(fill = "#64b5f6", width = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", pct_low_gt_high)),
              vjust = -0.35, size = 4.4) +
    scale_y_continuous(limits = c(0, max(ann_pct$pct_low_gt_high) * 1.15)) +
    scale_x_continuous(breaks = unique(ann_pct$year)) +
    labs(title = paste0(id, ": percent of days low-income > high-income"),
         x = NULL, y = "% of days") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title  = element_text(face = "bold")
    )
  
  ggsave(file.path(out_dir, paste0(id, "_annual_percent_low_gt_high.pdf")),
         plot = p_pct, width = max(12, 0.45 * n_years), height = 4.2, units = "in")
  ggsave(file.path(out_dir, paste0(id, "_annual_percent_low_gt_high.png")),
         plot = p_pct, width = max(12, 0.45 * n_years), height = 4.2, units = "in", dpi = 300)
  
  message("✓ Saved plots to ", out_dir)
}

message("Done.")
