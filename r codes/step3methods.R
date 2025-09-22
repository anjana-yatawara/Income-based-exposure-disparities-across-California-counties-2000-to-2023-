library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(scales)


#daily diparity and annual percentage of high exposure in low income groups

# ───────────────────────────────────────────────────────────────
# 1.  Annual % days low > high
# ───────────────────────────────────────────────────────────────
ann_pct_co <- cowt_daily_complete %>%
  mutate(date = as.Date(date),
         year = year(date),
         low_gt_high = low > high) %>% 
  group_by(year) %>% 
  summarise(pct_low_gt_high = mean(low_gt_high) * 100,
            .groups = "drop")

# ───────────────────────────────────────────────────────────────
# 2.  Base colours
# ───────────────────────────────────────────────────────────────
col_low   <- "#e6194B"
col_high  <- "#0571b0"
fill_blue <- "#64b5f6"

# ───────────────────────────────────────────────────────────────
# 3.  Shading
# ───────────────────────────────────────────────────────────────
shade_co <- ann_pct_co %>% 
  mutate(start = as.Date(make_date(year, 1, 1)),
         end   = as.Date(make_date(year, 12, 31)),
         alpha = rescale(pct_low_gt_high, to = c(0.05, 0.30)))

# ───────────────────────────────────────────────────────────────
# 4.  Ensure proper date formats
# ───────────────────────────────────────────────────────────────
cowt_daily_complete <- cowt_daily_complete %>%
  mutate(date = as.Date(date))



# ───────────────────────────────────────────────────────────────
# 5.  Plot base with shading and CO series
# ───────────────────────────────────────────────────────────────
p_co <- ggplot() +
  geom_rect(data = shade_co,
            aes(xmin = start, xmax = end,
                ymin = -Inf,  ymax =  Inf,
                alpha = alpha),
            fill = fill_blue) +
  scale_alpha_identity() +
  geom_line(data = cowt_daily_complete,
            aes(date, low , colour = "Low-income"),  linewidth = .4) +
  geom_line(data = cowt_daily_complete,
            aes(date, high, colour = "High-income"), linewidth = .4) +
  scale_colour_manual(values = c("Low-income"  = col_low,
                                 "High-income" = col_high),
                      name = NULL) +
  geom_text(data = ann_pct_co,
            aes(x = make_date(year, 7, 1),
                y = max(c(cowt_daily_complete$low,
                          cowt_daily_complete$high), na.rm = TRUE) * 1.04,
                label = sprintf("%.0f%%", pct_low_gt_high)),
            size = 4.2, fontface = "bold", colour = "grey20") +
  scale_x_date(breaks = make_date(unique(ann_pct_co$year), 1, 1),
               date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.08))) +
  labs(
    title = "Daily population-weighted CO by income group (California)",
    subtitle = "Blue shade = % of days low-income > high-income • Numbers show annual % (2000–2023)",
    x = NULL,
    y = "CO (ppm)",
    caption = "Counties ≥ 90 % data completeness; population = California DOF"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "top",
    legend.text       = element_text(size = 11, face = "bold"),
    plot.title        = element_text(face = "bold"),
    plot.margin       = margin(6, 60, 6, 6)
  )

print(p_co)


#disparity series
library(ggplot2)
library(lubridate)

# 1. Difference series -------------------------------------------------
diff_daily_co <- cowt_daily_complete |>
  transmute(
    date,
    diff = low - high
  ) |>
  mutate(date = as.Date(date))   # fix: ensure date is a Date object

# 2. Plot --------------------------------------------------------------
p_diff_simple_co <- ggplot(diff_daily_co, aes(date, diff)) +
  geom_line(colour = "#08306B", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 0.8) +
  scale_x_date(
    breaks      = make_date(seq(2000, 2023, 2), 1, 1),
    date_labels = "%Y",
    expand      = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "Population-weighted daily CO difference (low - high income)",
    x     = NULL,
    y     = "Delta CO (ppm)"
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

print(p_diff_simple_co)


# ───────────────────────────────────────────────────────────────
# 1.  Annual population-weighted means  (low & high) ------------
# ───────────────────────────────────────────────────────────────
annual_means <- cowt_daily_complete %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(
    y_low  = mean(low ,  na.rm = TRUE),   # Ȳ_low,t^(p)
    y_high = mean(high, na.rm = TRUE),   # Ȳ_high,t^(p)
    .groups = "drop"
  )

# ───────────────────────────────────────────────────────────────
# 2.  Absolute (Δ) & Relative (R) disparities  ------------------
# ───────────────────────────────────────────────────────────────
disp_year <- annual_means %>% 
  mutate(
    abs_disp = y_low  - y_high,  # Δ_t^(p)  (µg/m³)
    rel_disp = y_low  / y_high   # R_t^(p)  (ratio)
  ) %>% 
  select(year, abs_disp, rel_disp)

print(disp_year, n = Inf)        # show all years in console

# ───────────────────────────────────────────────────────────────
# 3.  Save to CSV  ----------------------------------------------
# ───────────────────────────────────────────────────────────────
write_csv(disp_year, "annual_disparities_low_vs_high_CO_2000_2023.csv")

cat("✓ CSV written to 'annual_disparities_low_vs_high_CO_2000_2023.csv'\n")

library(ggplot2)
library(readr)   # if you read disp_year from disk

# If disp_year is already in memory, skip this line
# disp_year <- read_csv("annual_disparities_low_vs_high_CO_2000_2023.csv")

# ───────────────────────────────────────────────────────────────
# 1.  Absolute disparity plot  (µg/m³) ---------------------------
# ───────────────────────────────────────────────────────────────
p_abs <- ggplot(disp_year, aes(year, abs_disp)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(colour = "#d73027", linewidth = 1) +
  geom_point(colour = "#d73027", size = 2) +
  labs(
    title = "Annual absolute disparity (low – high) in population-weighted CO",
    x = "Year", y = "Absolute disparity (µg/m³)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ───────────────────────────────────────────────────────────────
# 2.  Relative disparity plot  (ratio) ---------------------------
# ───────────────────────────────────────────────────────────────
p_rel <- ggplot(disp_year, aes(year, rel_disp)) +
  geom_hline(yintercept = 1, colour = "grey50") +
  geom_line(colour = "#1a9850", linewidth = 1) +
  geom_point(colour = "#1a9850", size = 2) +
  labs(
    title = "Annual relative disparity (low / high) in population-weighted CO",
    x = "Year", y = "Relative disparity (ratio)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Display
print(p_abs)
print(p_rel)

# ───────────────────────────────────────────────────────────────
# 3.  Save each plot as PDF  ------------------------------------
# ───────────────────────────────────────────────────────────────
ggsave("CO_abs_disparity_2000_2023.pdf", plot = p_abs,
       width = 7, height = 4.5, units = "in")

ggsave("CO_rel_disparity_2000_2023.pdf", plot = p_rel,
       width = 7, height = 4.5, units = "in")


#mixed effects model AR 1 and ARMA11 

# Fast model + one-step ARMA(1,1) check
library(nlme)
library(lubridate)

stopifnot(all(c("date","low","high") %in% names(cowt_daily_complete)))

# Prepare data
dat <- within(cowt_daily_complete, {
  date <- as.Date(date)
  year <- year(date)
  doy  <- yday(date)
  disparity <- low - high
})
dat <- dat[is.finite(dat$disparity), ]
dat <- dat[order(dat$year, dat$doy), ]
dat$year_c <- dat$year - mean(dat$year, na.rm = TRUE)

# 1) Primary model: LME with AR(1) residuals within year
fit_ar1 <- lme(
  disparity ~ year_c + sin(2 * pi * doy / 365.25) + cos(2 * pi * doy / 365.25),
  data = dat,
  random = ~ 1 | year,
  correlation = corAR1(form = ~ doy | year),
  method = "REML",
  control = lmeControl(msMaxIter = 150, msMaxEval = 200, tolerance = 1e-5,
                       returnObject = TRUE)
)
aic_ar1 <- AIC(fit_ar1)

# 2) Quick alternative: LME with ARMA(1,1) residuals
fit_arma11 <- try(
  lme(
    disparity ~ year_c + sin(2 * pi * doy / 365.25) + cos(2 * pi * doy / 365.25),
    data = dat,
    random = ~ 1 | year,
    correlation = corARMA(p = 1, q = 1, form = ~ doy | year),
    method = "REML",
    control = lmeControl(msMaxIter = 150, msMaxEval = 200, tolerance = 1e-5,
                         returnObject = TRUE)
  ),
  silent = TRUE
)

use_ar1 <- TRUE
best_model <- fit_ar1
best_lab   <- "AR(1)"
if (!inherits(fit_arma11, "try-error")) {
  aic_arma11 <- AIC(fit_arma11)
  # Switch only if improvement is meaningful and residual ACF suggested MA
  if ((aic_ar1 - aic_arma11) > 2) {
    best_model <- fit_arma11
    best_lab   <- "ARMA(1,1)"
  }
}

cat(sprintf("Selected residual structure: %s\n", best_lab))
print(summary(best_model))

# Trend estimate: yearly change in disparity (units per year)
tt <- summary(best_model)$tTable
b1 <- unname(tt["year_c","Value"])
se <- unname(tt["year_c","Std.Error"])
df <- unname(tt["year_c","DF"])
p  <- 2 * pt(abs(unname(tt["year_c","t-value"])), df = df, lower.tail = FALSE)
ci <- b1 + c(-1,1) * qt(0.975, df = df) * se

cat(sprintf("\nYearly trend: %.6f  95%% CI [%.6f, %.6f]  p=%.4g\n", b1, ci[1], ci[2], p))

# Optional quick diagnostics
# acf(resid(best_model, type = "normalized"), lag.max = 30, main = paste("ACF residuals -", best_lab))

