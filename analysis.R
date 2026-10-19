

# ---- setup ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})
options(width = 100, digits = 5, scipen = 999)
set.seed(42)

dir.create("figures", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Plot styling: one accent colour, quiet grid, readable labels
pal  <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a")
ink  <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", colour = ink, size = 12),
      plot.title.position = "plot",
      plot.subtitle    = element_text(colour = ink2),
      axis.text        = element_text(colour = ink2),
      axis.title       = element_text(colour = ink2),
      strip.text       = element_text(colour = ink, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
      legend.position  = "bottom",
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}
theme_set(theme_report())

save_fig <- function(name, plot, w = 7, h = 4.2) {
  ggsave(file.path("figures", name), plot, width = w, height = h, dpi = 200)
}

# ---- pipeline_fig ----
steps <- data.frame(
  x = 1:7,
  title = c("Load &\ninspect", "Audit data\nquality", "Impute\nmissing values",
            "Types &\nnew features", "Detect & treat\noutliers",
            "Transform, scale\n& encode", "Exploratory\nanalysis")
)
fig_pipe <- ggplot(steps) +
  geom_rect(aes(xmin = x - 0.44, xmax = x + 0.44, ymin = 0, ymax = 1),
            fill = "#e8f1fc", colour = pal["blue"], linewidth = 0.5) +
  geom_text(aes(x = x, y = 0.74, label = x), colour = pal["blue"],
            fontface = "bold", size = 5) +
  geom_text(aes(x = x, y = 0.33, label = title), colour = ink, size = 2.6,
            lineheight = 0.95) +
  geom_segment(data = data.frame(x = 1:6), aes(x = x + 0.46, xend = x + 0.54,
               y = 0.5, yend = 0.5), colour = ink2, linewidth = 0.5,
               arrow = arrow(length = unit(0.09, "in"), type = "closed")) +
  coord_cartesian(xlim = c(0.45, 7.55), ylim = c(-0.05, 1.05)) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = NA))
save_fig("fig01_pipeline.png", fig_pipe, 7.5, 1.7)

# ---- load_data ----
raw <- read.csv("data/housing.csv", stringsAsFactors = FALSE)
cat("Rows:", nrow(raw), "  Columns:", ncol(raw), "\n\n")
head(raw)

# ---- inspect_structure ----
str(raw)

# ---- inspect_summary ----
summary(raw)

# ---- audit ----
# 1. Missing values per column
miss <- data.frame(
  variable    = names(raw),
  n_missing   = colSums(is.na(raw)),
  pct_missing = round(100 * colMeans(is.na(raw)), 2),
  row.names   = NULL
)
miss

# 2. Duplicates and blank strings
cat("\nDuplicate rows:", sum(duplicated(raw)), "\n")
cat("Blank ocean_proximity values:", sum(trimws(raw$ocean_proximity) == ""), "\n")

# 3. Logical consistency
cat("Block groups with bedrooms > rooms:",
    sum(raw$total_bedrooms > raw$total_rooms, na.rm = TRUE), "\n")
cat("Block groups with households > population:",
    sum(raw$households > raw$population), "\n")
cat("Non-positive counts:",
    sum(raw$total_rooms <= 0 | raw$population <= 0 | raw$households <= 0), "\n")

# 4. Values censored (capped) by the data provider
cat("\nmedian_house_value == 500001 :", sum(raw$median_house_value == 500001), "\n")
cat("housing_median_age == 52     :", sum(raw$housing_median_age == 52), "\n")
cat("median_income == 15.0001     :", sum(raw$median_income == 15.0001), "\n")

# ---- missing_pattern ----
raw$bedrooms_missing <- is.na(raw$total_bedrooms)

# Share of missing values by category
raw %>%
  group_by(ocean_proximity) %>%
  summarise(n = n(), n_missing = sum(bedrooms_missing),
            pct_missing = round(100 * mean(bedrooms_missing), 2))

# Do rows with a missing value differ from complete rows on other variables?
compare_vars <- c("housing_median_age", "total_rooms", "population",
                  "households", "median_income", "median_house_value")
res <- t(sapply(compare_vars, function(v) {
  tt <- t.test(raw[[v]] ~ raw$bedrooms_missing)
  c(mean_complete = unname(tt$estimate[1]),
    mean_missing  = unname(tt$estimate[2]),
    p_value       = tt$p.value)
}))
round(res, 3)

# Where in California are the missing rows?
fig_miss <- ggplot(raw, aes(longitude, latitude)) +
  geom_point(colour = "#cfcdc6", size = 0.4, alpha = 0.5) +
  geom_point(data = filter(raw, bedrooms_missing),
             colour = unname(pal["orange"]), size = 1.3) +
  coord_quickmap() +
  labs(title = "Missing total_bedrooms values are scattered across the state",
       subtitle = "Orange = 207 block groups with a missing value; grey = all others",
       x = "Longitude", y = "Latitude")
save_fig("fig02_missing_map.png", fig_miss, 6.2, 5.2)

# ---- imputation_compare ----
# Hide 10% of the KNOWN values, impute them four ways, and score each method
obs_idx  <- which(!is.na(raw$total_bedrooms))
test_idx <- sample(obs_idx, size = round(0.10 * length(obs_idx)))
train    <- raw[setdiff(obs_idx, test_idx), ]
test     <- raw[test_idx, ]
truth    <- test$total_bedrooms

pred_global <- rep(median(train$total_bedrooms), nrow(test))
grp_med     <- tapply(train$total_bedrooms, train$ocean_proximity, median)
pred_group  <- unname(grp_med[test$ocean_proximity])
pred_ratio  <- median(train$total_bedrooms / train$total_rooms) * test$total_rooms
fit_lm      <- lm(log(total_bedrooms) ~ log(total_rooms) + log(households) +
                    log(population) + ocean_proximity, data = train)
pred_lm     <- exp(predict(fit_lm, newdata = test))

score <- function(pred) c(RMSE = sqrt(mean((pred - truth)^2)),
                          MAE  = mean(abs(pred - truth)),
                          MdAE = median(abs(pred - truth)))
results <- rbind(
  "Global median"          = score(pred_global),
  "Median by ocean group"  = score(pred_group),
  "Median bedroom ratio"   = score(pred_ratio),
  "Regression (log-scale)" = score(pred_lm)
)
round(results, 1)
cat("\nRegression R-squared on log scale:",
    round(summary(fit_lm)$r.squared, 3), "\n")

res_long <- as.data.frame(results[, c("RMSE", "MAE")]) %>%
  tibble::rownames_to_column("method") %>%
  pivot_longer(-method, names_to = "metric", values_to = "error") %>%
  mutate(method = factor(method, levels = rev(rownames(results))))
fig_imp <- ggplot(res_long, aes(error, method, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = round(error, 0)), position = position_dodge(width = 0.75),
            hjust = -0.2, size = 3, colour = ink) +
  scale_fill_manual(values = c(RMSE = unname(pal["blue"]), MAE = unname(pal["orange"]))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Regression imputation recovers hidden values best",
       subtitle = "Error on 2,043 deliberately hidden total_bedrooms values (lower is better)",
       x = "Error (number of bedrooms)", y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE))
save_fig("fig03_imputation_methods.png", fig_imp, 7, 3.6)

# ---- impute ----
fit_final <- lm(log(total_bedrooms) ~ log(total_rooms) + log(households) +
                  log(population) + ocean_proximity,
                data = raw[obs_idx, ])
miss_idx <- which(is.na(raw$total_bedrooms))
imputed  <- exp(predict(fit_final, newdata = raw[miss_idx, ]))

housing <- raw
housing$total_bedrooms[miss_idx] <- round(imputed)
names(housing)[names(housing) == "bedrooms_missing"] <- "bedrooms_imputed"

cat("Missing values after imputation:", sum(is.na(housing$total_bedrooms)), "\n")
cat("Imputed bedrooms exceeding total rooms:",
    sum(housing$total_bedrooms > housing$total_rooms), "\n\n")
rbind(observed = summary(raw$total_bedrooms[obs_idx]),
      imputed  = summary(housing$total_bedrooms[miss_idx]))

# ---- types_and_flags ----
# Nominal variable -> factor. INLAND becomes the reference level for modelling.
housing$ocean_proximity <- factor(
  housing$ocean_proximity,
  levels = c("INLAND", "<1H OCEAN", "NEAR OCEAN", "NEAR BAY", "ISLAND")
)
# Flag the rows whose target was capped by the census bureau
housing$value_capped <- housing$median_house_value >= 500001

levels(housing$ocean_proximity)
table(housing$ocean_proximity)
table(housing$value_capped)

# ---- features ----
# Block-group totals depend on block-group size; per-household ratios do not.
housing <- housing %>%
  mutate(rooms_per_household      = total_rooms / households,
         bedrooms_per_room        = total_bedrooms / total_rooms,
         population_per_household = population / households)

summary(housing[, c("rooms_per_household", "bedrooms_per_room",
                    "population_per_household")])

# ---- outliers_detect ----
num_vars <- c("housing_median_age", "total_rooms", "total_bedrooms",
              "population", "households", "median_income",
              "median_house_value", "rooms_per_household",
              "bedrooms_per_room", "population_per_household")

detect <- function(x) {
  q   <- quantile(x, c(0.25, 0.75)); iqr <- q[2] - q[1]
  lo  <- q[1] - 1.5 * iqr;           hi  <- q[2] + 1.5 * iqr
  out <- x < lo | x > hi
  z   <- (x - mean(x)) / sd(x)
  c(lower_fence = unname(lo), upper_fence = unname(hi), n_iqr = sum(out),
    pct_iqr = 100 * mean(out), n_z3 = sum(abs(z) > 3), max = max(x))
}
outlier_tbl <- round(t(sapply(housing[num_vars], detect)), 2)
outlier_tbl

# The most extreme block groups, to judge whether they are errors or real
housing %>%
  arrange(desc(rooms_per_household)) %>%
  select(rooms_per_household, total_rooms, households, population,
         median_house_value, ocean_proximity) %>%
  head(5)

fig_box <- housing %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(gsub("_", " ", name), levels = gsub("_", " ", num_vars))) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "#cde2fb", colour = ink2, outlier.size = 0.5,
               outlier.alpha = 0.25, outlier.colour = unname(pal["orange"]),
               linewidth = 0.4) +
  facet_wrap(~name, scales = "free_y", ncol = 5, labeller = label_wrap_gen(14)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Most count variables have long upper tails",
       subtitle = "Boxes show the IQR; orange points fall outside the 1.5 x IQR fences",
       y = NULL) +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 7))
save_fig("fig04_outlier_boxplots.png", fig_box, 7.5, 4.6)

# ---- outliers_treat ----
# Winsorise: pull values beyond the 1st / 99th percentile back to that limit.
winsorize <- function(x, p = c(0.01, 0.99)) {
  q <- quantile(x, p); pmin(pmax(x, q[1]), q[2])
}
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3

wins_vars <- c("total_rooms", "total_bedrooms", "population", "households",
               "rooms_per_household", "bedrooms_per_room",
               "population_per_household")

housing_clean <- housing
housing_clean[wins_vars] <- lapply(housing[wins_vars], winsorize)

effect <- data.frame(
  skew_before = sapply(housing[wins_vars], skew),
  skew_after  = sapply(housing_clean[wins_vars], skew),
  max_before  = sapply(housing[wins_vars], max),
  max_after   = sapply(housing_clean[wins_vars], max)
)
round(effect, 2)
cat("\nRows kept:", nrow(housing_clean), "of", nrow(housing), "\n")

show_vars <- c("population", "total_rooms", "rooms_per_household")
fig_win <- bind_rows(
  housing[show_vars]       %>% mutate(stage = "Before (raw)"),
  housing_clean[show_vars] %>% mutate(stage = "After (winsorised)")
) %>%
  pivot_longer(-stage) %>%
  mutate(stage = factor(stage, levels = c("Before (raw)", "After (winsorised)")),
         name  = factor(name, levels = show_vars)) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 50, fill = unname(pal["blue"]), colour = "white",
                 linewidth = 0.1) +
  facet_wrap(vars(stage, name), scales = "free", ncol = 3) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Winsorising removes the extreme tail but keeps every row",
       subtitle = "Each panel has its own axes, so note the shorter x-range in the bottom row",
       x = NULL, y = "Block groups") +
  theme(strip.text = element_text(size = 8))
save_fig("fig05_winsorise_effect.png", fig_win, 7.5, 4.6)

# ---- transform_scale ----
features_num <- c("housing_median_age", "total_rooms", "total_bedrooms",
                  "population", "households", "median_income",
                  "rooms_per_household", "bedrooms_per_room",
                  "population_per_household")

# Rule: log-transform a variable only if it is clearly right-skewed (skewness > 1)
# AND the transform actually brings skewness closer to zero.
skew_cmp <- data.frame(
  skew_raw = round(sapply(housing_clean[features_num], skew), 2),
  skew_log = round(sapply(housing_clean[features_num],
                          function(x) skew(log1p(x))), 2)
)
skew_cmp$transform <- skew_cmp$skew_raw > 1 &
  abs(skew_cmp$skew_log) < abs(skew_cmp$skew_raw)
skew_cmp

skewed <- rownames(skew_cmp)[skew_cmp$transform]
cat("\nLog-transformed:", paste(skewed, collapse = ", "), "\n")

trans <- housing_clean[features_num]
trans[skewed] <- lapply(trans[skewed], log1p)
names(trans)[names(trans) %in% skewed] <- paste0("log_", skewed)

# Two scalers: min-max to [0, 1], and z-score (mean 0, sd 1)
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
mm <- as.data.frame(lapply(trans, minmax))
zs <- as.data.frame(scale(trans))

cat("\nZ-score check (mean / sd):\n")
round(rbind(mean = colMeans(zs), sd = apply(zs, 2, sd))[, 1:5], 3)
cat("\nMin-max check (min / max):\n")
round(rbind(min = apply(mm, 2, min), max = apply(mm, 2, max))[, 1:5], 3)

stages <- bind_rows(
  tibble(value = housing_clean$total_rooms,      stage = "1. Winsorised"),
  tibble(value = log1p(housing_clean$total_rooms), stage = "2. Log-transformed"),
  tibble(value = zs$log_total_rooms,             stage = "3. Log + z-score")
)
fig_scale <- ggplot(stages, aes(value)) +
  geom_histogram(bins = 50, fill = unname(pal["blue"]), colour = "white",
                 linewidth = 0.1) +
  facet_wrap(~stage, scales = "free") +
  labs(title = "total_rooms: from skewed counts to a symmetric, standardised scale",
       x = NULL, y = "Block groups")
save_fig("fig06_transform_stages.png", fig_scale, 7.5, 3.2)

# ---- encoding ----
# One-hot (dummy) encoding with INLAND as reference level
dummies <- model.matrix(~ ocean_proximity, data = housing_clean)[, -1]
clean_names <- gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_",
                    levels(housing_clean$ocean_proximity)[-1]))
colnames(dummies) <- paste0("ocean_", clean_names)
head(dummies)
cat("\nColumn totals (should match the category counts):\n")
colSums(dummies)

# Why not label encoding? It invents an order between unordered categories.
head(data.frame(category = housing_clean$ocean_proximity,
                label_code = as.integer(housing_clean$ocean_proximity)), 3)

housing_final <- cbind(housing_clean, dummies)
cat("\nFinal dimensions:", dim(housing_final), "\n")

# ---- eda_structure ----
str(housing_final[, c("housing_median_age", "total_rooms", "median_income",
                      "median_house_value", "ocean_proximity", "value_capped",
                      "bedrooms_imputed", "ocean_NEAR_BAY")])

# ---- eda_summary ----
summary(housing_final[, c("housing_median_age", "total_rooms", "total_bedrooms",
                          "population", "households", "median_income",
                          "median_house_value")])

# ---- eda_descriptives ----
describe <- function(x) c(mean = mean(x), sd = sd(x), min = min(x),
                          median = median(x), max = max(x),
                          skew = skew(x), cv = sd(x) / mean(x))
desc <- round(t(sapply(housing_final[c(num_vars)], describe)), 2)
desc

# ---- eda_univariate ----
fig_hist <- housing_final %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = num_vars)) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 40, fill = unname(pal["blue"]), colour = "white",
                 linewidth = 0.1) +
  facet_wrap(~name, scales = "free", ncol = 4) +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Distributions of the numeric variables after cleaning",
       x = NULL, y = "Block groups") +
  theme(strip.text = element_text(size = 7.5), axis.text = element_text(size = 7))
save_fig("fig07_histograms.png", fig_hist, 7.5, 5.6)

cat_tbl <- housing_final %>%
  count(ocean_proximity) %>%
  mutate(pct = round(100 * n / sum(n), 1))
cat_tbl

fig_cat <- ggplot(cat_tbl, aes(n, reorder(ocean_proximity, n))) +
  geom_col(fill = unname(pal["blue"]), width = 0.6) +
  geom_text(aes(label = paste0(comma(n), "  (", pct, "%)")), hjust = -0.1,
            size = 3.2, colour = ink) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)), labels = comma) +
  labs(title = "Most block groups are within an hour of the ocean, or inland",
       x = "Block groups", y = NULL)
save_fig("fig08_ocean_proximity.png", fig_cat, 7, 3)

# ---- eda_bivariate ----
fig_inc <- ggplot(housing_final, aes(median_income, median_house_value)) +
  geom_point(colour = unname(pal["blue"]), alpha = 0.12, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, colour = unname(pal["orange"]),
              se = FALSE, linewidth = 1) +
  annotate("text", x = 11.6, y = 600000, label = "Linear fit", hjust = 0.5,
           colour = ink, size = 3.4) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(title = "Richer neighbourhoods have more expensive homes",
       subtitle = "The flat band at $500,001 is the census price cap",
       x = "Median income (tens of thousands of US$)",
       y = "Median house value")
save_fig("fig09_income_vs_value.png", fig_inc, 7, 4.2)

by_ocean <- housing_final %>%
  group_by(ocean_proximity) %>%
  summarise(n = n(),
            median_value  = median(median_house_value),
            mean_value    = round(mean(median_house_value)),
            median_income = round(median(median_income), 2),
            pct_capped    = round(100 * mean(value_capped), 1)) %>%
  arrange(desc(median_value))
as.data.frame(by_ocean)

fig_box_ocean <- ggplot(housing_final,
    aes(reorder(ocean_proximity, median_house_value, median), median_house_value)) +
  geom_boxplot(fill = "#cde2fb", colour = unname(pal["blue"]), outlier.size = 0.5,
               outlier.alpha = 0.3, linewidth = 0.5) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(title = "House values differ sharply by proximity to the ocean",
       x = NULL, y = "Median house value")
save_fig("fig10_value_by_ocean.png", fig_box_ocean, 7, 4)

# ---- eda_geo ----
fig_geo <- housing_final %>%
  arrange(median_house_value) %>%
  ggplot(aes(longitude, latitude, colour = median_house_value)) +
  geom_point(size = 0.6, alpha = 0.85) +
  scale_colour_gradientn(colours = c("#cde2fb", "#6da7ec", "#2a78d6", "#184f95", "#0d366b"),
                         labels = label_dollar(scale_cut = cut_short_scale()),
                         name = "Median house value") +
  coord_quickmap() +
  labs(title = "Expensive block groups hug the coast and the big metros",
       x = "Longitude", y = "Latitude") +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  theme(legend.key.width = unit(0.6, "in"))
save_fig("fig11_geo_values.png", fig_geo, 6.4, 5.4)

# ---- eda_corr ----
cor_vars <- c("longitude", "latitude", "housing_median_age", "total_rooms",
              "total_bedrooms", "population", "households", "median_income",
              "rooms_per_household", "bedrooms_per_room",
              "population_per_household", "median_house_value")
short <- c("lon", "lat", "age", "rooms", "beds", "pop", "hh", "income",
           "rm/hh", "bd/rm", "pop/hh", "value")

cor_p <- cor(housing_final[cor_vars]);                      dimnames(cor_p) <- list(short, short)
cor_s <- cor(housing_final[cor_vars], method = "spearman"); dimnames(cor_s) <- list(short, short)
round(cor_p, 2)

# Correlation with the target, Pearson vs Spearman
tgt <- data.frame(pearson  = cor_p[, "value"], spearman = cor_s[, "value"])
tgt <- tgt[rownames(tgt) != "value", ]
round(tgt[order(-abs(tgt$pearson)), ], 3)

# Strongly correlated predictor pairs (|r| > 0.8)
hi <- which(abs(cor_p) > 0.8 & upper.tri(cor_p), arr.ind = TRUE)
data.frame(var1 = rownames(cor_p)[hi[, 1]], var2 = colnames(cor_p)[hi[, 2]],
           r = round(cor_p[hi], 2))

# Variance inflation factors for the predictors (VIF > 10 = serious overlap)
preds <- setdiff(cor_vars, "median_house_value")
vif <- sapply(preds, function(v) {
  r2 <- summary(lm(reformulate(setdiff(preds, v), v),
                   data = housing_final))$r.squared
  1 / (1 - r2)
})
round(sort(vif, decreasing = TRUE), 1)

cor_long <- as.data.frame(as.table(cor_p)) %>%
  setNames(c("x", "y", "r")) %>%
  mutate(x = factor(x, levels = short), y = factor(y, levels = rev(short)))
fig_cor <- ggplot(cor_long, aes(x, y, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", r),
                colour = abs(r) > 0.6), size = 2.7) +
  scale_colour_manual(values = c("FALSE" = ink, "TRUE" = "white"), guide = "none") +
  scale_fill_gradient2(low = "#2a78d6", mid = "#f0efec", high = "#e34948",
                       midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
  labs(title = "Correlation matrix of the numeric variables", x = NULL, y = NULL) +
  theme(panel.grid = element_blank(), legend.key.width = unit(0.5, "in"))
save_fig("fig12_correlation_heatmap.png", fig_cor, 7, 5.8)

# ---- eda_multicollinearity ----
fig_mc <- ggplot(housing_final, aes(total_rooms, total_bedrooms)) +
  geom_point(colour = unname(pal["blue"]), alpha = 0.12, size = 0.8) +
  labs(title = "total_rooms and total_bedrooms move almost in lock-step",
       subtitle = sprintf("Pearson r = %.2f (values winsorised at the 1st/99th percentile)",
                          cor(housing_final$total_rooms, housing_final$total_bedrooms)),
       x = "Total rooms", y = "Total bedrooms")
save_fig("fig13_rooms_vs_bedrooms.png", fig_mc, 6, 3.8)

# ---- tests ----
# Income vs house value
cor.test(housing_final$median_income, housing_final$median_house_value)

# Does the price cap distort the relationship?
uncapped <- !housing_final$value_capped
cat("\nr (all rows)        :",
    round(cor(housing_final$median_income, housing_final$median_house_value), 3), "\n")
cat("r (cap rows removed):",
    round(cor(housing_final$median_income[uncapped],
              housing_final$median_house_value[uncapped]), 3), "\n\n")

# House value across ocean-proximity groups
kruskal.test(median_house_value ~ ocean_proximity, data = housing_final)
aov_tbl <- summary(aov(median_house_value ~ ocean_proximity, data = housing_final))[[1]]
cat("\nEta-squared (share of variance explained by ocean_proximity):",
    round(aov_tbl[1, "Sum Sq"] / sum(aov_tbl[, "Sum Sq"]), 3), "\n")

# ---- save_output ----
# Cleaned data in original units, and a model-ready scaled version
housing_model <- cbind(zs, dummies,
                       median_house_value = housing_clean$median_house_value)
write.csv(housing_final, "data/housing_clean.csv", row.names = FALSE)
write.csv(housing_model, "data/housing_scaled.csv", row.names = FALSE)
cat("housing_clean.csv :", nrow(housing_final), "rows,", ncol(housing_final), "columns\n")
cat("housing_scaled.csv:", nrow(housing_model), "rows,", ncol(housing_model), "columns\n")

# ---- session ----
sessionInfo()
