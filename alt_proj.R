library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)

# ------------------------------------------------------------
# 0. Preparation
# ------------------------------------------------------------

# Import data of party ratings from various polling companies
polls <- read_delim("C:/Users/kinde/Desktop/polls.csv")

# Selected parties used in the analysis
selected_parties <- c(
  "LSDP", "NA", "LVZS", "LLRA-KSS", "TS-LKD", "LRLS",
  "DSVL", "LP", "DP"
)

# Main pollsters used in some parts of the analysis
main_pollsters <- c("Vilmorus", "Spinter", "Baltijos tyrimai")

# Transform data into long format, rename parties, filter selected parties,
# remove missing/zero values, convert dates, and convert support to percentages
polls_long <- polls %>%
  pivot_longer(
    cols = -c(pollster, start_date, end_date),
    names_to = "party",
    values_to = "support"
  ) %>%
  mutate(
    party = recode(
      party,
      "NA." = "NA",
      "VL" = "DSVL"
    ),
    end_date = as.Date(end_date),
    support = support * 100
  ) %>%
  filter(
    party %in% selected_parties,
    !is.na(support),
    support > 0
  )

# Party colors used in plots
party_colors <- c(
  "TS-LKD" = "darkcyan",
  "LSDP" = "red",
  "LVZS" = "chartreuse3",
  "NA" = "brown",
  "LRLS" = "orange",
  "DSVL" = "navyblue",
  "LP" = "pink",
  "DP" = "lightblue",
  "LLRA-KSS" = "maroon"
)

# ------------------------------------------------------------
# 1. Plot ratings over time for all selected parties
# ------------------------------------------------------------

ggplot(polls_long, aes(x = end_date, y = support, color = party)) +
  geom_point(size = 0.55, alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1, span = 0.3) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "black") +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Lithuanian Party Ratings Over Time",
    x = "Date",
    y = "Support (%)",
    color = "Party"
  ) +
  theme_bw()

# ------------------------------------------------------------
# 2. Create election-period variable
# ------------------------------------------------------------

polls_long <- polls_long %>%
  mutate(
    period = case_when(
      end_date >= as.Date("2012-10-14") & end_date < as.Date("2016-10-09") ~ "2012 – 2016",
      end_date >= as.Date("2016-10-09") & end_date < as.Date("2020-10-11") ~ "2016 – 2020",
      end_date >= as.Date("2020-10-11") & end_date < as.Date("2024-10-13") ~ "2020 – 2024",
      end_date >= as.Date("2024-10-13") ~ "2024 – ..."
    )
  ) %>%
  filter(!is.na(period)) %>%
  mutate(
    period = factor(
      period,
      levels = c("2012 – 2016", "2016 – 2020", "2020 – 2024", "2024 – ...")
    )
  )

# ------------------------------------------------------------
# 3. Plot ratings by Seimas period
# ------------------------------------------------------------

ggplot(polls_long, aes(x = end_date, y = support, color = party)) +
  geom_point(size = 0.55, alpha = 0.5) +
  geom_smooth(se = FALSE, linewidth = 1, span = 0.3) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "black") +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Lithuanian Party Ratings by Seimas Period",
    x = "Date",
    y = "Support (%)",
    color = "Party"
  ) +
  theme_bw() +
  facet_wrap(~ period, scales = "free_x", nrow = 1)

# ------------------------------------------------------------
# 4. Summary statistics
# ------------------------------------------------------------

summary_stats <- polls_long %>%
  group_by(party) %>%
  summarise(
    mean = mean(support, na.rm = TRUE),
    sd = sd(support, na.rm = TRUE),
    min = min(support, na.rm = TRUE),
    max = max(support, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

summary_stats

# ------------------------------------------------------------
# 5. Volatility analysis
# ------------------------------------------------------------

volatility_stats <- polls_long %>%
  group_by(party) %>%
  summarise(
    mean_support = mean(support, na.rm = TRUE),
    sd_support = sd(support, na.rm = TRUE),
    cv = sd_support / mean_support,
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(cv))

volatility_stats

# ------------------------------------------------------------
# 6. Pollster comparison
# ------------------------------------------------------------

pollster_stats <- polls_long %>%
  filter(pollster %in% main_pollsters) %>%
  group_by(pollster, party) %>%
  summarise(
    mean_support = mean(support, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(pollster_stats, aes(x = pollster, y = mean_support, fill = pollster)) +
  geom_col() +
  facet_wrap(~ party, scales = "free_y") +
  labs(
    title = "Average Party Support by Pollster",
    x = "Pollster",
    y = "Average support (%)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# ------------------------------------------------------------
# 7. Correlation between party ratings
# ------------------------------------------------------------

party_wide <- polls_long %>%
  group_by(end_date, party) %>%
  summarise(
    support = mean(support, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = party,
    values_from = support
  )

cor_matrix <- party_wide %>%
  select(-end_date) %>%
  cor(use = "pairwise.complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 90,
  number.cex = 0.8
)

# ------------------------------------------------------------
# 8. Simple prediction model for party ratings
# ------------------------------------------------------------

# Prediction based on all pollsters, use this:
prediction_data <- polls_long

polls_prediction <- prediction_data %>%
  arrange(party, pollster, end_date) %>%
  group_by(party, pollster) %>%
  mutate(
    lag_support = lag(support),
    days_since_last_poll = as.numeric(end_date - lag(end_date))
  ) %>%
  ungroup() %>%
  filter(
    !is.na(lag_support),
    !is.na(days_since_last_poll)
  )

# Training data: before 2024 parliamentary election
train <- polls_prediction %>%
  filter(end_date < as.Date("2024-10-13"))

# Testing data: from election day onward
test <- polls_prediction %>%
  filter(end_date >= as.Date("2024-10-13"))

model <- lm(
  support ~ lag_support + days_since_last_poll + party + pollster,
  data = train
)

test <- test %>%
  mutate(
    predicted = predict(model, newdata = test),
    error = support - predicted,
    abs_error = abs(error)
  )

# RMSE: punishes larger mistakes more strongly
rmse <- sqrt(
  mean((test$support - test$predicted)^2, na.rm = TRUE)
)

# MAE: average absolute prediction error
mae <- mean(
  abs(test$support - test$predicted),
  na.rm = TRUE
)

rmse
mae

# ------------------------------------------------------------
# 9. Table of actual vs predicted values
# ------------------------------------------------------------

test_results <- test %>%
  select(end_date, pollster, party, support, predicted, error, abs_error) %>%
  arrange(desc(abs_error))

View(test_results)

# ------------------------------------------------------------
# 10. Plot actual vs predicted ratings
# ------------------------------------------------------------

test_long <- test %>%
  select(end_date, party, pollster, support, predicted) %>%
  pivot_longer(
    cols = c(support, predicted),
    names_to = "type",
    values_to = "rating"
  ) %>%
  mutate(
    type = recode(
      type,
      "support" = "Actual",
      "predicted" = "Predicted"
    )
  )

ggplot(
  test_long,
  aes(
    x = end_date,
    y = rating,
    color = type,
    group = interaction(type, party, pollster)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ party, scales = "free_y") +
  labs(
    title = "Actual vs Predicted Party Ratings",
    subtitle = paste0("MAE = ", round(mae, 2), ", RMSE = ", round(rmse, 2)),
    x = "Date",
    y = "Support (%)",
    color = ""
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 11. Plot average absolute prediction error by party
# ------------------------------------------------------------

party_errors <- test %>%
  group_by(party) %>%
  summarise(
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_error))

ggplot(party_errors, aes(x = reorder(party, mean_abs_error), y = mean_abs_error)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Absolute Prediction Error by Party",
    x = "Party",
    y = "Mean absolute error"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 12. Plot distribution of prediction errors
# ------------------------------------------------------------

ggplot(test, aes(x = error)) +
  geom_histogram(bins = 20) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Prediction Errors",
    x = "Prediction error",
    y = "Count"
  ) +
  theme_minimal()
