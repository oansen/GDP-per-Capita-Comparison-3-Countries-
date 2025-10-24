setwd("~/Desktop/R Directory/ASG 4")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1) Read File
path <- "Index Data.xlsx"
raw  <- read_excel(path)

# 2) Detect columns
name_col <- intersect(names(raw), c("Country","country","Nation","Name","name"))
if (length(name_col) == 0) name_col <- names(raw)[1]

year_cols <- names(raw)[grepl("^\\d{4}$", names(raw))]
if (length(year_cols) == 0) {
  stop("No year columns detected. Please ensure your year headers are 4-digit (e.g., 2005, 2006, ...).")
}

# 3) Long format: country, year, index
df <- raw %>%
  select(all_of(c(name_col, year_cols))) %>%
  rename(country = all_of(name_col)) %>%
  pivot_longer(cols = all_of(year_cols),
               names_to = "year",
               values_to = "index") %>%
  mutate(
    year  = as.integer(year),
    index = as.numeric(index)
  ) %>%
  filter(!is.na(index)) %>%
  filter(country %in% c("Germany","France","Denmark","DEU","FRA","DNK"))

# --- 4) Axes helpers ---
yr_min <- min(df$year)
yr_max <- max(df$year)

# ticks every 5 years plus endpoints
breaks5 <- seq(ceiling(yr_min/5)*5, floor(yr_max/5)*5, by = 5)
breaks5 <- sort(unique(c(yr_min, breaks5, yr_max)))

# 5)(centered title/subtitle, left caption) ---
p <- ggplot(df, aes(x = year, y = index, color = country)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4) +
  scale_x_continuous(breaks = breaks5) +
  scale_y_continuous(labels = number_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.10))) +
  labs(
    title    = "GDP per capita, PPP (constant 2021 international $)",
    subtitle = paste0("(Indexed, 2005 = 100) Years: ", yr_min, "–", yr_max),
    x = "Year", y = "Index (2005 = 100)",
    caption  = "Source: World Bank Data. \nNumber of Observations: 3."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption  = element_text(hjust = 0),
    plot.margin   = margin(10, 14, 12, 14)
  )

# 6) Endpoints & label positioning ---
# Vertical offset for labels (~3% of y-range). Use -dy to place below if you prefer.
dy <- diff(range(df$index, na.rm = TRUE)) * 0.03
if (!is.finite(dy) || dy == 0) dy <- 1

endpoints <- df %>%
  group_by(country) %>%
  filter(year %in% c(yr_min, yr_max)) %>%
  ungroup() %>%
  mutate(
    label     = sprintf("%.1f", index),
    x_pos     = if_else(year == yr_min, year + 0.25, year - 0.25),  # nudge inside panel
    hjust_val = if_else(year == yr_min, 0, 1)
  )

# Keep points for both ends, but LABEL & CONNECT only the LAST year
end_last  <- endpoints %>% filter(year == yr_max)

final_plot <- p +
  geom_point(data = endpoints, aes(x = year, y = index), size = 2) +
  geom_segment(
    data = end_last,
    aes(x = year, xend = x_pos, y = index, yend = index, color = country),
    linewidth = 0.3, alpha = 0.85
  ) +
  geom_text(
    data = end_last,
    aes(x = x_pos, y = index + dy, label = label, color = country),
    hjust = end_last$hjust_val,
    vjust = +1.1,
    size = 3.3,
    show.legend = FALSE
  )

print(final_plot)

