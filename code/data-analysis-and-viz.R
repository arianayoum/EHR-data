# Load packages
pacman::p_load(dplyr, lme4, afex, ez, ggplot2, tidyverse, RColorBrewer, wesanderson, janitor,
               ggrepel, stringr, forcats, formattable, ggpubr, effsize, robustHD, ggeffects)

# Set directory
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- file.path(path, "cleaned_data")
files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

# Read in csv as dfs 
for (file in files) {
  raw_name <- tools::file_path_sans_ext(basename(file))
  clean_name <- gsub("-", "_", raw_name)  
  clean_name <- make.names(clean_name) 
  # clean column names
  df <- read.csv(file, header = TRUE)
  df <- clean_names(df)  # Clean names INSIDE the loop
  # assign df
  assign(clean_name, df, envir = .GlobalEnv)
}

# View distributions - here, I'm also checking the range to see if values are within a normal amount (e.g. no negatives)
boxplot(can_net_expenses[sapply(can_net_expenses, is.numeric)],
        main = "Boxplot of Numeric Columns", las = 2)

boxplot(on_net_expenses[sapply(can_net_expenses, is.numeric)],
        main = "Boxplot of Numeric Columns", las = 2)

##################################################################
######################## KEY TRENDS ##############################
##################################################################
on_net_expenses$region <- "Ontario"
can_net_expenses$region <- "Canada"
all_expenses <- rbind(on_net_expenses, can_net_expenses)
all_expenses <- all_expenses %>%
  mutate(year_start = as.numeric(substr(year, 1, 4)))

ggplot(all_expenses, aes(x = year_start, y = total, color = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Net Hospital Spending: Ontario vs. Canada (2005–2022)",
    x = "Year",
    y = "Total Spending ($Millions CAD)",
    color = "Region",
    caption = 'Data: CIHI Ontario and Canada Net Expenses'
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  )

# calculating gap across time
all_expenses %>%
  filter(year_start %in% c(2005, 2022)) %>%
  group_by(year_start, region) %>%
  summarise(total = sum(total), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = total) %>%
  mutate(percent_canada_of_ontario = Canada / Ontario * 100)


##################################################################
###################### BENCHMARKING ##############################
##################################################################

# Are Ontario’s spending priorities aligned with national averages?

# Reshape
can_dist_long <- can_distribution_percent_change %>%
  pivot_longer(-year, names_to = "ServiceArea", values_to = "Canada")

on_dist_long <- on_distribution_percent_change %>%
  pivot_longer(-year, names_to = "ServiceArea", values_to = "Ontario")

# Merge
dist_merge <- left_join(on_dist_long, can_dist_long, by = c("year", "ServiceArea"))

# Calculate the difference 
dist_merge <- dist_merge %>%
  mutate(Difference = Ontario - Canada)

# Summary
summary_table <- dist_merge %>%
  group_by(ServiceArea) %>%
  summarise(
    Avg_Ontario = mean(Ontario, na.rm = TRUE),
    Avg_Canada = mean(Canada, na.rm = TRUE),
    Avg_Gap = mean(Difference, na.rm = TRUE),
    Gap_Trend = cor(as.numeric(gsub("[^0-9]", "", year)), Difference, use = "complete.obs")
  ) %>%
  arrange(desc(abs(Avg_Gap)))

print(summary_table)

# Need year as int for viz
dist_merge <- dist_merge %>%
  mutate(year_start = as.numeric(substr(year, 1, 4)))

# Wrap column names
dist_merge$ServiceArea <- str_wrap(dist_merge$ServiceArea, width = 20)

# Spending proportions over time - viz
ggplot(dist_merge %>% filter(ServiceArea != 'total'), aes(x = year_start)) +
  geom_line(aes(y = Ontario, color = "Ontario")) +
  geom_line(aes(y = Canada, color = "Canada")) +
  facet_wrap(~ServiceArea, scales = "free") +
  theme_bw() +
  labs(
    title = "Spending Distribution by Service Area",
    y = "Percent of Total Hospital Spending",
    x = "Year",
    color = "Region",
    caption = 'Data: CIHI Canada and Ontario Spending Distribution % Change'
  )

# Divergence heatmaps
ggplot(dist_merge, aes(x = year_start, y = fct_reorder(ServiceArea, Difference, .fun = mean, na.rm = TRUE))) +
  geom_tile(aes(fill = Difference)) +
  scale_fill_gradient2(
    low = "cyan4", mid = "white", high = "goldenrod2", midpoint = 0) +
  theme_bw() +
  labs(
    title = "Difference in Spending Priorities (Ontario - Canada)",
    x = "Year",
    y = "Service Area",
    fill = "Percentage Gap",
    caption = 'Data:  CIHI Canada and Ontario Spending Distribution % Change') +
  theme(axis.text.y = element_text(size = 9)  
  )

# Which service areas does Ontario prioritize more or less than the national average?

ggplot(summary_table, aes(x = fct_reorder(ServiceArea, Avg_Gap), y = Avg_Gap)) +
  geom_col(fill = ifelse(summary_table$Avg_Gap > 0, "goldenrod2", "cyan4")) +
  coord_flip() +
  labs(
    title = "Average Gap in Spending Priority: Ontario vs. Canada",
    x = "Service Area",
    y = "Ontario – Canada (% Points)",
    caption = "Positive = Ontario spends more proportionally than national average 
    \n Data:  CIHI Canada and Ontario Spending Distribution % Change"
  ) +
  theme_bw()

##################################################################
###################### COST HOTSPOTS #############################
##################################################################

# Which services are driving the largest increases in raw spending?
on_net_long <- on_net_expenses %>%
  pivot_longer(-year, names_to = "service_area", values_to = "spending_millions")

# Calculate total change per service
on_net_growth <- on_net_long %>%
  group_by(service_area) %>%
  summarise(growth = last(spending_millions) - first(spending_millions)) %>%
  arrange(desc(growth))

# Viz
ggplot(on_net_growth, aes(x = fct_reorder(service_area, growth), y = growth)) +
  geom_col(fill = "dodgerblue3") +
  coord_flip() +
  labs(
    title = "Growth in Hospital Spending by Service Area (2005–2023)",
    x = "Service Area",
    y = "Spending Growth (Millions of $)",
    caption = "Data: CIHI Ontario Net Expenses"
  ) +
  theme_bw()

## Percentage growth?
on_net_growth_pct <- on_net_long %>%
  group_by(service_area) %>%
  summarise(
    raw_growth = last(spending_millions) - first(spending_millions),
    pct_growth = (last(spending_millions) - first(spending_millions)) / first(spending_millions) * 100
  ) %>%
  arrange(desc(raw_growth))

# Are there any outliers or volatile sectors?
on_spending_long <- on_spending_percent_change %>%
  pivot_longer(-year, names_to = "service_area", values_to = "pct_change")

# Calculate SD as a measure of volatility
volatility_table <- on_spending_long %>%
  group_by(service_area) %>%
  summarise(
    sd_change = sd(pct_change, na.rm = TRUE),
    max_change = max(abs(pct_change), na.rm = TRUE),
    min_change = min(pct_change, na.rm = TRUE),
    avg_change = mean(pct_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(sd_change))

# Viz
ggplot(volatility_table, aes(x = fct_reorder(service_area, sd_change), y = sd_change)) +
  geom_col(fill = "mediumturquoise") +
  coord_flip() +
  labs(
    title = "Volatility in Service Areas by Year-over-Year Spending Change",
    x = "Service Area",
    y = "Standard Deviation of % Change",
    caption = "Data: CIHI Ontario Spending Percent Change"
  ) +
  theme_bw()

## Combined analysis of growth vs. volatility in Ontario (for fun)
combined_analysis <- on_net_growth_pct %>%
  select(service_area, pct_growth) %>%
  inner_join(volatility_table, by = "service_area")

# Quadrants for strategic recommendations
median_growth <- median(combined_analysis$pct_growth, na.rm = TRUE)
median_volatility <- median(combined_analysis$sd_change, na.rm = TRUE)

# Viz
ggplot(combined_analysis, aes(x = pct_growth, y = sd_change)) +
  geom_point(color = "goldenrod2", size = 3) +
  geom_vline(xintercept = median_growth, linetype = "dashed", color = "cyan4") +
  geom_hline(yintercept = median_volatility, linetype = "dashed", color = "cyan4") +
  geom_text_repel(aes(label = service_area), max.overlaps = 15) +
  labs(
    title = "Growth vs. Volatility with Strategic Quadrants",
    x = "Total % Growth (2005–2023)",
    y = "Spending Volatility (SD of YoY % Change)",
    caption = "Data: CIHI Ontario Spending Percent Change"
  ) +
  theme_bw()

##################################################################
###################### PRIORITIZATION ############################
##################################################################

# Are we spending more (proportionally) on some services while others are declining?
on_dist_annual_long <- on_distribution_annual_percent_change %>%
  pivot_longer(-year, names_to = "service_area", values_to = "annual_change")

# Look at trends
trend_table <- on_dist_annual_long %>%
  group_by(service_area) %>%
  summarise(
    total_change = sum(annual_change, na.rm = TRUE),   # rough proxy for long-term shift
    avg_annual_change = mean(annual_change, na.rm = TRUE),
    trend_slope = coef(lm(annual_change ~ as.numeric(substr(year, 1, 4))))[2],  # linear trend
    .groups = "drop"
  ) %>%
  arrange(desc(total_change))

ggplot(trend_table, aes(x = fct_reorder(service_area, total_change), y = total_change)) +
  geom_col(aes(fill = total_change > 0)) +
  scale_fill_manual(values = c("TRUE" = "goldenrod2", "FALSE" = "cyan4"), labels = c("Gaining Share", "Losing Share")) +
  coord_flip() +
  labs(
    title = "Long-Term Change in Proportional Spending by Service Area",
    x = "Service Area",
    y = "Cumulative % Change (2005–2023)",
    fill = NULL,
    caption = "Positive = Spending share increased over time"
  ) +
  theme_bw()
