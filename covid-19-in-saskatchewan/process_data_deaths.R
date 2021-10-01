# Process COVID-19 mortality data for Saskatchewan

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
sk_old <- read.csv("raw/cases_2020-09-13.csv", stringsAsFactors = FALSE)
sk_new <- read.csv("raw/cases_2021-07-24.csv", stringsAsFactors = FALSE)

# process last Saskatchewan data file with old boundaries (6 zones) (2020-09-13)

## process data file
sk_old <- sk_old %>%
  select(Date, Region, Deaths) %>%
  mutate(Date = as.Date(Date, "%Y/%m/%d")) %>%
  rename(date = Date, region = Region, cumulative_deaths = Deaths) %>%
  arrange(date, region) %>%
  group_by(region) %>%
  fill(cumulative_deaths, .direction = "down") %>%
  replace_na(list(cumulative_deaths = 0)) %>%
  mutate(deaths = c(first(cumulative_deaths), diff(cumulative_deaths))) %>%
  ungroup() %>%
  filter(region != "Total") %>%
  filter(date < as.Date("2020-08-04"))

## write dataset
write.csv(sk_old, "data/sk-death-timeseries-old.csv", row.names = FALSE)

# process recent Saskatchewan data file with new boundaries (13 zones) (2021-07-24)

## process data file
sk_new <- sk_new %>%
  select(Date, Region, Deaths) %>%
  mutate(Date = as.Date(Date, "%Y/%m/%d")) %>%
  rename(date = Date, region = Region, cumulative_deaths = Deaths) %>%
  arrange(date, region) %>%
  group_by(region, date) %>%
  summarize(cumulative_deaths = sum(cumulative_deaths, na.rm = TRUE), .groups = "drop_last") %>%
  fill(cumulative_deaths, .direction = "down") %>%
  replace_na(list(cumulative_deaths = 0)) %>%
  ungroup() %>%
  filter(region != "Total")

## write dataset
write.csv(sk_new, "data/sk-death-timeseries-new.csv", row.names = FALSE)

# convert old dataset to use new boundaries
sk_old_converted <- sk_old %>%
  select(-deaths) %>%
  filter(cumulative_deaths > 0) %>%
  mutate(
    region = case_when(
      region == "Far North" ~ "Far North West",
      region == "Saskatoon" ~ "Saskatoon",
      region == "Central (excluding Saskatoon)" ~ "Central East",
      region == "Regina" ~ "Regina",
      region == "South (excluding Regina)" ~ "South Central",
      TRUE ~ region
    )
  )

# "North" deaths have to be assigned (arbitrarily) to either "North West" or "North Central".
# We will assign the final "North" case to "North Central" and the rest to "North West".
# Note that first death in SK was really at the end of March, not beginning of April,
# but the CSV was not updated every day at that point.
# https://www.cbc.ca/news/canada/saskatchewan/covid-sask-deaths-1.5530862
# This may be corrected manually in the future.
sk_old_converted[sk_old_converted$region == "North" &
                   sk_old_converted$date != as.Date("2020-07-23"), "region"] <- "North West"
sk_old_converted[sk_old_converted$region == "North West" &
                   sk_old_converted$date >= as.Date("2020-07-23"), "cumulative_deaths"] <- 4
north_central <- sk_old_converted[sk_old_converted$region == "North", ] %>%
  mutate(
    region = "North Central",
    cumulative_deaths = ifelse(
      .data$date >= as.Date("2020-07-23"), 1, 0))
sk_old_converted <- rbind(sk_old_converted, north_central)

# finish processing
sk_old_converted <- sk_old_converted %>%
  arrange(date, region)

# combine datasets
date_seq <- seq.Date(from = min(sk_old_converted$date), to = max(sk_new$date), by = "day")
boundaries <- unique(sk_new$region)
sk_combined <- bind_rows(
  sk_old_converted,
  sk_new
) %>%
  right_join(
    data.frame(
      date = rep(date_seq, each = length(boundaries)),
      region = rep(boundaries, times = length(date_seq))
    ),
    by = c("date", "region")
  ) %>%
  arrange(date, region) %>%
  group_by(region) %>%
  fill(cumulative_deaths, .direction = "down") %>%
  replace_na(list(cumulative_deaths = 0)) %>%
  mutate(deaths = c(first(cumulative_deaths), diff(cumulative_deaths))) %>%
  ungroup()

# write data
write.csv(sk_combined, "data/sk-death-timeseries-complete.csv", row.names = FALSE)
