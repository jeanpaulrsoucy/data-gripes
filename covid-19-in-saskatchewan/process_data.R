# Process COVID-19 case data for Saskatchewan

# load packages
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
sk_old <- read.csv("raw/cases_2020-09-13.csv", stringsAsFactors = FALSE)
sk_new <- read.csv("raw/cases_2021-07-24.csv", stringsAsFactors = FALSE)
sk_total_old <- read_json("raw/health-wellness-covid-19-cases_2020-09-13.json")
sk_total_new <- read_json("raw/health-wellness-covid-19-cases_2021-07-24.json")
foi <- read.csv("data/sk-foi-data.csv", stringsAsFactors = FALSE)

# process last Saskatchewan data file with new boundaries (province total) (2020-09-13)
sk_total_old <- sk_total_old$tabs[[1]]$chart$data[[1]]$data %>%
  unlist() %>%
  {data.frame(
    date = as.Date(as.POSIXct(.[seq(1, 360, 2)], origin = "1970-01-01", tz = "GMT")),
    cumulative_cases = .[seq(2, 360, 2)]
  )}

# process last Saskatchewan data file with old boundaries (6 zones) (2020-09-13)

## process data file
sk_old <- sk_old %>%
  select(Date, Region, New.Cases, Total.Cases) %>%
  mutate(Date = as.Date(Date, "%Y/%m/%d")) %>%
  rename(date = Date, region = Region, cases = New.Cases, cumulative_cases = Total.Cases) %>%
  replace_na(list(cases = 0, cumulative_cases = 0)) %>%
  filter(region != "Total")

## calculate "Not Assigned" region column (difference between SK total and region sums for each day)
sk_old <- {sk_total_old %>%
  pull(cumulative_cases) -
  sk_old %>%
    select(-region, -cases) %>%
    group_by(date) %>%
    summarize(cumulative_cases = sum(cumulative_cases), .groups = "drop") %>%
    pull(cumulative_cases)} %>%
  data.frame(
    date = unique(sk_old$date),
    region = "Not Assigned",
    cumulative_cases = .
  ) %>%
  {mutate(., cases = c(.[1, "cumulative_cases"], diff(.$cumulative_cases)))} %>%
  bind_rows(sk_old, .) %>%
  arrange(date, region)

## write dataset
write.csv(sk_old, "data/sk-case-timeseries-old.csv", row.names = FALSE)

# process recent Saskatchewan data file with new boundaries (province total) (2021-07-24)
sk_total_new <- sk_total_new$tabs[[1]]$chart$data[[1]]$data %>%
  unlist() %>%
  {data.frame(
    date = as.Date(as.POSIXct(.[seq(1, 709, 2)], origin = "1970-01-01", tz = "GMT")),
    cumulative_cases = .[seq(2, 710, 2)]
  )}

# process recent Saskatchewan data file with new boundaries (13 zones) (2021-07-24)

## process data file
sk_new <- sk_new %>%
  select(Date, Region, New.Cases, Total.Cases) %>%
  mutate(Date = as.Date(Date, "%Y/%m/%d")) %>%
  rename(date = Date, region = Region, cases = New.Cases, cumulative_cases = Total.Cases) %>%
  replace_na(list(cases = 0, cumulative_cases = 0)) %>%
  group_by(date, region) %>%
  summarize(cases = sum(cases), cumulative_cases = sum(cumulative_cases), .groups = "drop")

## calculate "Not Assigned" region column (difference between SK total and region sums for each day)
sk_new <- {sk_total_new %>%
    pull(cumulative_cases) -
    sk_new %>%
    select(-region, -cases) %>%
    group_by(date) %>%
    summarize(cumulative_cases = sum(cumulative_cases), .groups = "drop") %>%
    pull(cumulative_cases)} %>%
  data.frame(
    date = unique(sk_new$date),
    region = "Not Assigned",
    cumulative_cases = .
  ) %>%
  {mutate(., cases = c(.[1, "cumulative_cases"], diff(.$cumulative_cases)))} %>%
  bind_rows(sk_new, .) %>%
  arrange(date, region)

## write dataset
write.csv(sk_new, "data/sk-case-timeseries-new.csv", row.names = FALSE)

# process Freedom of Information data file
foi <- foi %>%
  pivot_longer(
    c(-Date),
    names_to = "region",
    values_to = "cases"
  ) %>%
  mutate(
    Date = as.Date(Date, "%Y-%m-%d"),
    region = gsub("\\.", " ", region),
  ) %>%
  group_by(region) %>%
  mutate(cumulative_cases = cumsum(cases)) %>%
  ungroup() %>%
  rename(date = Date) %>%
  filter(region != "Total") %>%
  arrange(date, region)

# combine data Freedom of Information dataset with new Saskatchewan zone dataset
# these datasets overlap (first ends 2020-08-31, whereas the latter begins 2020-08-04)
# we will opt to favour the new Saskatchewan zone dataset and use this starting 2020-08-04

## combine datasets
sk_combined <- bind_rows(
  foi %>% filter(date < as.Date("2020-08-04")),
  sk_new
)

## recalculate cases column (due to discontinuities in cumulative case numbers between the two datasets)
sk_combined <- sk_combined %>%
  group_by(region) %>%
  mutate(
    cases = cumulative_cases - lag(cumulative_cases),
    cases = ifelse(is.na(cases), cumulative_cases, cases))

## write dataset
write.csv(sk_combined, "data/sk-case-timeseries-complete.csv", row.names = FALSE)
