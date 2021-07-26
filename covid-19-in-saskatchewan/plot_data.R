# Plot COVID-19 case data for Saskatchewan

# load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(gganimate)
library(transformr) # required for transition_reveal()
library(gifski) # required to save GIFs

# load data
sk_old <- read.csv("data/sk-case-timeseries-old.csv") %>%
  mutate(date = as.Date(date), boundaries = factor(
    "6 zones (old)", levels = c("6 zones (old)", "13 zones (new)")))
sk_new <- read.csv("data/sk-case-timeseries-new.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date), boundaries = factor(
    "13 zones (new)", levels = c("6 zones (old)", "13 zones (new)")))
sk_combined <- read.csv("data/sk-case-timeseries-complete.csv") %>%
  mutate(date = as.Date(date))

# old data versus new data

## build dataset (old and new boundaries)
sk_old_new_1 <- bind_rows(
  sk_old %>% filter(date >= as.Date("2020-07-21") & date <= as.Date("2020-08-04")),
  sk_new %>% filter(date >= as.Date("2020-08-04") & date <= as.Date("2020-08-18"))
) %>%
  mutate(
    region_boundaries = paste(region, boundaries),
    vline = ifelse(date >= as.Date("2020-08-04"), as.Date("2020-08-04"), NA)
  )

## plot and save dataset (old and new boundaries)
anim_sk_old_new_1 <- ggplot(data = sk_old_new_1,
                            aes(x = date, y = cumulative_cases,
                                colour = boundaries, group = region_boundaries)) +
  geom_line() +
  geom_vline(aes(xintercept = vline)) +
  labs(x = "Date", y = "Cumulative reported COVID-19 cases", colour = "Boundaries") +
  theme_pubr() +
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  transition_reveal(date)
animate(anim_sk_old_new_1, renderer = gifski_renderer(), nframes = 100, fps = 10,
        units = "px", height = 750, width = 750)
anim_save("figures/sk-old-new-1.gif")

## build dataset (attempt at reconciling old and new boundaries)
sk_old_new_2 <- bind_rows(
  sk_old %>% filter(date >= as.Date("2020-07-21") & date <= as.Date("2020-08-04")),
  sk_new %>% filter(date >= as.Date("2020-08-04") & date <= as.Date("2020-08-18"))
  ) %>%
  mutate(
    region = case_when(
      region %in% c("Central (excluding Saskatoon)", "Central East", "Central West") ~ "Central",
      region %in% c("Far North", "Far North Central", "Far North East", "Far North West") ~ "Far North",
      region %in% c("North", "North Central", "North East", "North West") ~ "North",
      region %in% c("Not Assigned") ~ "Not Assigned",
      region %in% c("Regina") ~ "Regina",
      region %in% c("Saskatoon") ~ "Saskatoon",
      region %in% c("South (excluding Regina)", "South Central", "South East", "South West") ~ "South"
    )
  ) %>%
  group_by(date, region, boundaries) %>%
  summarize(cases = sum(cases), cumulative_cases = sum(cumulative_cases), .groups = "drop") %>%
  mutate(
    region_boundaries = paste(region, boundaries),
    vline = ifelse(date >= as.Date("2020-08-04"), as.Date("2020-08-04"), NA)
    )

## plot and save dataset (attempt at reconciling old and new boundaries)
anim_sk_old_new_2 <- ggplot(data = sk_old_new_2,
                          aes(x = date, y = cumulative_cases,
                              colour = region, group = region_boundaries)) +
  geom_line() +
  geom_vline(aes(xintercept = vline)) +
  labs(x = "Date", y = "Cumulative reported COVID-19 cases", colour = "Region") +
  theme_pubr() +
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
    transition_reveal(date)
animate(anim_sk_old_new_2, renderer = gifski_renderer(), nframes = 100, fps = 10,
        units = "px", height = 750, width = 750)
anim_save("figures/sk-old-new-2.gif")

# complete case time series using new boundaries (13 zones)

## build dataset
sk_combined_1 <- sk_combined %>%
  mutate(vline = ifelse(date >= as.Date("2020-08-04"), as.Date("2020-08-04"), NA))

## plot and save dataset (transition period)
anim_sk_complete_1 <- ggplot(data = sk_combined_1 %>%
                               filter(date >= as.Date("2020-07-21") & date <= as.Date("2020-08-18")),
                            aes(x = date, y = cumulative_cases,
                                colour = region, group = region)) +
  geom_line() +
  geom_vline(aes(xintercept = vline)) +
  labs(x = "Date", y = "Cumulative reported COVID-19 cases", colour = "Region") +
  theme_pubr() +
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  transition_reveal(date)
animate(anim_sk_complete_1, renderer = gifski_renderer(), nframes = 100, fps = 10,
        units = "px", height = 750, width = 750)
anim_save("figures/sk-complete-transition.gif")

## plot and save dataset (full timeline)
anim_sk_complete_2 <- ggplot(data = sk_combined_1,
                             aes(x = date, y = cumulative_cases,
                                 colour = region, group = region)) +
  geom_line() +
  geom_vline(aes(xintercept = vline)) +
  scale_y_continuous(label = comma) +
  labs(x = "Date", y = "Cumulative reported COVID-19 cases", colour = "Region") +
  theme_pubr() +
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  transition_reveal(date)
animate(anim_sk_complete_2, renderer = gifski_renderer(), nframes = 100, fps = 10,
        units = "px", height = 750, width = 750)
anim_save("figures/sk-complete-full.gif")
